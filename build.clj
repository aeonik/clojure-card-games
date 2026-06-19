(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b])
  (:import [java.time Instant ZoneOffset]
           [java.time.format DateTimeFormatter]))

(def target-dir "target")
(def static-package-dir (str target-dir "/karbosh-static"))
(def release-root (str target-dir "/releases"))

(defn- env [name default]
  (or (not-empty (System/getenv name)) default))

(defn- printable-arg [arg]
  (let [arg (str arg)]
    (if (re-find #"\s|['\"$`\\]" arg)
      (pr-str arg)
      arg)))

(defn- sh! [& args]
  (println "$" (str/join " " (map printable-arg args)))
  (let [{:keys [exit]} (b/process {:command-args (vec args)})]
    (when-not (zero? exit)
      (throw (ex-info "Command failed" {:args args :exit exit})))))

(defn- process-output [& args]
  (str/trim (:out (b/process {:command-args (vec args)
                              :out :capture}))))

(defn- deploy-host []
  (env "KARBOSH_DEPLOY_HOST" "dc3systems.com"))

(defn- app-dst []
  (str (deploy-host) ":" (env "KARBOSH_APP_DIR" "~/apps/clojure-card-games/")))

(defn- static-dst []
  (str (deploy-host) ":"
       (env "KARBOSH_STATIC_DIR"
            "/var/www/dc3systems.com/public_html/karbosh/")))

(defn- health-url []
  (env "KARBOSH_HEALTH_URL" "https://dc3systems.com/karbosh/api/health"))

(defn- ssh-port []
  (not-empty (System/getenv "KARBOSH_SSH_PORT")))

(defn- ssh-args []
  (cond-> ["ssh"]
    (ssh-port) (conj "-p" (ssh-port))))

(defn- rsync-command []
  (cond
    (not-empty (System/getenv "KARBOSH_RSYNC_RSH"))
    ["rsync" "-e" (System/getenv "KARBOSH_RSYNC_RSH")]

    (ssh-port)
    ["rsync" "-e" (str "ssh -p " (ssh-port))]

    :else
    ["rsync"]))

(def rsync-common
  ["-avz"
   "--human-readable"
   "--itemize-changes"
   "--delay-updates"
   "--exclude" "assets/js/out/"
   "--exclude" ".git/"
   "--exclude" ".cpcache/"
   "--exclude" ".clj-kondo/.cache/"
   "--exclude" "target/"
   "--exclude" "deploy/dev.env"
   "--exclude" ".DS_Store"
   "--exclude" "*.pdf"])

(defn- rsync! [& args]
  (apply sh! (concat (rsync-command) rsync-common args)))

(defn- rsync-dry-run! [& args]
  (apply sh! (concat (rsync-command) rsync-common ["--dry-run"] args)))

(defn- rsync-delete! [& args]
  (apply sh! (concat (rsync-command) rsync-common ["--delete"] args)))

(defn- rsync-delete-dry-run! [& args]
  (apply sh! (concat (rsync-command) rsync-common ["--delete" "--dry-run"] args)))

(defn- ssh! [command]
  (apply sh! (concat (ssh-args) [(deploy-host) command])))

(defn clean [_]
  (b/delete {:path target-dir}))

(defn test [_]
  (sh! "clojure" "-M:test"))

(defn cljs [_]
  (sh! "clojure" "-M:karbosh-cljs"))

(defn package-static [_]
  (b/delete {:path static-package-dir})
  (b/copy-dir {:src-dirs ["karbosh"]
               :target-dir static-package-dir})
  (b/delete {:path (str static-package-dir "/assets/js/out")})
  {:static-dir static-package-dir})

(defn- copy-file! [src dst]
  (io/make-parents dst)
  (io/copy (io/file src) (io/file dst)))

(defn- copy-dir-if-exists! [src dst]
  (when (.exists (io/file src))
    (b/copy-dir {:src-dirs [src]
                 :target-dir dst})))

(defn- release-id []
  (let [timestamp (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HHmmss'Z'")
                           (.atZone (Instant/now) ZoneOffset/UTC))
        sha (process-output "git" "rev-parse" "--short" "HEAD")]
    (str timestamp "-" sha)))

(defn release [_]
  (cljs nil)
  (let [id (release-id)
        release-dir (str release-root "/" id)]
    (b/delete {:path release-dir})
    (doseq [dir ["src" "build" "deploy" "karbosh" "resources"]]
      (copy-dir-if-exists! dir (str release-dir "/" dir)))
    (b/delete {:path (str release-dir "/deploy/dev.env")})
    (copy-file! "deps.edn" (str release-dir "/deps.edn"))
    (spit (io/file release-dir "VERSION")
          (str "release=" id "\n"
               "git-sha=" (process-output "git" "rev-parse" "HEAD") "\n"))
    {:release id
     :path release-dir}))

(defn reload [_]
  (ssh! (str "set -a; "
             "[ -f ~/.config/karbosh/karbosh.env ] && "
             ". ~/.config/karbosh/karbosh.env; "
             "set +a; "
             ": \"${KARBOSH_ADMIN_PASSWORD:?KARBOSH_ADMIN_PASSWORD missing}\"; "
             "curl -fsS -u "
             "\"${KARBOSH_ADMIN_USER:-admin}:$KARBOSH_ADMIN_PASSWORD\" "
             "--max-time 15 "
             "-X POST https://dc3systems.com/karbosh/admin/reload")))

(defn smoke [_]
  (sh! "curl" "-fsS" (health-url)))

(defn- print-edn [x]
  (println (pr-str x))
  x)

(defn compact-archive [opts]
  (let [compact! (requiring-resolve
                  'clojure-card-games.karbosh.archive/compact-archive!)]
    (print-edn (compact! opts))))

(defn storage-report [opts]
  (let [report (requiring-resolve
                'clojure-card-games.karbosh.storage-report/report)]
    (print-edn (report opts))))

(defn prod-storage-report [_]
  (ssh! (str "cd "
             (env "KARBOSH_APP_DIR" "~/apps/clojure-card-games/")
             " && /usr/local/bin/clojure -M -m "
             "clojure-card-games.karbosh.storage-report")))

(defn compact-prod-archive [{:keys [confirm]}]
  (when-not (= confirm "COMPACT_ARCHIVE")
    (throw (ex-info "Production archive compaction moves the legacy audit directory; pass :confirm \"COMPACT_ARCHIVE\""
                    {:required-confirm "COMPACT_ARCHIVE"})))
  (ssh! (str "cd "
             (env "KARBOSH_APP_DIR" "~/apps/clojure-card-games/")
             " && /usr/local/bin/clojure -M -m "
             "clojure-card-games.karbosh.archive "
             "--confirm COMPACT_ARCHIVE")))

(defn deploy-compatible [_]
  (rsync! "deps.edn" "build.clj" "src" "build" "deploy" "karbosh" (app-dst))
  (rsync! "karbosh/" (static-dst))
  (reload nil)
  (smoke nil))

(defn deploy-compatible-dry-run [_]
  (rsync-dry-run! "deps.edn" "build.clj" "src" "build" "deploy" "karbosh" (app-dst))
  (rsync-dry-run! "karbosh/" (static-dst))
  {:status :dry-run
   :mode :compatible})

(defn deploy-restart [{:keys [confirm]}]
  (when-not (= confirm "DROP_ROOMS")
    (throw (ex-info "Restart deploy drops active in-memory rooms; pass :confirm \"DROP_ROOMS\""
                    {:required-confirm "DROP_ROOMS"})))
  (println "WARNING: restarting karbosh.service drops active in-memory rooms.")
  (rsync! "deps.edn" "build.clj" (app-dst))
  (doseq [dir ["src" "build" "deploy" "karbosh"]]
    (rsync-delete! (str dir "/") (str (app-dst) dir "/")))
  (ssh! (env "KARBOSH_RESTART_COMMAND" "sudo systemctl restart karbosh.service"))
  (smoke nil))

(defn deploy-restart-dry-run [_]
  (println "DRY RUN: restart deploy would delete stale files inside src/build/deploy/karbosh and restart karbosh.service.")
  (rsync-dry-run! "deps.edn" "build.clj" (app-dst))
  (doseq [dir ["src" "build" "deploy" "karbosh"]]
    (rsync-delete-dry-run! (str dir "/") (str (app-dst) dir "/")))
  {:status :dry-run
   :mode :restart})

(defn rollback [{:keys [release confirm]}]
  (when-not (and release (= confirm "ROLLBACK_KARBOSH"))
    (throw (ex-info "Rollback requires :release and :confirm \"ROLLBACK_KARBOSH\""
                    {:required-confirm "ROLLBACK_KARBOSH"})))
  (let [current (env "KARBOSH_CURRENT_LINK" "~/apps/karbosh/current")]
    (ssh! (str "ln -sfn ~/apps/karbosh/releases/" release " " current " && "
               (env "KARBOSH_RESTART_COMMAND" "sudo systemctl restart karbosh.service")))
    (smoke nil)))
