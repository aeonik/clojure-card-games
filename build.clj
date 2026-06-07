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

(defn- sh! [& args]
  (println "$" (str/join " " args))
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
   "--exclude" ".DS_Store"
   "--exclude" "*.pdf"])

(defn- rsync! [& args]
  (apply sh! "rsync" (concat rsync-common args)))

(defn- ssh! [command]
  (sh! "ssh" (deploy-host) command))

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

(defn deploy-restart [{:keys [confirm]}]
  (when-not (= confirm "DROP_ROOMS")
    (throw (ex-info "Restart deploy drops active in-memory rooms; pass :confirm \"DROP_ROOMS\""
                    {:required-confirm "DROP_ROOMS"})))
  (println "WARNING: restarting karbosh.service drops active in-memory rooms.")
  (rsync! "deps.edn" "build.clj" "src" "build" "deploy" "karbosh" (app-dst))
  (ssh! (env "KARBOSH_RESTART_COMMAND" "systemctl --user restart karbosh.service"))
  (smoke nil))

(defn rollback [{:keys [release confirm]}]
  (when-not (and release (= confirm "ROLLBACK_KARBOSH"))
    (throw (ex-info "Rollback requires :release and :confirm \"ROLLBACK_KARBOSH\""
                    {:required-confirm "ROLLBACK_KARBOSH"})))
  (let [current (env "KARBOSH_CURRENT_LINK" "~/apps/karbosh/current")]
    (ssh! (str "ln -sfn ~/apps/karbosh/releases/" release " " current " && "
               (env "KARBOSH_RESTART_COMMAND" "systemctl --user restart karbosh.service")))
    (smoke nil)))
