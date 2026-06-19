(ns clojure-card-games.karbosh.client
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.hand-order :as hand-order]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [replicant.dom :as d]))

(def fast-mode-storage-key "karbosh-fast-mode")
(def room-history-storage-key "karbosh-room-history")
(def max-room-history 10)

(def normal-timings
  {:play-animation 1150
   :trick-popup 3400
   :bid-popup 1600
   :fireworks 5200
   :hand-animation 220})

(def fast-timings
  {:play-animation 260
   :trick-popup 700
   :bid-popup 420
   :fireworks 2600
   :hand-animation 90})

(def ultra-fast-timings
  {:play-animation 130
   :trick-popup 350
   :bid-popup 210
   :fireworks 1300
   :hand-animation 45})

(def speed-modes #{:normal :fast :ultra-fast})

(defn normalize-speed-mode [mode]
  (let [mode (cond
               (true? mode) :fast
               (false? mode) :normal
               (keyword? mode) mode
               (= "true" mode) :fast
               (= "false" mode) :normal
               (string? mode) (keyword mode)
               :else :normal)]
    (if (contains? speed-modes mode)
      mode
      :normal)))

(defn fast-speed? [mode]
  (not= :normal (normalize-speed-mode mode)))

(defn ultra-fast-speed? [mode]
  (= :ultra-fast (normalize-speed-mode mode)))

(defn next-speed-mode [mode]
  (case (normalize-speed-mode mode)
    :normal :fast
    :fast :ultra-fast
    :ultra-fast :normal))

(defn stored-speed-mode []
  (try
    (normalize-speed-mode (.getItem js/localStorage fast-mode-storage-key))
    (catch :default _
      :normal)))

(defn persist-speed-mode! [mode]
  (try
    (.setItem js/localStorage fast-mode-storage-key (name (normalize-speed-mode mode)))
    (catch :default _
      nil)))

(def initial-speed-mode (stored-speed-mode))

(defonce app
  (atom {:socket nil
         :connected? false
         :room-id nil
         :player nil
         :view nil
         :play-animation nil
         :trick-popup nil
         :queued-trick-popup nil
         :bid-popup nil
         :fireworks nil
         :seat-popover-player nil
         :seat-invite-copied-player nil
         :join-modal nil
         :public-rooms {:loading? false
                        :rooms []
                        :error nil}
         :hand-order nil
         :card-drag nil
         :hand-animating? false
         :suppress-card-click? false
         :pending-card nil
         :pending-auto? false
         :auto-play-latched? false
         :suppress-auto-click? false
         :speed-mode initial-speed-mode
         :fast-mode? (fast-speed? initial-speed-mode)
         :last-reconnect-at 0
         :error nil}))

(def reconnect-throttle-ms 1200)
(def auto-play-hold-threshold-ms 520)

(defonce ^:private auto-play-hold* (atom nil))

(declare maybe-run-latched-auto-play!)

(defn el [id]
  (.getElementById js/document id))

(defn qs [selector]
  (.querySelector js/document selector))

(defn closest [node selector]
  (let [node (if (and node (.-closest node))
               node
               (some-> node .-parentElement))]
    (when (and node (.-closest node))
      (.closest node selector))))

(defn html! [node content]
  (d/render node (if (= "" content) nil content)))

(defn text! [node content]
  (set! (.-textContent node) content))

(def perf?
  (boolean (re-find #"[?&]perf=1" (str (.-search js/location)))))

(defonce ^:private perf-stats
  (volatile! nil))

(def ^:private empty-perf-stats
  {:msgs 0 :msg-ms 0 :max-msg-ms 0
   :renders 0 :render-ms 0 :max-render-ms 0
   :max-delay 0})

(defn- perf-time! [count-key ms-key max-key f]
  (if perf?
    (let [start (js/performance.now)
          result (f)
          ms (- (js/performance.now) start)]
      (vswap! perf-stats
              #(-> (or % empty-perf-stats)
                   (update count-key inc)
                   (update ms-key + ms)
                   (update max-key max ms)))
      result)
    (f)))

(defn- perf-queue-delay! [event]
  (when perf?
    (let [delay (max 0 (- (js/performance.now) (.-timeStamp event)))]
      (vswap! perf-stats #(update (or % empty-perf-stats) :max-delay max delay)))))

(defn- perf-line [{:keys [msgs msg-ms max-msg-ms renders render-ms max-render-ms max-delay]}]
  (str "msg/s " msgs
       " avg " (.toFixed (if (pos? msgs) (/ msg-ms msgs) 0) 1)
       " max " (.toFixed max-msg-ms 1)
       " | render/s " renders
       " avg " (.toFixed (if (pos? renders) (/ render-ms renders) 0) 1)
       " max " (.toFixed max-render-ms 1)
       " | queue-delay max " (.toFixed max-delay 0) "ms"))

(defn- start-perf-overlay! []
  (let [node (.createElement js/document "div")]
    (set! (.-id node) "karbosh-perf-overlay")
    (set! (.-cssText (.-style node))
          (str "position:fixed;left:4px;bottom:4px;z-index:99999;"
               "background:rgba(0,0,0,.75);color:#9f9;font:11px/1.4 monospace;"
               "padding:4px 6px;border-radius:4px;pointer-events:none;white-space:pre"))
    (.appendChild (.-body js/document) node)
    (js/setInterval
     (fn []
       (set! (.-textContent node) (perf-line (or @perf-stats empty-perf-stats)))
       (vreset! perf-stats empty-perf-stats))
     1000)))

(defn active-game-layout! [{:keys [view speed-mode]}]
  (let [classes (.-classList (.-body js/document))]
    (.toggle classes "has-karbosh-game" (some? view))
    (.toggle classes "karbosh-fast-mode" (fast-speed? speed-mode))
    (.toggle classes "karbosh-ultra-fast-mode" (ultra-fast-speed? speed-mode))))

(defn public-rooms-visible? [{:keys [view join-modal]}]
  (and (nil? view)
       (nil? join-modal)))

(defn entered-public-rooms? [old new]
  (and (public-rooms-visible? new)
       (not (public-rooms-visible? old))))

(defn timing-ms [k]
  (get (case (normalize-speed-mode (:speed-mode @app))
         :ultra-fast ultra-fast-timings
         :fast fast-timings
         normal-timings)
       k))

(defn kw-name [x]
  (when x (name x)))

(defn card-label [card]
  (cards/card->str card))

(defn suit-class [suit]
  (case suit
    :♥ " is-red suit-heart"
    :♦ " is-red suit-diamond"
    :♠ " is-black suit-spade"
    :♣ " is-black suit-club"
    ""))

(defn card-suit-class [[_ suit]]
  (suit-class suit))

(defn visible-hand [hand pending-card]
  (hand-order/visible-hand hand pending-card))

(defn reconcile-hand-order [hand-order hand hand-index]
  (hand-order/reconcile hand-order hand hand-index))

(defn ordered-hand [view hand-order]
  (:cards (reconcile-hand-order hand-order (:hand view) (:hand-index view))))

(defn displayed-hand [view pending-card hand-order]
  (visible-hand (ordered-hand view hand-order) pending-card))

(defn index-of-card [cards card]
  (hand-order/index-of-card cards card))

(defn move-card-to [cards card index]
  (hand-order/move-card-to cards card index))

(defn player-class [player]
  (str "player-" (kw-name player)))

(defn team-class [team]
  (when team
    (str " team-" team)))

(defn phase-label [phase]
  (-> (kw-name phase)
      (str/replace "-" " ")
      (str/capitalize)))

(defn player-by-id [view player]
  (first (filter #(= player (:id %)) (:players view))))

(defn player-label [view player]
  (or (:name (player-by-id view player))
      (some-> player kw-name)
      "--"))

(defn team-label [team]
  (str "Team " team))

(defn bid-label [{:keys [bid-type value]}]
  (case bid-type
    :pass "Pass"
    :bid (str "Bid " value)
    :karbosh "Karbosh"
    :double-karbosh "Double"
    "--"))

(defn latest-bid [view player]
  (last (filter #(= player (:player %)) (:bids-this-hand view))))

(defn fit-number-attr [node attr fallback]
  (let [n (js/parseFloat (or (.getAttribute node attr) ""))]
    (if (js/isNaN n) fallback n)))

(defonce seat-name-measure-context
  (delay
    (let [canvas (.createElement js/document "canvas")]
      (.getContext canvas "2d"))))

(defn text-width [font text]
  (when-let [context @seat-name-measure-context]
    (set! (.-font context) font)
    (.-width (.measureText context text))))

(defn computed-font [computed]
  (let [font (.-font computed)]
    (if (str/blank? font)
      (str (.-fontStyle computed) " "
           (.-fontWeight computed) " "
           (.-fontSize computed) " "
           (.-fontFamily computed))
      font)))

(defonce ^:private seat-fit-signature (volatile! nil))

(defn- seat-name-nodes []
  (vec (array-seq (.querySelectorAll js/document ".seat-name"))))

(defn- unfitted-node? [node]
  (not (.hasAttribute node "data-fit-done")))

(defn fit-seat-names! [nodes]
  ;; Batched phases: write everything, then read everything, then write
  ;; everything — at most one forced reflow total instead of one per node.
  (doseq [node nodes]
    (set! (.-fontSize (.-style node)) ""))
  (let [measures (mapv (fn [node]
                         (let [computed (js/getComputedStyle node)]
                           {:node node
                            :max-size (js/parseFloat (.-fontSize computed))
                            :font (computed-font computed)
                            :available (.-clientWidth node)
                            :min-size (fit-number-attr node "data-fit-min" 4.5)
                            :text (.-textContent node)}))
                       nodes)]
    (doseq [{:keys [node max-size font available min-size text]} measures]
      (let [measured (text-width font text)]
        (when (and (pos? available) (pos? max-size) (pos? measured))
          (let [target (* max-size (/ (- available 1) measured))
                size (-> target
                         (min max-size)
                         (max min-size))]
            (set! (.-fontSize (.-style node)) (str size "px")))))
      (.setAttribute node "data-fit-done" "1"))))

(defn maybe-fit-seat-names!
  "Refit seat names only when their text, the viewport width, or the set of
  nodes has changed — the signature check reads no layout, so the common
  per-render case costs a querySelectorAll and string compares."
  []
  (let [nodes (seat-name-nodes)
        signature [(mapv #(.-textContent %) nodes) (.-innerWidth js/window)]]
    (when (or (not= signature @seat-fit-signature)
              (some unfitted-node? nodes))
      (vreset! seat-fit-signature signature)
      (fit-seat-names! nodes))))

(defn schedule-fit-seat-names! []
  (js/requestAnimationFrame
   (fn [_]
     (maybe-fit-seat-names!))))

(defn bot-player-persona [view player]
  (:persona (player-by-id view player)))

(defn player-initials [label]
  (let [words (remove str/blank? (str/split (or label "") #"\s+"))
        initials (apply str (take 2 (map #(subs % 0 1) words)))]
    (str/upper-case (if (str/blank? initials)
                      "P"
                      initials))))

(defn dealer? [view player]
  (= player (:dealer view)))

(defn dealer-chip-html []
  [:span {:class "dealer-chip"
          :title "Dealer"
          :aria-label "Dealer"}
   "D"])

(defn ws-url []
  (let [params (js/URLSearchParams. (.-search js/location))
        explicit (.get params "ws")
        protocol (if (= "https:" (.-protocol js/location)) "wss://" "ws://")]
    (or explicit (str protocol (.-host js/location) "/karbosh/ws"))))

(defn send! [message]
  (when-let [socket (:socket @app)]
    (when (= (.-readyState socket) js/WebSocket.OPEN)
      (.send socket (pr-str message)))))

(defn action! [event]
  (send! {:op :action :event event}))

(defn query-room-param []
  (let [params (js/URLSearchParams. (.-search js/location))
        room (.get params "room")]
    (when-not (str/blank? room)
      (str/trim room))))

(defn same-room-id? [a b]
  (= (some-> a str/upper-case)
     (some-> b str/upper-case)))

(defn stored-room-id []
  (let [room (.getItem js/localStorage "karbosh-room")]
    (when-not (str/blank? room)
      (str/trim room))))

(defn stored-player []
  (let [p (.getItem js/localStorage "karbosh-player")]
    (when-not (str/blank? p) (keyword p))))

(defn stored-player-name []
  (let [n (.getItem js/localStorage "karbosh-name")]
    (when-not (str/blank? n) n)))

(defn local-storage-read [k]
  (try
    (.getItem js/localStorage k)
    (catch :default _
      nil)))

(defn local-storage-write! [k value]
  (try
    (.setItem js/localStorage k value)
    (catch :default _
      nil)))

(defn local-storage-remove! [k]
  (try
    (.removeItem js/localStorage k)
    (catch :default _
      nil)))

(defn normalize-room-history-entry [entry]
  (let [room-id (some-> (:room-id entry) str str/trim str/upper-case)
        player (:player entry)
        player (cond
                 (keyword? player) player
                 (str/blank? (str player)) nil
                 :else (keyword player))
        name (some-> (:name entry) str str/trim)
        updated-at (:updated-at entry)]
    (when-not (str/blank? room-id)
      (cond-> {:room-id room-id
               :updated-at (if (number? updated-at) updated-at 0)}
        player (assoc :player player)
        (not (str/blank? name)) (assoc :name name)))))

(defn room-history-from-storage []
  (try
    (let [raw (local-storage-read room-history-storage-key)
          value (when-not (str/blank? raw)
                  (reader/read-string raw))]
      (->> (if (sequential? value) value [])
           (keep normalize-room-history-entry)
           vec))
    (catch :default _
      [])))

(defn legacy-room-history-entry []
  (when-let [room-id (stored-room-id)]
    (normalize-room-history-entry
     {:room-id room-id
      :player (stored-player)
      :name (stored-player-name)
      :updated-at 0})))

(defn dedupe-room-history [entries]
  (->> entries
       (keep normalize-room-history-entry)
       (reduce (fn [by-room entry]
                 (let [k (:room-id entry)
                       existing (get by-room k)]
                   (if (>= (:updated-at entry)
                           (or (:updated-at existing) -1))
                     (assoc by-room k entry)
                     by-room)))
               {})
       vals
       (sort-by :updated-at >)
       (take max-room-history)
       vec))

(defn stored-room-history []
  (let [legacy (legacy-room-history-entry)]
    (dedupe-room-history
     (cond-> (room-history-from-storage)
       legacy (conj legacy)))))

(defn persist-room-history! [entries]
  (local-storage-write! room-history-storage-key
                        (pr-str (dedupe-room-history entries))))

(defn remember-room! [room-id player name]
  (let [entry {:room-id room-id
               :player player
               :name name
               :updated-at (.now js/Date)}]
    (persist-room-history! (cons entry (stored-room-history)))))

(declare render-recent-rooms!)

(defn forget-room! [room-id]
  (persist-room-history!
   (remove #(same-room-id? room-id (:room-id %)) (stored-room-history)))
  (when (same-room-id? room-id (stored-room-id))
    (local-storage-remove! "karbosh-room")
    (local-storage-remove! "karbosh-player"))
  (render-recent-rooms!))

(defn save-session! [room-id player name]
  (local-storage-write! "karbosh-room" room-id)
  (local-storage-write! "karbosh-player" (kw-name player))
  (local-storage-write! "karbosh-name" name)
  (remember-room! room-id player name))

(defn player-name []
  (let [input (el "player-name")
        value (str/trim (.-value input))]
    (if (str/blank? value) "Player" value)))

(defn create-public-room? []
  (boolean (some-> (el "create-public-room") .-checked)))

(defn room-page-url
  ([room-id]
   (room-page-url room-id nil))
  ([room-id player]
   (let [url (js/URL. (.-href js/location))]
     (.set (.-searchParams url) "room" room-id)
     (if player
       (.set (.-searchParams url) "player" (kw-name player))
       (.delete (.-searchParams url) "player"))
     (.-href url))))

(defn seat-invite-url [room-id player]
  (room-page-url room-id player))

(defn set-share-link! [room-id]
  (text! (el "share-link") (room-page-url room-id)))

(defn set-room-url! [room-id]
  (.replaceState js/history nil "" (room-page-url room-id)))

(defn clear-room-url! []
  (let [url (js/URL. (.-href js/location))]
    (.delete (.-searchParams url) "room")
    (.replaceState js/history nil "" (.-href url))))

(defn clear-session! []
  (local-storage-remove! "karbosh-room")
  (local-storage-remove! "karbosh-player"))

(defn render-status! [{:keys [connected? room-id player error]}]
  (text! (el "connection-status")
         (cond
           error error
           connected? "Connected"
           :else "Disconnected"))
  (text! (el "room-code") (or room-id "--"))
  (text! (el "seat-code") (or (some-> player name) "--")))

(def invalid-seed ::invalid-seed)

(defn set-error! [message]
  (swap! app assoc :error message))

(defn create-room-seed []
  (let [raw (some-> (el "create-room-seed") .-value str/trim)]
    (cond
      (str/blank? raw) nil
      (not (re-matches #"[+-]?\d+" raw)) invalid-seed
      :else (let [seed (js/Number raw)]
              (if (js/Number.isSafeInteger seed)
                seed
                invalid-seed)))))

(defn room-preview-url [room-id]
  (str "/karbosh/api/room/" (js/encodeURIComponent room-id)))

(defn public-rooms-url []
  "/karbosh/api/public-rooms")

(declare close-join-modal!
         join-from-modal!
         select-join-player!
         prepare-shared-room!
         load-public-rooms!
         load-public-rooms-if-needed!
         join-room!)

(defn joinable-seat-count [preview]
  (count (filter :joinable? (:players preview))))

(defn default-join-player [preview]
  (or (some (fn [{:keys [id joinable?]}]
              (when joinable? id))
            (:players preview))
      (some-> preview :players first :id)))

(defn join-modal-disabled? [loading? selected-player error]
  (or loading? error (nil? selected-player)))

(defn preview-seat-status [{:keys [open? bot? connected?]}]
  (cond
    open? "Open"
    bot? "Bot"
    connected? "Online"
    :else "Offline"))

(defn preview-seat-name [{:keys [id name open?]}]
  (if open?
    "Open seat"
    (or name (str "Seat " (last (kw-name id))))))

(defn preview-seat-html [selected-player {:keys [id team joinable?] :as player}]
  (let [selected? (= selected-player id)]
    [:button {:class (str "join-seat-option"
                          (when selected? " is-selected")
                          (when joinable? " is-joinable"))
              :type "button"
              :data-join-player (kw-name id)
              :on {:click (fn [_] (select-join-player! id))}}
     [:span "Seat " (last (kw-name id))]
     [:strong (preview-seat-name player)]
     [:em (str (team-label team) " / " (preview-seat-status player))]]))

(defn team-preview-html [selected-player team players]
  [:section {:class "join-team"}
   [:h3 (team-label team)]
   [:div {:class "join-seat-grid"}
    (for [player players]
      (preview-seat-html selected-player player))]])

(defn preview-players-html [selected-player preview]
  (let [team-groups (group-by :team (:players preview))
        available-count (joinable-seat-count preview)]
    [:div {:class "join-teams"}
     (for [team [1 2]]
       (team-preview-html selected-player team (get team-groups team)))
     [:p {:class "join-modal-count"}
      available-count " available "
      (if (= 1 available-count) "seat" "seats")]]))

(defn public-room-html [{:keys [room-id phase player-count connected-count available-count]}]
  [:article {:class "public-room-row"}
   [:div
    [:strong room-id]
    [:span (phase-label phase)]]
   [:em
    player-count " / 6 players"
    (when (pos? connected-count)
      (str " / " connected-count " online"))]
   [:span available-count " available"]
   [:button {:type "button"
             :data-public-room room-id}
    "Join"]])

(defn recent-room-time-label [updated-at]
  (if (and (number? updated-at) (pos? updated-at))
    (.toLocaleString (js/Date. updated-at))
    "Recent"))

(defn recent-room-seat-label [{:keys [player name]}]
  (cond
    (and player (not (str/blank? name)))
    (str name " / " (kw-name player))

    player
    (kw-name player)

    (not (str/blank? name))
    name

    :else
    "Choose a seat"))

(defn recent-room-html [{:keys [room-id player name updated-at] :as entry}]
  [:article {:class "recent-room-row"}
   [:div
    [:strong room-id]
    [:span (recent-room-seat-label entry)]]
   [:em (recent-room-time-label updated-at)]
   [:div {:class "recent-room-actions"}
    (when player
      [:button {:type "button"
                :data-recent-resume room-id
                :data-recent-player (kw-name player)
                :data-recent-name (or name "")}
       "Resume"])
    [:button {:type "button"
              :data-recent-choose room-id}
     "Choose Seat"]
    [:button {:type "button"
              :class "recent-room-forget"
              :data-recent-forget room-id}
     "Forget"]]])

(defn render-recent-rooms! []
  (when-let [root (el "recent-rooms-root")]
    (let [rooms (stored-room-history)]
      (html! root
             (if (seq rooms)
               [:div {:class "recent-room-list"}
                (for [room rooms]
                  (recent-room-html room))]
               [:p {:class "recent-rooms-empty"} "No recent rooms."])))))

(defn public-rooms-hiccup [{:keys [loading? rooms error]}]
  (cond
    error
    [:p {:class "public-rooms-empty"} error]

    (seq rooms)
    [:div {:class "public-room-list"}
     (for [room rooms] (public-room-html room))]

    loading?
    [:p {:class "public-rooms-empty"} "Loading rooms..."]

    :else
    [:p {:class "public-rooms-empty"} "No public rooms."]))

(defn resume-recent-room! [room-id player name]
  (when-not (str/blank? name)
    (set! (.-value (el "player-name")) name))
  (set! (.-value (el "join-room-id")) room-id)
  (join-room! room-id player))

(defn join-modal-hiccup [{:keys [room-id loading? preview player error]}]
  (when room-id
    [:div {:class "modal-backdrop"}
     [:section {:class "join-modal"
                :role "dialog"
                :aria-modal "true"
                :aria-labelledby "join-modal-title"}
      [:p {:class "eyebrow"} "Karbosh table"]
      [:h2 {:id "join-modal-title"} "Join room " room-id]
      (cond
        loading?
        [:p {:class "join-modal-muted"} "Loading players..."]

        error
        [:p {:class "join-modal-error"} error]

        :else
        [:div
         [:h3 "Current players"]
         (preview-players-html player preview)])
      [:label
       [:span "Name"]
       [:input {:id "join-modal-name"
                :type "text"
                :maxlength "24"
                :value (player-name)
                :replicant/on-mount (fn [{:replicant/keys [node]}]
                                      (.focus node)
                                      (.select node))
                :on {:keydown (fn [event]
                                (when (= "Enter" (.-key event))
                                  (join-from-modal!)))}}]]
      [:div {:class "join-modal-actions"}
       [:button {:id "join-modal-cancel"
                 :type "button"
                 :on {:click close-join-modal!}}
        "Cancel"]
       [:button {:id "join-modal-submit"
                 :type "button"
                 :disabled (join-modal-disabled? loading? player error)
                 :on {:click join-from-modal!}}
        "Join Table"]]]]))

(defn select-join-player! [player]
  (swap! app assoc-in [:join-modal :player] player))

(defn close-join-modal! []
  (swap! app assoc :join-modal nil))

(defn join-from-modal! []
  (let [{:keys [room-id loading? player error]} (:join-modal @app)
        input (el "join-modal-name")
        name (if input
               (str/trim (.-value input))
               "")]
    (when (and room-id
               (not (join-modal-disabled? loading? player error)))
      (when-not (str/blank? name)
        (set! (.-value (el "player-name")) name))
      (set! (.-value (el "join-room-id")) room-id)
      (close-join-modal!)
      (join-room! room-id player))))

(defn seat-state-label [{:keys [bot? connected?]}]
  (cond
    bot? "bot"
    connected? "online"
    :else "open"))

(defn hand-backs-html [hand-count]
  (for [n (range (min 5 hand-count))]
    [:i {:style (str "--i:" n)}]))

(defn open-seat? [{:keys [bot? name connected?]}]
  (and (not bot?)
       (not connected?)
       (str/blank? (or name ""))))

(defn player-seat-html [view {:keys [id team name connected? bot? active? hand-count] :as seat}]
  (let [current? (= id (:current-player view))
        dealer-seat? (dealer? view id)
        you? (= id (:you view))
        open? (open-seat? seat)
        occupied? (not open?)]
    [:div {:class (str "player-seat " (player-class id)
                       (team-class team)
                       (when connected? " is-connected")
                       (when bot? " is-bot")
                       (when occupied? " is-occupied")
                       (when open? " is-empty")
                       (when (false? active?) " is-sitting-out")
                       (when dealer-seat? " is-dealer")
                       (when current? " is-current")
                       (when you? " is-you"))
           :data-seat-player (when occupied? (kw-name id))
           :data-open-seat-player (when open? (kw-name id))
           :title (if open? "Open seat options" "Player options")}
     (when dealer-seat? (dealer-chip-html))
     [:div {:class "seat-copy"}
      [:strong {:class "seat-name"
                :data-fit-min 3.2}
       (if open?
         "Open seat"
         (or name (clojure.core/name id)))]
      [:small (team-label team) " / " hand-count " cards / " (seat-state-label seat)]
      (when-let [bid (latest-bid view id)]
        [:em {:class "bid-chip"} (bid-label bid)])]
     [:div {:class "seat-hand-backs"} (hand-backs-html hand-count)]
     (when current? [:em {:class "turn-badge"} "Current"])]))

(defn mobile-seat-roster-html [view]
  (into [:ul {:class "mobile-seat-roster" :aria-label "Players"}]
        (for [{:keys [id team name connected? bot? active? hand-count] :as seat} (:players view)]
          (let [current? (= id (:current-player view))
                dealer-seat? (dealer? view id)
                you? (= id (:you view))
                open? (open-seat? seat)
                occupied? (not open?)]
            [:li {:class (str (player-class id)
                              (team-class team)
                              (when connected? " is-connected")
                              (when bot? " is-bot")
                              (when occupied? " is-occupied")
                              (when open? " is-empty")
                              (when (false? active?) " is-sitting-out")
                              (when dealer-seat? " is-dealer")
                              (when current? " is-current")
                              (when you? " is-you"))
                  :data-seat-player (when occupied? (kw-name id))
                  :data-open-seat-player (when open? (kw-name id))
                  :title (if open? "Open seat options" "Player options")}
             (when dealer-seat? (dealer-chip-html))
             [:div {:class "seat-copy"}
              [:strong {:class "seat-name"
                        :data-fit-min 3.2}
               (if open?
                 "Open seat"
                 (or name (clojure.core/name id)))]
              [:span (team-label team) " / " hand-count " cards / "
               (seat-state-label seat)]]
             (if-let [bid (latest-bid view id)]
               [:em (bid-label bid)]
               [:em "--"])]))))

(defn same-play? [a b]
  (and (= (:player a) (:player b))
       (= (:card a) (:card b))))

(defn trick-card-html [view winning-play {:keys [player card] :as play}]
  [:li {:class (str "trick-card "
                    (player-class player)
                    (when (same-play? play winning-play) " is-winning"))}
   [:span (player-label view player)]
   [:strong {:class (str "card-face" (card-suit-class card))}
    (card-label card)]])

(defn play-animation-html [view winning-play {:keys [player card] :as play}]
  (when (and player card)
    [:li {:class (str "trick-card is-animating "
                      (player-class player)
                      (when (same-play? play winning-play) " is-winning")
                      " from-" (player-class player))}
     [:span (player-label view player)]
     [:strong {:class (str "card-face" (card-suit-class card))}
      (card-label card)]]))

(defn trick-html [view trick animation]
  (let [visible-trick (cond-> (vec trick)
                        animation (conj animation))
        winning-play (rules/winning-play visible-trick (:trump view))
        cards (map #(trick-card-html view winning-play %) trick)
        animation (play-animation-html view winning-play animation)]
    (if (or (seq trick) animation)
      (vec (concat cards (when animation [animation])))
      [[:li {:class "trick-empty"} [:span "No cards played"]]])))

(defn settled-trick [trick animation]
  (if animation
    (vec (remove #(same-play? % animation) trick))
    trick))

(defn trick-popup-html [view {:keys [player card]}]
  (when player
    [:div {:class "trick-winner-popup"}
     [:span "Trick winner"]
     [:strong (player-label view player)]
     (when card
       [:em "with "
        [:span {:class (str "card-face" (card-suit-class card))} (card-label card)]])]))

(defn bid-popup-html [view {:keys [player] :as bid}]
  (when player
    [:div {:class "bid-popup"}
     [:span (player-label view player)]
     [:strong (bid-label bid)] ]))

(def firework-hues [43 51 9 198 284 339 159 32 316 23 176 211])

(def firework-bursts
  [{:x 16 :y 25 :radius 7.4 :delay 0 :spokes 18 :hue-offset 0 :angle 0.15}
   {:x 50 :y 18 :radius 8.6 :delay 280 :spokes 22 :hue-offset 3 :angle 0.02}
   {:x 82 :y 28 :radius 7.8 :delay 540 :spokes 18 :hue-offset 6 :angle 0.22}
   {:x 31 :y 48 :radius 6.7 :delay 900 :spokes 16 :hue-offset 2 :angle 0.0}
   {:x 68 :y 48 :radius 7.0 :delay 1130 :spokes 16 :hue-offset 8 :angle 0.34}
   {:x 18 :y 67 :radius 6.0 :delay 1480 :spokes 14 :hue-offset 5 :angle 0.18}
   {:x 84 :y 66 :radius 6.3 :delay 1710 :spokes 14 :hue-offset 10 :angle 0.29}])

(def double-karbosh-bursts
  [{:x 50 :y 40 :radius 12.4 :delay 1700 :spokes 30 :hue-offset 0 :angle 0.07}
   {:x 24 :y 36 :radius 9.6 :delay 2180 :spokes 24 :hue-offset 4 :angle 0.29}
   {:x 76 :y 36 :radius 9.6 :delay 2360 :spokes 24 :hue-offset 8 :angle 0.11}
   {:x 50 :y 68 :radius 10.8 :delay 2840 :spokes 28 :hue-offset 2 :angle 0.2}
   {:x 14 :y 78 :radius 7.8 :delay 3460 :spokes 18 :hue-offset 6 :angle 0.36}
   {:x 86 :y 78 :radius 7.8 :delay 3460 :spokes 18 :hue-offset 10 :angle 0.0}])

(defn round-tenth [n]
  (/ (js/Math.round (* n 10)) 10))

(defn burst-particles [{:keys [x y radius delay spokes hue-offset angle]}]
  (mapv (fn [i]
          (let [theta (+ angle (/ (* 2 js/Math.PI i) spokes))
                ring (+ radius (* 0.55 (mod i 3)))
                hue (nth firework-hues (mod (+ hue-offset i) (count firework-hues)))]
            {:x x
             :y y
             :dx (round-tenth (* ring (js/Math.cos theta)))
             :dy (round-tenth (* ring (js/Math.sin theta)))
             :h hue
             :d (+ delay (* 16 (mod (* i 7) spokes)))
             :s (if (zero? (mod i 5)) 1.35 1.0)}))
        (range spokes)))

(def firework-fountain-particles
  (mapv (fn [i]
          (let [left? (< i 16)
                slot (mod i 16)
                x (if left?
                    (+ 10 (* slot 2.3))
                    (- 90 (* slot 2.3)))
                dx (if left?
                     (- (* slot 0.18) 1.8)
                     (- 1.8 (* slot 0.18)))
                dy (- -5.5 (* 0.32 (mod i 5)))
                hue (nth firework-hues (mod (+ 4 i) (count firework-hues)))]
            {:x (round-tenth x)
             :y 88
             :dx (round-tenth dx)
             :dy (round-tenth dy)
             :h hue
             :d (+ 360 (* i 42))
             :s 0.8}))
        (range 32)))

(def firework-particles
  (vec (concat (mapcat burst-particles firework-bursts)
               firework-fountain-particles)))

(def double-karbosh-particles
  (vec (concat firework-particles
               (mapcat burst-particles double-karbosh-bursts))))

(defn fireworks-particles-for [kind]
  (if (= :double-karbosh kind)
    double-karbosh-particles
    firework-particles))

(defn particle-style [{:keys [x y dx dy h d s]}]
  {"--x" (str x "%")
   "--y" (str y "%")
   "--dx" (str dx "rem")
   "--dy" (str dy "rem")
   "--dx-mid" (str (round-tenth (* dx 0.58)) "rem")
   "--dy-mid" (str (round-tenth (* dy 0.58)) "rem")
   "--dx-near" (str (round-tenth (* dx 0.9)) "rem")
   "--dy-near" (str (round-tenth (* dy 0.9)) "rem")
   "--h" h
   "--d" (str d "ms")
   "--size" (str (round-tenth (* 0.36 (or s 1))) "rem")})

(defn fireworks-bid-label [{:keys [bid-type] :as bid}]
  (case bid-type
    :double-karbosh "Double Karbosh"
    (bid-label bid)))

(defn fireworks-title [view {:keys [kind player team bid-type] :as firework}]
  (case kind
    :karbosh [(str (fireworks-bid-label firework) " made")
              (player-label view player)]
    :double-karbosh ["Double Karbosh made"
                     (str (player-label view player) " did the impossible")]
    :game-win ["Game over"
               (str (team-label team) " wins")]
    [nil (some-> bid-type name)]))

(defn fireworks-html [view {:keys [kind] :as firework}]
  (when kind
    (let [[label title] (fireworks-title view firework)]
      (into [:div {:class (str "fireworks-overlay is-" (kw-name kind))
                   :aria-hidden true}
             [:div {:class "fireworks-message"}
              [:span label]
              [:strong title]]]
            (map (fn [particle]
                   [:i {:class "firework-spark"
                        :style (particle-style particle)}])
                 (fireworks-particles-for kind))))))

(defn panel-fireworks? [fireworks]
  (contains? #{:game-win :double-karbosh} (:kind fireworks)))

(defn bid-log-html [view]
  (when (seq (:bids-this-hand view))
    [:ol {:class "bid-log"}
     (for [bid (:bids-this-hand view)]
       [:li
        [:span (player-label view (:player bid))]
        [:strong (bid-label bid)]])]))

(defn trump-value-html [suit]
  (if suit
    [:strong {:class (str "trump-symbol" (suit-class suit))}
     (cards/suit->str suit)]
    [:strong {:class "trump-symbol is-empty"} "--"]))

(defn table-hand-status-html [view]
  [:div {:class "table-hand-status"}
   [:div
    [:span "Hand"]
    [:strong (inc (or (:hand-index view) 0))]]
   [:div
    [:span "Trump"]
    (trump-value-html (:trump view))]
   [:div
    [:span "Bid"]
    [:strong (bid-label (:current-bid view))]]
   [:div
    [:span "Team 1 Tricks"]
    [:strong (get-in view [:tricks-this-hand 1] 0)]]
   [:div
    [:span "Team 2 Tricks"]
    [:strong (get-in view [:tricks-this-hand 2] 0)]]])

(defn table-status-html [view bid-popup]
  [:div {:class "table-status"}
   [:div {:class "turn-summary"}
    [:span "Current player"]
    [:strong (player-label view (:current-player view))]]
   [:div {:class "bid-trail"}
    [:span "Bid trail"]
    (if (seq (:bids-this-hand view))
      (bid-log-html view)
      [:ol {:class "bid-log is-empty"} [:li [:strong "--"]]])]
   (when bid-popup (bid-popup-html view bid-popup))])

(defn table-surface-html [view animation trick-popup queued-trick-popup fireworks]
  (let [trick (if-let [completed-trick (:trick trick-popup)]
                completed-trick
                (if-let [queued-trick (:trick queued-trick-popup)]
                  (settled-trick queued-trick animation)
                  (settled-trick (:current-trick view) animation)))]
    (into [:div {:class "table-surface"}
           (table-hand-status-html view)
           [:div {:class "felt-oval"}]]
          (concat
           (map #(player-seat-html view %) (:players view))
           [[:div {:class "table-center"}
             (into [:ul {:class "trick-pile"}]
                   (trick-html view trick animation))]
            (or (trick-popup-html view trick-popup) "")
            (or (fireworks-html view fireworks) "")]))))

(defn bot-option-html [{:keys [name]}]
  [:option {:value name} name])

(defn open-seat-popover-html [view room-id copied-player player]
  (when-let [{:keys [team] :as seat} (player-by-id view player)]
    (when (open-seat? seat)
      (let [bots (vec (:available-bot-personas view))
            invite-url (seat-invite-url room-id player)
            copied? (= copied-player player)]
        [:aside {:class (str "bot-persona-popover seat-popover seat-invite-popover "
                             (player-class player))
                 :data-seat-popover true
                 :aria-live "polite"}
         [:span {:class "bot-persona-icon seat-invite-icon"} "+"]
         [:div {:class "seat-invite-content"}
          [:strong "Open seat"]
          [:p (str (team-label team) " / Seat " (last (kw-name player)))]
          [:div {:class "seat-invite-actions"}
           [:button {:type "button"
                     :class "seat-invite-copy"
                     :data-copy-seat-invite (kw-name player)}
            (if copied? "Copied" "Copy invite URL")]
           [:code {:class "seat-invite-url"} invite-url]]
          [:div {:class "seat-bot-picker"}
           [:select {:class "seat-bot-select"
                     :data-seat-bot-select (kw-name player)
                     :disabled (empty? bots)}
            (if (seq bots)
              (map bot-option-html bots)
              [[:option {:value ""} "No bots available"]])]
           [:button {:type "button"
                     :class "seat-bot-button"
                     :data-seat-bot (kw-name player)
                     :disabled (empty? bots)}
            "Invite Bot"]]]
         [:button {:type "button"
                   :class "bot-persona-close"
                   :data-close-seat-popover true
                   :aria-label "Close open seat options"}
          "x"]]))))

(defn occupied-seat-popover-html [view player]
  (when-let [{:keys [team bot? connected?] :as seat} (player-by-id view player)]
    (when-not (open-seat? seat)
      (let [{:keys [name icon catchphrase]} (bot-player-persona view player)
            label (or name (player-label view player))
            can-kick? (and (:can-kick? view)
                           (not= player (:you view)))]
        [:aside {:class (str "bot-persona-popover seat-popover " (player-class player))
                 :data-seat-popover true
                 :aria-live "polite"}
         [:span {:class "bot-persona-icon"}
          (or icon (player-initials label))]
         [:div
          [:strong label]
          [:p (or catchphrase
                  (str (team-label team) " / "
                       (seat-state-label {:bot? bot?
                                          :connected? connected?})))]]
         (when can-kick?
           [:button {:type "button"
                     :class "seat-kick-button"
                     :data-kick-player (kw-name player)}
            "Kick"])
         [:button {:type "button"
                   :class "bot-persona-close"
                   :data-close-seat-popover true
                   :aria-label "Close player options"}
          "x"]]))))

(defn seat-popover-html [view room-id copied-player player]
  (or (open-seat-popover-html view room-id copied-player player)
      (occupied-seat-popover-html view player)))

(defn seat-popover-root-html [{:keys [view room-id seat-popover-player
                                      seat-invite-copied-player]}]
  [:div {:id "seat-popover-root"}
   (seat-popover-html view
                      room-id
                      seat-invite-copied-player
                      seat-popover-player)])

(defn card-button [{:keys [card index disabled? dragging?]}]
  [:button {:class (str "card-button" (card-suit-class card)
                        (when disabled? " is-disabled")
                        (when dragging? " is-dragging"))
            :type "button"
            :data-card (pr-str card)
            :data-hand-index index
            :data-card-disabled (if disabled? "true" "false")
            :aria-disabled (if disabled? "true" "false")
            :tabindex (when disabled? -1)
            :draggable "false"}
   (card-label card)])

(def auto-play-phases
  #{:bidding
    :trump-selection
    :karbosh-donation
    :karbosh-discard
    :trick-playing})

(defn auto-play-button [active? paused? pending? latched?]
  [:button {:type "button"
            :class (str "auto-play-button"
                        (when latched? " is-latched"))
            :data-auto-play true
            :aria-pressed (if latched? "true" "false")
            :disabled (and (not latched?)
                           (or (not active?) paused? pending?))}
   (if latched? "Auto On" "Auto Play")])

(defn hand-primary-action-button [view active? paused? pending? latched?]
  (case (:phase view)
    :hand-complete
    [:button {:type "button"
              :class "auto-play-button"
              :data-new-hand true}
     "New Hand"]

    :game-over
    [:button {:type "button"
              :class "auto-play-button"
              :data-new-game true}
     "New Game"]

    (when (auto-play-phases (:phase view))
      (auto-play-button active? paused? pending? latched?))))

(defn bid-controls [view active?]
  (when (= :bidding (:phase view))
    (let [disabled (not active?)
          legal? #(and active? (rules/legal-bid? (:current-bid view) %))
          bid-disabled? #(not (legal? %))]
      [:div {:class "control-group"}
       [:button {:type "button"
                 :data-bid "pass"
                 :disabled disabled}
        "Pass"]
       (for [n (range 1 9)]
         (let [bid {:bid-type :bid :value n}]
           [:button {:type "button"
                     :data-bid-value n
                     :disabled (bid-disabled? bid)}
            n]))
       [:button {:type "button"
                 :data-bid "karbosh"
                 :disabled (bid-disabled? {:bid-type :karbosh})}
        "Karbosh"]
       [:button {:type "button"
                 :data-bid "double-karbosh"
                 :disabled (bid-disabled? {:bid-type :double-karbosh})}
        "Double"]])))

(defn disabled-button? [target]
  (true? (.-disabled target)))

(defn trump-picker-html [view active?]
  (when (= :trump-selection (:phase view))
    [:div {:class "modal-backdrop trump-picker-backdrop"}
     [:section {:class "trump-picker-modal"
                :role "dialog"
                :aria-modal "true"
                :aria-labelledby "trump-picker-title"}
      [:strong {:id "trump-picker-title"
                :class "trump-picker-label"}
       (if active?
         "Choose trump"
         (str "Waiting for " (player-label view (:current-player view))))]
      [:div {:class "trump-suit-grid"}
       (for [suit cards/suits]
         [:button {:type "button"
                   :class (str "trump-button" (suit-class suit))
                   :data-trump (pr-str suit)
                   :disabled (not active?)}
          (cards/suit->str suit)])]]]))

(defn hand-title [view]
  (case (:phase view)
    :karbosh-donation "Donate one card"
    :karbosh-discard "Discard two cards"
    "Your hand"))

(defn karbosh-callout-html [view active?]
  (when (= :karbosh-donation (:phase view))
    (let [bid (:current-bid view)
          caller (:player bid)
          trump (:trump view)]
      [:div {:class (str "hand-phase-callout"
                         (when active? " is-active-donation"))}
       [:span {:class "hand-phase-callout-label"}
        (if active? "Donate one card" "Karbosh donation")]
       [:strong
        (if active?
          (str "Choose one card for " (player-label view caller))
          (str (player-label view (:current-player view)) " is donating to "
               (player-label view caller)))]
       [:em "Trump " (trump-value-html trump)]])))

(defn card-disabled? [view legal-set card pending-card paused?]
  (let [active? (= (:you view) (:current-player view))]
    (or pending-card
        paused?
        (not active?)
        (case (:phase view)
          :trick-playing
          (not (contains? legal-set card))

          (:karbosh-donation :karbosh-discard)
          false

          true))))

(defn sort-hand-button [disabled?]
  [:button {:class "sort-hand-button"
            :type "button"
            :data-sort-hand true
            :disabled disabled?}
   "Sort"])

(defn fast-mode-button [mode]
  (let [mode (normalize-speed-mode mode)
        next-mode (next-speed-mode mode)]
    [:button {:class (str "fast-mode-button"
                          (when (fast-speed? mode) " is-active")
                          (when (ultra-fast-speed? mode) " is-ultra"))
              :type "button"
              :data-speed-mode (name next-mode)
              :aria-pressed (if (fast-speed? mode) "true" "false")}
     (case mode
       :ultra-fast "Ultra"
       "Fast")]))

(defn hand-panel-html [{:keys [view pending-card hand-order card-drag
                               hand-animating? pending-auto? speed-mode
                               trick-popup queued-trick-popup
                               auto-play-latched?]}]
  (let [paused? (or (some? trick-popup) (some? queued-trick-popup))
        hand (displayed-hand view pending-card hand-order)
        legal-set (when (= :trick-playing (:phase view))
                    (set (rules/legal-cards hand (:current-trick view) (:trump view))))
        dragging-index (:index card-drag)
        sorting? (:dragging? card-drag)
        active? (= (:you view) (:current-player view))]
    [:section {:class (str "hand-panel"
                           (when (= :karbosh-donation (:phase view))
                             " is-karbosh-donation"))}
     [:div {:class "hand-heading"}
      [:h2 (hand-title view)]
      [:div {:class "hand-actions"}
       [:span (count hand) " cards"]
       (sort-hand-button (or pending-card (empty? hand)))
       (fast-mode-button speed-mode)
       (hand-primary-action-button view active? paused? pending-auto?
                                   auto-play-latched?)]]
     (karbosh-callout-html view active?)
     [:div {:class (str "hand-row"
                        (when sorting? " is-sorting")
                        (when hand-animating? " is-animating")
                        (when (and active?
                                   (= :karbosh-donation (:phase view)))
                          " is-donation-pick"))}
      (map-indexed
       (fn [index card]
         (card-button {:card card
                      :index index
                      :disabled? (card-disabled? view legal-set card pending-card paused?)
                      :dragging? (and sorting? (= index dragging-index))}))
       hand)]]))

(defn game-over-html [view]
  (when (= :game-over (:phase view))
    [:section {:class "game-over-panel"}
     [:span "Game over"]
     [:strong (team-label (:winner view)) " wins"]
     [:em "Final score " (get-in view [:scores 1] 0) " / " (get-in view [:scores 2] 0)]]))

(defn fill-bots-button []
  [:button {:class "table-fill-bots-button"
            :type "button"
            :data-fill-bots true}
   "Fill Bots"])

(defn table-new-game-button []
  [:button {:class "table-new-game-button"
            :type "button"
            :data-new-game true}
   "New Game"])

(defn leave-room-button []
  [:button {:class "leave-room-button"
            :type "button"
            :data-leave-room true}
   "Leave Room"])

(defn workbench-capture-form [room-id]
  [:form {:class "table-workbench-form"
          :method "post"
          :action (str "/karbosh/admin/workbench/" room-id)}
   [:input {:type "hidden"
            :name "action"
            :value "capture-room"}]
   [:input {:type "hidden"
            :name "note"
            :value "Captured from the game table."}]
   [:button {:class "table-workbench-button"
             :type "submit"
             :title "Send this hand to the workbench"}
    "Workbench"]])

(defn room-visibility-button [view]
  (let [public? (:public? view)]
    [:button {:class "visibility-button"
              :type "button"
              :data-room-public (if public? "false" "true")}
     (if public? "Make Private" "Make Public")]))

(defn table-top-actions [view room-id]
  [:div {:class "table-top-actions"}
   [:div {:class "table-main-actions"}
    (fill-bots-button)
    (room-visibility-button view)]
   [:div {:class "table-room-actions"}
    (workbench-capture-form room-id)
    (table-new-game-button)
    (leave-room-button)]])

(defn render-controls [view]
  (let [active? (= (:you view) (:current-player view))]
    (filter identity
            [(bid-controls view active?)])))

(defn modal-hiccup [{:keys [join-modal view bid-popup]}]
  (if join-modal
    (join-modal-hiccup join-modal)
    (when-not (some? bid-popup)
      (trump-picker-html view (= (:you view) (:current-player view))))))

(defn game-hiccup [{:keys [view room-id play-animation trick-popup queued-trick-popup
                           fireworks] :as state}]
  (when view
    [:section {:class "table-grid"}
     [:div {:class "panel table-panel"}
      [:div {:class "panel-heading"}
       [:p {:class "eyebrow"} "Karbosh table"]
       [:h1 "Room " room-id]
       [:p {:class "status-line"}
        (phase-label (:phase view)) " / Current: "
        (player-label view (:current-player view))]
       (table-top-actions view room-id)]
      [:section {:class "score-summary" :aria-label "Total scores"}
       [:span {:class "score-summary-label"} "Total scores"]
       [:div {:class "score-row"}
        [:span "Team 1 " [:strong (get-in view [:scores 1] 0)]]
        [:span "Team 2 " [:strong (get-in view [:scores 2] 0)]]]]
      (game-over-html view)
      (when (panel-fireworks? fireworks)
        (fireworks-html view fireworks))
      (table-surface-html view
                          play-animation
                          trick-popup
                          queued-trick-popup
                          (when-not (panel-fireworks? fireworks)
                            fireworks))
      [:div {:class "play-controls-panel"}
       (hand-panel-html state)
       [:div {:class "controls"}
        (render-controls view)]]
      (seat-popover-root-html state)
      (mobile-seat-roster-html view)]]))

(defn show-seat-popover! [player]
  (swap! app assoc
         :seat-popover-player player
         :seat-invite-copied-player nil))

(defn close-seat-popover! []
  (swap! app assoc
         :seat-popover-player nil
         :seat-invite-copied-player nil))

(defn card-event [view card]
  (case (:phase view)
    :trick-playing {:type :play-card :card card}
    :karbosh-donation {:type :donate-card :card card}
    :karbosh-discard {:type :discard-card :card card}
    nil))

(defn play-card! [card]
  (when-not (or (:trick-popup @app) (:queued-trick-popup @app))
    (when-let [event (card-event (:view @app) card)]
      (swap! app assoc :pending-card card)
      (action! event))))

(defn played-card-event [old-view new-view]
  (when old-view
    (let [old-trick (:current-trick old-view)
          new-trick (:current-trick new-view)]
      (cond
        (> (count new-trick) (count old-trick))
        (nth new-trick (count old-trick))

        (> (count (:completed-tricks new-view))
           (count (:completed-tricks old-view)))
        (let [completed-trick (nth (:completed-tricks new-view)
                                   (count (:completed-tricks old-view)))
              card-index (min (count old-trick)
                              (dec (count completed-trick)))]
          (nth completed-trick card-index))))))

(defn won-trick-event [old-view new-view]
  (when (and old-view
             (> (count (:completed-tricks new-view))
                (count (:completed-tricks old-view))))
    (let [trick (last (:completed-tricks new-view))
          winner (rules/resolve-trick trick (:trump new-view))
          card (:card (first (filter #(= winner (:player %)) trick)))]
      {:player winner
       :card card
       :trick trick})))

(defn clear-trick-popup! [popup-id]
  (when (= popup-id (:id (:trick-popup @app)))
    (swap! app assoc :trick-popup nil)
    (maybe-run-latched-auto-play!)))

(defn show-trick-popup! [popup]
  (swap! app assoc
         :play-animation nil
         :queued-trick-popup nil
         :trick-popup popup)
  (js/setTimeout #(clear-trick-popup! (:id popup)) (timing-ms :trick-popup)))

(defn clear-play-animation! [animation-id]
  (when (= animation-id (:id (:play-animation @app)))
    (if-let [popup (:queued-trick-popup @app)]
      (show-trick-popup! popup)
      (do
        (swap! app assoc :play-animation nil)
        (maybe-run-latched-auto-play!)))))

(defn bid-event [old-view new-view]
  (when old-view
    (let [old-count (count (:bids-this-hand old-view))
          new-bids (:bids-this-hand new-view)]
      (when (> (count new-bids) old-count)
        (nth new-bids old-count)))))

(defn karbosh-bid? [bid]
  (contains? #{:karbosh :double-karbosh} (:bid-type bid)))

(def completed-hand-phases #{:hand-complete :game-over})

(defn completed-hand-event? [old-view new-view]
  (and old-view
       (not (contains? completed-hand-phases (:phase old-view)))
       (contains? completed-hand-phases (:phase new-view))))

(defn player-team [view player]
  (:team (player-by-id view player)))

(defn successful-karbosh-event [old-view new-view]
  (let [bid (:current-bid new-view)
        team (player-team new-view (:player bid))]
    (when (and (completed-hand-event? old-view new-view)
               (karbosh-bid? bid)
               (= 8 (get-in new-view [:tricks-this-hand team] 0)))
      (assoc bid :team team))))

(defn game-winner-event [old-view new-view]
  (when (and old-view
             (not= :game-over (:phase old-view))
             (= :game-over (:phase new-view)))
    {:team (:winner new-view)}))

(defn clear-bid-popup! [popup-id]
  (when (= popup-id (:id (:bid-popup @app)))
    (swap! app assoc :bid-popup nil)
    (maybe-run-latched-auto-play!)))

(defn clear-fireworks! [fireworks-id]
  (when (= fireworks-id (:id (:fireworks @app)))
    (swap! app assoc :fireworks nil)))

(defn fireworks-duration-ms [fireworks]
  (let [base (timing-ms :fireworks)]
    (if (= :double-karbosh (:kind fireworks))
      (js/Math.round (* 1.7 base))
      base)))

(defn show-fireworks! [fireworks]
  (when (and (= (:room-id fireworks) (:room-id @app))
             (= (:hand-index fireworks) (get-in @app [:view :hand-index])))
    (swap! app assoc :fireworks fireworks)
    (js/setTimeout #(clear-fireworks! (:id fireworks))
                   (fireworks-duration-ms fireworks))))

(defn fireworks-delay-ms [fireworks animation popup]
  (+ (if animation (timing-ms :play-animation) 0)
     (if (and popup (not (panel-fireworks? fireworks)))
       (+ (timing-ms :trick-popup) 120)
       0)))

(defn reset-room-state! [message]
  (when-let [socket (:socket @app)]
    (.close socket))
  (clear-session!)
  (clear-room-url!)
  (swap! app assoc
         :socket nil
         :connected? false
         :room-id nil
         :player nil
         :view nil
         :play-animation nil
         :trick-popup nil
         :queued-trick-popup nil
         :bid-popup nil
         :fireworks nil
         :seat-popover-player nil
         :seat-invite-copied-player nil
         :join-modal nil
         :hand-order nil
         :card-drag nil
         :hand-animating? false
         :suppress-card-click? false
         :pending-card nil
         :pending-auto? false
         :auto-play-latched? false
         :suppress-auto-click? false
         :error message)
  (render-recent-rooms!))

(defn handle-server-message! [raw]
  (let [message (reader/read-string raw)]
    (case (:op message)
      :state
      (let [old-view (:view @app)
            view (:view message)
            speed-mode (normalize-speed-mode (or (:speed-mode view)
                                                 (:fast-mode? view)))
            fast-mode? (fast-speed? speed-mode)
            hand-order (reconcile-hand-order (:hand-order @app)
                                             (:hand view)
                                             (:hand-index view))
            animation (played-card-event old-view view)
            trick-winner (won-trick-event old-view view)
            bid (bid-event old-view view)
            successful-karbosh (successful-karbosh-event old-view view)
            game-winner (game-winner-event old-view view)
            now (.now js/Date)
            animation-id (when animation
                           (str now "-" (kw-name (:player animation))))
            popup-id (when trick-winner
                       (str now "-trick-winner"))
            popup (some-> trick-winner (assoc :id popup-id))
            queue-popup? (and animation popup)
            bid-popup-id (when bid
                           (str now "-bid-" (kw-name (:player bid))))
            double-karbosh (when (= :double-karbosh
                                    (:bid-type successful-karbosh))
                              successful-karbosh)
            fireworks (cond
                        double-karbosh
                        (assoc double-karbosh
                               :id (str now "-double-karbosh-made-"
                                        (kw-name (:player double-karbosh)))
                               :room-id (:room-id message)
                               :hand-index (:hand-index view)
                               :kind :double-karbosh)

                        game-winner
                        (assoc game-winner
                               :id (str now "-game-win")
                               :room-id (:room-id message)
                               :hand-index (:hand-index view)
                               :kind :game-win)

                        successful-karbosh
                        (assoc successful-karbosh
                               :id (str now "-karbosh-made-" (kw-name (:player successful-karbosh)))
                               :room-id (:room-id message)
                               :hand-index (:hand-index view)
                               :kind :karbosh))]
        (swap! app assoc
               :room-id (:room-id message)
               :player (:player message)
               :view view
               :play-animation (some-> animation (assoc :id animation-id))
               :trick-popup (when-not queue-popup? popup)
               :queued-trick-popup (when queue-popup? popup)
               :bid-popup (some-> bid (assoc :id bid-popup-id))
               :fireworks (:fireworks @app)
               :hand-order hand-order
               :card-drag nil
               :hand-animating? false
               :pending-card nil
               :pending-auto? false
               :speed-mode speed-mode
               :fast-mode? fast-mode?
               :error nil)
        (persist-speed-mode! speed-mode)
        (set-room-url! (:room-id message))
        (save-session! (:room-id message) (:player message) (player-name))
        (when animation-id
          (js/setTimeout #(clear-play-animation! animation-id) (timing-ms :play-animation)))
        (when (and popup (not queue-popup?))
          (js/setTimeout #(clear-trick-popup! popup-id) (timing-ms :trick-popup)))
        (when bid-popup-id
          (js/setTimeout #(clear-bid-popup! bid-popup-id) (timing-ms :bid-popup)))
        (when fireworks
          (js/setTimeout #(show-fireworks! fireworks)
                         (fireworks-delay-ms fireworks animation popup)))
        (maybe-run-latched-auto-play!))

      :error
      (swap! app assoc
             :error (:message message)
             :pending-card nil
             :auto-play-latched? false
             :pending-auto? false)

      :pong nil
      :left-room
      (reset-room-state! nil)

      :room-closed
      (reset-room-state! "Room closed")

      :kicked
      (reset-room-state! "Kicked from room")

      nil)))

(defn connect! [after-open]
  (when-let [old (:socket @app)]
    (.close old))
  (let [socket (js/WebSocket. (ws-url))]
    (swap! app assoc :socket socket :connected? false :error nil)
    (set! (.-onopen socket)
          (fn []
            (swap! app assoc :connected? true)
            (after-open)))
    (set! (.-onclose socket)
          (fn []
            (when (= socket (:socket @app))
              (swap! app assoc :connected? false))))
    (set! (.-onerror socket)
          (fn []
            (when (= socket (:socket @app))
              (swap! app assoc :error "Connection error"))))
    (set! (.-onmessage socket)
          (fn [event]
            (when (= socket (:socket @app))
              (perf-queue-delay! event)
              (perf-time! :msgs :msg-ms :max-msg-ms
                          #(handle-server-message! (.-data event))))))))

(defn create-room! []
  (let [seed (create-room-seed)]
    (if (= invalid-seed seed)
      (set-error! "Seed must be an integer")
      (connect! #(send! (cond-> {:op :create-room
                                 :name (player-name)
                                 :public? (create-public-room?)
                                 :speed-mode (:speed-mode @app)
                                 :fast-mode? (:fast-mode? @app)}
                          (some? seed) (assoc :seed seed)))))))

(defn join-room! [room-id player]
  (connect! #(send! {:op :join-room
                     :room-id room-id
                     :player player
                     :name (player-name)})))

(defn reconnect-room-id []
  (or (:room-id @app)
      (stored-room-id)))

(defn reconnect-player []
  (or (:player @app)
      (stored-player)))

(defn reconnect-visible-room! []
  (let [now (.now js/Date)
        last-reconnect (:last-reconnect-at @app)
        room-id (reconnect-room-id)
        player (reconnect-player)]
    (when (and room-id
               player
               (not (:join-modal @app))
               (not (.-hidden js/document))
               (> (- now last-reconnect) reconnect-throttle-ms))
      (swap! app assoc
             :last-reconnect-at now
             :connected? false
             :error "Reconnecting")
      (join-room! room-id player))))

(defn reconnect-on-wake! []
  (when-not (.-hidden js/document)
    (reconnect-visible-room!)))

(defn fill-bots! []
  (send! {:op :fill-bots}))

(defn auto-play! []
  (swap! app assoc :pending-auto? true)
  (send! {:op :auto-play}))

(defn auto-play-ready? [{:keys [view pending-auto? auto-play-latched?
                                trick-popup queued-trick-popup]}]
  (and auto-play-latched?
       view
       (auto-play-phases (:phase view))
       (= (:you view) (:current-player view))
       (not pending-auto?)
       (nil? trick-popup)
       (nil? queued-trick-popup)))

(defn maybe-run-latched-auto-play! []
  (when (auto-play-ready? @app)
    (auto-play!)))

(defn clear-suppressed-auto-click! []
  (swap! app assoc :suppress-auto-click? false))

(defn suppress-next-auto-click! []
  (swap! app assoc :suppress-auto-click? true)
  (js/setTimeout
   (fn []
     (when (:suppress-auto-click? @app)
       (clear-suppressed-auto-click!)))
   350))

(defn start-auto-play-latch! []
  (swap! app assoc :auto-play-latched? true)
  (maybe-run-latched-auto-play!))

(defn stop-auto-play-latch! []
  (swap! app assoc :auto-play-latched? false))

(defn handle-auto-play-click! [event]
  (cond
    (:suppress-auto-click? @app)
    (do
      (.preventDefault event)
      (clear-suppressed-auto-click!))

    (:auto-play-latched? @app)
    (do
      (.preventDefault event)
      (stop-auto-play-latch!))

    :else
    (auto-play!)))

(defn begin-auto-play-hold! [event]
  (when-let [node (closest (.-target event) "[data-auto-play]")]
    (when (and (or (nil? (.-button event)) (zero? (.-button event)))
               (not (:auto-play-latched? @app))
               (not (disabled-button? node)))
      (let [pointer-id (.-pointerId event)
            timer (js/setTimeout
                   (fn []
                     (when (= pointer-id (:pointer-id @auto-play-hold*))
                       (swap! auto-play-hold* assoc :fired? true)
                       (start-auto-play-latch!)))
                   auto-play-hold-threshold-ms)]
        (reset! auto-play-hold* {:pointer-id pointer-id
                                 :timer timer
                                 :fired? false})))))

(defn finish-auto-play-hold! [event]
  (when-let [{:keys [pointer-id timer fired?]} @auto-play-hold*]
    (when (= pointer-id (.-pointerId event))
      (js/clearTimeout timer)
      (reset! auto-play-hold* nil)
      (when fired?
        (.preventDefault event)
        (suppress-next-auto-click!)))))

(defn leave-room! []
  (send! {:op :leave-room}))

(defn kick-player! [player]
  (close-seat-popover!)
  (send! {:op :kick-player :player (kw-name player)}))

(defn copy-seat-invite! [player]
  (let [url (seat-invite-url (:room-id @app) player)]
    (when-let [clipboard (.-clipboard js/navigator)]
      (.writeText clipboard url))
    (swap! app assoc :seat-invite-copied-player player)
    (js/setTimeout
     (fn []
       (when (= player (:seat-invite-copied-player @app))
         (swap! app assoc :seat-invite-copied-player nil)))
     1800)))

(defn selected-seat-bot-name [player]
  (some-> (qs (str "[data-seat-bot-select=\"" (kw-name player) "\"]"))
          .-value
          str/trim
          not-empty))

(defn seat-bot! [player]
  (when-let [bot-name (selected-seat-bot-name player)]
    (close-seat-popover!)
    (send! {:op :seat-bot
            :player (kw-name player)
            :bot-name bot-name})))

(defn set-room-visibility! [public?]
  (send! {:op :set-room-visibility :public? public?})
  (js/setTimeout load-public-rooms-if-needed! 500))

(defn clear-hand-animation! []
  (when (:hand-animating? @app)
    (swap! app assoc :hand-animating? false)))

(defn pulse-hand-animation! []
  (swap! app assoc :hand-animating? true)
  (js/setTimeout clear-hand-animation! (timing-ms :hand-animation)))

(defn sort-hand! []
  (when-let [view (:view @app)]
    (let [sorted-hand (hand-order/sorted-hand (:hand view) (:trump view))]
      (swap! app assoc
             :hand-order {:hand-index (:hand-index view)
                          :cards sorted-hand})
      (pulse-hand-animation!))))

(defn set-speed-mode! [mode]
  (let [mode (normalize-speed-mode mode)]
    (persist-speed-mode! mode)
    (swap! app assoc
           :speed-mode mode
           :fast-mode? (fast-speed? mode))
    (send! {:op :set-fast-mode
            :speed-mode mode
            :fast-mode? (fast-speed? mode)})))

(def drag-threshold-px 8)

(defn read-card-attr [node]
  (reader/read-string (.getAttribute node "data-card")))

(defn read-hand-index-attr [node]
  (let [n (js/parseInt (.getAttribute node "data-hand-index") 10)]
    (when-not (js/isNaN n) n)))

(defn card-button-node [target]
  (closest target ".card-button[data-card]"))

(defn hand-card-nodes []
  (when-let [row (qs ".hand-row")]
    (array-seq (js/Array.from (.querySelectorAll row ".card-button[data-card]")))))

(defn hand-card-rects
  "Snapshot the hand's card slot rects as plain data. The slots stay put while
  cards reorder within them, so one capture at drag start serves the whole drag."
  []
  (vec (for [node (hand-card-nodes)]
         (let [rect (.getBoundingClientRect node)]
           {:index (read-hand-index-attr node)
            :cx (+ (.-left rect) (/ (.-width rect) 2))
            :cy (+ (.-top rect) (/ (.-height rect) 2))
            :height (.-height rect)}))))

(defn rect-distance [{:keys [cx cy]} x y]
  (let [dx (- x cx)
        dy (- y cy)]
    (+ (* dx dx) (* dy dy))))

(defn nearest-card-rect [rects x y]
  (when (seq rects)
    (apply min-key #(rect-distance % x y) rects)))

(defn after-rect? [{:keys [cx cy height]} x y]
  (let [dx (- x cx)
        dy (- y cy)]
    (if (> (js/Math.abs dy) (* 0.65 height))
      (pos? dy)
      (pos? dx))))

(defn reorder-dragged-card [state target-index after?]
  (let [view (:view state)
        cards (ordered-hand view (:hand-order state))
        from-index (get-in state [:card-drag :index])]
    (if (and from-index target-index (not= from-index target-index))
      (let [{:keys [cards index]} (hand-order/move-index-to cards
                                                            from-index
                                                            target-index
                                                            after?)]
        (-> state
            (assoc :hand-order {:hand-index (:hand-index view)
                                :cards cards})
            (assoc-in [:card-drag :index] index)))
      state)))

(defn begin-card-drag! [event]
  (when-let [node (card-button-node (.-target event))]
    (when (or (nil? (.-button event)) (zero? (.-button event)))
      (swap! app assoc
             :card-drag {:card (read-card-attr node)
                         :index (read-hand-index-attr node)
                         :rects (hand-card-rects)
                         :pointer-id (.-pointerId event)
                         :start-x (.-clientX event)
                         :start-y (.-clientY event)
                         :dragging? false})
      nil)))

(defn update-card-drag! [event]
  (when-let [{:keys [pointer-id start-x start-y dragging? rects] :as drag} (:card-drag @app)]
    (when (= pointer-id (.-pointerId event))
      (let [x (.-clientX event)
            y (.-clientY event)
            dx (- x start-x)
            dy (- y start-y)
            moved? (> (js/Math.sqrt (+ (* dx dx) (* dy dy))) drag-threshold-px)]
        (when (or dragging? moved?)
          (.preventDefault event)
          (let [target (nearest-card-rect rects x y)
                target-index (:index target)
                after? (when target (after-rect? target x y))]
            (swap! app
                   (fn [state]
                     (-> state
                         (assoc :card-drag (assoc drag :dragging? true))
                         (cond-> target-index
                           (reorder-dragged-card target-index after?)))))))))))

(defn clear-suppressed-card-click! []
  (swap! app assoc :suppress-card-click? false))

(defn finish-card-drag! [event]
  (when-let [{:keys [pointer-id dragging?]} (:card-drag @app)]
    (when (= pointer-id (.-pointerId event))
      (when dragging?
        (.preventDefault event)
        (swap! app assoc :suppress-card-click? true)
        (js/setTimeout clear-suppressed-card-click! 250))
      (swap! app assoc :card-drag nil)
      (when dragging?
        (pulse-hand-animation!)))))

(defn render!
  "Project one immutable state snapshot onto the page. The only function that
  touches replicant; everything it renders is pure hiccup derived from state."
  [state]
  (active-game-layout! state)
  (render-status! state)
  (when (:view state)
    (set-share-link! (:room-id state)))
  (d/render (el "game-root") (game-hiccup state))
  (d/render (el "modal-root") (modal-hiccup state))
  (when-let [root (el "public-rooms-root")]
    (d/render root (when (public-rooms-visible? state)
                     (public-rooms-hiccup (:public-rooms state)))))
  (maybe-fit-seat-names!))

(defonce ^:private render-scheduled? (volatile! false))

(defn- schedule-render!
  "Coalesce any number of state changes per frame into one render of the
  newest state."
  []
  (when-not @render-scheduled?
    (vreset! render-scheduled? true)
    (js/requestAnimationFrame
     (fn [_]
       (vreset! render-scheduled? false)
       (perf-time! :renders :render-ms :max-render-ms
                   #(render! @app))))))

(defn start-render-loop! []
  (add-watch app ::render
             (fn [_ _ old new]
               (when (not= old new)
                 (when (entered-public-rooms? old new)
                   (load-public-rooms-if-needed!))
                 (schedule-render!))))
  (schedule-render!))

(defn bind-controls! []
  (.addEventListener (el "create-room") "click" create-room!)
  (.addEventListener (el "join-room") "click"
                     (fn []
                       (let [room-id (str/trim (.-value (el "join-room-id")))]
                         (when-not (str/blank? room-id)
                           (prepare-shared-room! room-id)))))
  (when-let [public-root (el "public-rooms-root")]
    (.addEventListener public-root "click"
                       (fn [event]
                         (let [target (.-target event)]
                           (when-let [room-target (closest target "[data-public-room]")]
                             (prepare-shared-room!
                              (.getAttribute room-target "data-public-room")))))))
  (when-let [recent-root (el "recent-rooms-root")]
    (.addEventListener recent-root "click"
                       (fn [event]
                         (let [target (.-target event)
                               resume-target (closest target "[data-recent-resume]")
                               choose-target (closest target "[data-recent-choose]")
                               forget-target (closest target "[data-recent-forget]")]
                           (cond
                             resume-target
                             (resume-recent-room!
                              (.getAttribute resume-target "data-recent-resume")
                              (keyword (.getAttribute resume-target "data-recent-player"))
                              (.getAttribute resume-target "data-recent-name"))

                             choose-target
                             (prepare-shared-room!
                              (.getAttribute choose-target "data-recent-choose"))

                             forget-target
                             (forget-room!
                              (.getAttribute forget-target "data-recent-forget")))))))
  (.addEventListener (el "copy-link") "click"
                     (fn []
                       (when-let [text (not-empty (.-textContent (el "share-link")))]
                         (.. js/navigator -clipboard (writeText text)))))
  (when-let [fill-bots (el "fill-bots")]
    (.addEventListener fill-bots "click" fill-bots!))
  (.addEventListener (el "modal-root") "click"
                     (fn [event]
                       (let [target (.-target event)
                             trump-target (closest target "[data-trump]")
                             auto-target (closest target "[data-auto-play]")]
                         (cond
                           trump-target
                           (action! {:type :trump-selection
                                     :suit (reader/read-string
                                            (.getAttribute trump-target "data-trump"))})

                           auto-target
                           (handle-auto-play-click! event)))))
  (.addEventListener (el "modal-root") "pointerdown" begin-auto-play-hold!)
  (.addEventListener (el "game-root") "pointerdown" begin-card-drag!)
  (.addEventListener (el "game-root") "pointerdown" begin-auto-play-hold!)
  (.addEventListener js/window "pointermove" update-card-drag!)
  (.addEventListener js/window "pointerup" finish-card-drag!)
  (.addEventListener js/window "pointerup" finish-auto-play-hold!)
  (.addEventListener js/window "pointercancel" finish-card-drag!)
  (.addEventListener js/window "pointercancel" finish-auto-play-hold!)
  (.addEventListener js/window "resize" schedule-fit-seat-names!)
  (.addEventListener js/window "focus" reconnect-on-wake!)
  (.addEventListener js/window "pageshow" reconnect-on-wake!)
  (.addEventListener js/document "visibilitychange" reconnect-on-wake!)
  (.addEventListener (el "game-root") "click"
                     (fn [event]
                       (let [target (.-target event)
                             card-target (card-button-node target)
                             seat-target (closest target "[data-seat-player]")
                             open-seat-target (closest target "[data-open-seat-player]")
                             close-seat-target (closest target "[data-close-seat-popover]")
                             kick-target (closest target "[data-kick-player]")
                             copy-seat-invite-target (closest target "[data-copy-seat-invite]")
                             seat-bot-target (closest target "[data-seat-bot]")
                             fast-target (or (closest target "[data-speed-mode]")
                                             (closest target "[data-fast-mode]"))
                             popover-target (closest target "[data-seat-popover]")]
                         (cond
                           close-seat-target
                           (do
                             (.preventDefault event)
                             (close-seat-popover!))

                           kick-target
                           (do
                             (.preventDefault event)
                             (kick-player!
                              (keyword (.getAttribute kick-target "data-kick-player"))))

                           copy-seat-invite-target
                           (do
                             (.preventDefault event)
                             (copy-seat-invite!
                              (keyword (.getAttribute copy-seat-invite-target
                                                      "data-copy-seat-invite"))))

                           seat-bot-target
                           (do
                             (.preventDefault event)
                             (seat-bot!
                              (keyword (.getAttribute seat-bot-target
                                                      "data-seat-bot"))))

                           seat-target
                           (do
                             (.preventDefault event)
                             (show-seat-popover!
                              (keyword (.getAttribute seat-target "data-seat-player"))))

                           open-seat-target
                           (do
                             (.preventDefault event)
                             (show-seat-popover!
                              (keyword (.getAttribute open-seat-target
                                                      "data-open-seat-player"))))

                           (.hasAttribute target "data-bid")
                           (when-not (disabled-button? target)
                             (let [bid (keyword (.getAttribute target "data-bid"))]
                               (action! {:type :bid :bid-type bid})))

                           (.hasAttribute target "data-bid-value")
                           (when-not (disabled-button? target)
                             (action! {:type :bid
                                       :bid-type :bid
                                       :value (js/Number (.getAttribute target "data-bid-value"))}))

                           (.hasAttribute target "data-trump")
                           (action! {:type :trump-selection
                                     :suit (reader/read-string (.getAttribute target "data-trump"))})

                           card-target
                           (if (:suppress-card-click? @app)
                             (do
                               (.preventDefault event)
                               (clear-suppressed-card-click!))
                             (when-not (= "true" (.getAttribute card-target "data-card-disabled"))
                               (play-card! (read-card-attr card-target))))

                           (.hasAttribute target "data-auto-play")
                           (handle-auto-play-click! event)

                           (.hasAttribute target "data-sort-hand")
                           (sort-hand!)

                           fast-target
                           (set-speed-mode!
                            (or (.getAttribute fast-target "data-speed-mode")
                                (= "true" (.getAttribute fast-target "data-fast-mode"))))

                           (.hasAttribute target "data-fill-bots")
                           (fill-bots!)

                           (.hasAttribute target "data-leave-room")
                           (leave-room!)

                           (.hasAttribute target "data-room-public")
                           (set-room-visibility!
                            (= "true" (.getAttribute target "data-room-public")))

                           (.hasAttribute target "data-reshuffle-hand")
                           (action! {:type :reshuffle-hand})

                           (.hasAttribute target "data-new-game")
                           (action! {:type :new-game})

                           (.hasAttribute target "data-new-hand")
                           (action! {:type :new-hand})

                           (and (:seat-popover-player @app)
                                (not popover-target))
                           (close-seat-popover!))))))

(defn focus-join-flow! []
  (let [name-input (el "player-name")
        setup (el "setup")]
    (when setup
      (.scrollIntoView setup #js {:behavior "smooth" :block "start"}))
    (when name-input
      (.focus name-input)
      (.select name-input))))

(defn load-room-preview! [room]
  (swap! app assoc :join-modal {:room-id room
                                :loading? true
                                :preview nil
                                :player nil
                                :error nil})
  (-> (js/fetch (room-preview-url room))
      (.then (fn [response]
               (-> (.text response)
                   (.then (fn [body]
                            (let [data (reader/read-string body)]
                              (swap! app assoc
                                     :join-modal
                                     (if (:ok data)
                                       {:room-id room
                                        :loading? false
                                        :preview data
                                        :player (default-join-player data)
                                        :error nil}
                                       {:room-id room
                                        :loading? false
                                        :preview nil
                                        :player nil
                                        :error (:message data)}))))))))
      (.catch (fn [_]
                (swap! app assoc :join-modal {:room-id room
                                              :loading? false
                                              :preview nil
                                              :player nil
                                              :error "Could not load this room."})))))

(defn load-public-rooms! []
  (swap! app update :public-rooms
         (fn [public-rooms]
           (assoc public-rooms :loading? true :error nil)))
  (-> (js/fetch (public-rooms-url))
      (.then (fn [response]
               (-> (.text response)
                   (.then (fn [body]
                            (let [data (reader/read-string body)]
                              (when (public-rooms-visible? @app)
                                (swap! app assoc
                                       :public-rooms
                                       (if (:ok data)
                                         {:loading? false
                                          :rooms (:rooms data)
                                          :error nil}
                                         {:loading? false
                                          :rooms []
                                          :error "Could not load public rooms."})))))))))
      (.catch (fn [_]
                (when (public-rooms-visible? @app)
                  (swap! app assoc :public-rooms {:loading? false
                                                  :rooms []
                                                  :error "Could not load public rooms."}))))))

(defn load-public-rooms-if-needed! []
  (when (public-rooms-visible? @app)
    (load-public-rooms!)))

(defn prepare-shared-room! [room]
  (set! (.-value (el "join-room-id")) room)
  (load-room-preview! room))

(defn restore-saved-room! [room]
  (set! (.-value (el "join-room-id")) room)
  (if-let [player (stored-player)]
    (join-room! room player)
    (focus-join-flow!)))

(defn init! []
  (let [stored-name (.getItem js/localStorage "karbosh-name")]
    (when stored-name
      (set! (.-value (el "player-name")) stored-name)))
  (bind-controls!)
  (start-render-loop!)
  (render-recent-rooms!)
  (when perf?
    (start-perf-overlay!))
  (if-let [room (query-room-param)]
    (if (and (stored-player)
             (same-room-id? room (stored-room-id)))
      (restore-saved-room! room)
      (prepare-shared-room! room))
    (do
      (when-let [room (stored-room-id)]
        (set! (.-value (el "join-room-id")) room))
      (load-public-rooms-if-needed!))))

(set! (.-onload js/window) init!)
