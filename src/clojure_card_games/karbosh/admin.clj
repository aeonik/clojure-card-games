(ns clojure-card-games.karbosh.admin
  (:require [clojure.string :as str]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.hand-order :as hand-order]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.trick-lab :as trick-lab]
            [clojure-card-games.karbosh.hiccup :as h])
  (:import [java.lang.management ManagementFactory]
           [java.time Instant]))

(defn kw-label [x]
  (if x
    (-> (name x)
        (str/replace "-" " ")
        (str/capitalize))
    "--"))

(defn team-label [team]
  (str "Team " team))

(defn score-label [scores]
  (str (get scores 1 0) " / " (get scores 2 0)))

(defn bid-label [{:keys [bid-type value]}]
  (case bid-type
    :pass "Pass"
    :bid (str "Bid " value)
    :karbosh "Karbosh"
    :double-karbosh "Double"
    "--"))

(defn duration-label [ms]
  (let [seconds (quot ms 1000)
        minutes (quot seconds 60)
        hours (quot minutes 60)]
    (cond
      (pos? hours) (str hours "h " (mod minutes 60) "m")
      (pos? minutes) (str minutes "m " (mod seconds 60) "s")
      :else (str seconds "s"))))

(defn bytes-label [n]
  (let [mb (/ (double n) 1048576.0)]
    (format "%.1f MB" mb)))

(defn time-label [ms]
  (if ms
    (str (Instant/ofEpochMilli ms))
    "--"))

(defn suit-class [suit]
  (case suit
    :♥ " heart"
    :♦ " diamond"
    :♠ " spade"
    :♣ " club"
    ""))

(defn card-class [[_ suit]]
  (suit-class suit))

(defn suit-html [suit]
  (if suit
    [:span {:class (str "suit" (suit-class suit))}
     (cards/suit->str suit)]
    "--"))

(defn card-html [card]
  [:span {:class (str "card" (card-class card))}
   (cards/card->str card)])

(defn maybe-card-html [x]
  (if (and (vector? x) (= 2 (count x)))
    (card-html x)
    (str x)))

(defn compact-number [x]
  (cond
    (number? x) (format "%.3f" (double x))
    (nil? x) "--"
    :else (str x)))

(defn probability-label [x]
  (cond
    (number? x) (format "%.1f%%" (* 100.0 (double x)))
    (nil? x) "--"
    :else (str x)))

(defn cards-html [cards]
  (if (seq cards)
    (for [card cards]
      (card-html card))
    [:span {:class "empty"} "--"]))

(defn player-label [view player]
  (or (some->> (:players view)
               (filter #(= player (:id %)))
               first
               :name)
      (some-> player name)
      "--"))

(defn stat-card [label value]
  [:div {:class "stat"}
   [:span label]
   [:strong value]])

(defn table-cell [label & body]
  (into [:td {:data-label label}] body))

(defn live-room-entry? [[_ room]]
  (and (map? room)
       (:created-at room)
       (:game room)))

(defn active-room-entry? [[_ room :as entry]]
  (and (live-room-entry? entry)
       (seq (:connections room))))

(defn sorted-room-entries [rooms]
  (sort-by (comp :created-at val) (filter active-room-entry? rooms)))

(defn live-room [rooms room-id]
  (let [room (get rooms room-id)]
    (when (active-room-entry? [room-id room])
      room)))

(defn runtime-stats [{:keys [rooms
                             metrics
                             pending-bot-count
                             open-websocket-count
                             limits
                             started-at
                             now]}]
  (let [loaded-room-entries (filter live-room-entry? rooms)
        active-room-entries (filter active-room-entry? rooms)
        runtime (Runtime/getRuntime)
        used (- (.totalMemory runtime) (.freeMemory runtime))
        room-updates (get metrics :room-updates 0)
        total-ns (get metrics :room-update-total-ns 0)
        avg-update-ms (if (pos? room-updates)
                        (/ total-ns room-updates 1000000.0)
                        0.0)
        connections (reduce + (map #(count (:connections %))
                                   (map val loaded-room-entries)))]
    [{:label "Uptime" :value (duration-label (- now started-at))}
     {:label "Active rooms" :value (count active-room-entries)}
     {:label "Loaded room cache" :value (count loaded-room-entries)}
     {:label "Idle loaded" :value (- (count loaded-room-entries)
                                     (count active-room-entries))}
     {:label "Max loaded rooms" :value (or (:max-rooms limits) "--")}
     {:label "Connections" :value connections}
     {:label "Open websockets" :value (or open-websocket-count 0)}
     {:label "Max websockets" :value (or (:max-websocket-connections limits) "--")}
     {:label "Max room conns" :value (or (:max-room-connections limits) "--")}
     {:label "Max message" :value (bytes-label (or (:max-message-bytes limits) 0))}
     {:label "Idle unload" :value (duration-label (or (:idle-room-ms limits) 0))}
     {:label "Pending bot timers" :value pending-bot-count}
     {:label "Inbound messages" :value (get metrics :incoming-messages 0)}
     {:label "Inbound bytes" :value (bytes-label (get metrics :incoming-bytes 0))}
     {:label "Broadcasts" :value (get metrics :broadcasts 0)}
     {:label "State messages" :value (get metrics :state-messages 0)}
     {:label "Errors" :value (get metrics :errors 0)}
     {:label "Avg update" :value (format "%.2f ms" avg-update-ms)}
     {:label "Max update" :value (format "%.2f ms" (/ (get metrics :room-update-max-ns 0)
                                                       1000000.0))}
     {:label "Heap used" :value (bytes-label used)}
     {:label "Heap max" :value (bytes-label (.maxMemory runtime))}
     {:label "Threads" :value (.getThreadCount (ManagementFactory/getThreadMXBean))}] ))

(defn room-age [now room]
  (duration-label (- now (:created-at room))))

(defn room-idle-age [now room]
  (if (seq (:connections room))
    "--"
    (duration-label (- now (or (:empty-since room) (:created-at room))))))

(defn delete-room-control [room-id]
  [:button {:class "danger"
            :type "button"
            :data-delete-room room-id}
   "Delete"])

(defn room-summary-row [now selected-id [room-id room]]
  (let [state (:game room)
        view (game/admin-view state (:seats room))]
    [:tr {:class (when (= selected-id room-id) "selected")}
     (table-cell "Room" [:a {:href (str "/karbosh/admin?room=" room-id)} room-id])
     (table-cell "Phase" (kw-label (:phase state)))
     (table-cell "Score" (score-label (:scores state)))
     (table-cell "Current" (player-label view (:current-player state)))
     (table-cell "Hand" (inc (or (:hand-index state) 0)))
     (table-cell "Conns" (count (:connections room)))
     (table-cell "Age" (room-age now room))
     (table-cell "Idle" (room-idle-age now room))
     (table-cell "Actions" (delete-room-control room-id))]))

(defn rooms-table [rooms selected-id now]
  (if (seq (sorted-room-entries rooms))
    [:table {:class "admin-table"}
     [:thead
      [:tr
       [:th "Room"]
       [:th "Phase"]
       [:th "Score"]
       [:th "Current"]
       [:th "Hand"]
       [:th "Conns"]
       [:th "Age"]
       [:th "Idle"]
       [:th "Actions"]]]
     [:tbody (for [entry (sorted-room-entries rooms)]
               (room-summary-row now selected-id entry))]]
    [:p {:class "empty"} "No rooms are currently active."]))

(defn room-record-room [record]
  (:room record))

(defn room-hand-count [room]
  (+ (count (get-in room [:game :hand-history]))
     (reduce + (map #(count (get-in % [:game :hand-history]))
                    (:games room)))))

(defn room-played? [room]
  (pos? (room-hand-count room)))

(defn room-winner [room]
  (get-in room [:game :winner]))

(defn historical-room-records [rooms records]
  (let [live-ids (set (map first (sorted-room-entries rooms)))]
    (->> records
         (remove #(contains? live-ids (:room-id %)))
         (filter #(room-played? (:room %)))
         (sort-by :logged-at >)
         vec)))

(defn historical-stats [records]
  (let [rooms (map room-record-room records)
        games (filter #(= :game-over (get-in % [:game :phase])) rooms)
        hands (reduce + (map room-hand-count rooms))
        winners (frequencies (keep room-winner games))
        room-count (count rooms)]
    [{:label "Archived rooms" :value room-count}
     {:label "Completed games" :value (count games)}
     {:label "Total hands" :value hands}
     {:label "Avg hands" :value (if (pos? room-count)
                                  (format "%.1f" (/ (double hands) room-count))
                                  "--")}
     {:label "Team 1 wins" :value (get winners 1 0)}
     {:label "Team 2 wins" :value (get winners 2 0)}
     {:label "Latest record" :value (time-label (:logged-at (first records)))}]))

(defn bid-outcome [room hand]
  (let [bid (:bid hand)
        team (get-in room [:game :players (:player bid) :team])
        target (case (:bid-type bid)
                 :bid (:value bid)
                 (:karbosh :double-karbosh) 8
                 nil)
        taken (when team (get-in hand [:tricks team] 0))]
    (when target
      {:bid (bid-label bid)
       :target target
       :taken taken
       :made? (and taken (>= taken target))
       :margin (when taken (- taken target))})))

(defn bid-trends [records]
  (->> records
       (map room-record-room)
       (mapcat (fn [room]
                 (keep #(bid-outcome room %)
                       (get-in room [:game :hand-history]))))
       (group-by :bid)
       (map (fn [[bid outcomes]]
              (let [attempts (count outcomes)
                    made (count (filter :made? outcomes))
                    margins (keep :margin outcomes)]
                {:bid bid
                 :attempts attempts
                 :made made
                 :make-rate (if (pos? attempts)
                              (format "%.0f%%" (* 100.0 (/ made attempts)))
                              "--")
                 :avg-margin (if (seq margins)
                               (format "%.1f"
                                       (/ (double (reduce + margins))
                                          (count margins)))
                               "--")})))
       (sort-by :bid)
       vec))

(defn bid-trends-table [records]
  (let [trends (bid-trends records)]
    (if (seq trends)
      [:table
       {:class "admin-table"}
       [:thead
        [:tr
         [:th "Bid"]
         [:th "Attempts"]
         [:th "Made"]
         [:th "Make rate"]
         [:th "Avg margin"]]]
       [:tbody
        (for [{:keys [bid attempts made make-rate avg-margin]} trends]
          [:tr
           (table-cell "Bid" bid)
           (table-cell "Attempts" attempts)
           (table-cell "Made" made)
           (table-cell "Make rate" make-rate)
           (table-cell "Avg margin" avg-margin)])]]
      [:p {:class "empty"} "No completed bid history yet."])))

(defn historical-room-row [record]
  (let [room (:room record)
        state (:game room)
        room-id (:room-id record)]
    [:tr
     (table-cell "Room" [:a {:href (str "/karbosh/admin/rooms/" room-id "/snapshot")} room-id])
     (table-cell "Phase" (kw-label (:phase state)))
     (table-cell "Score" (score-label (:scores state)))
     (table-cell "Winner" (or (some-> room room-winner team-label) "--"))
     (table-cell "Hands" (room-hand-count room))
     (table-cell "Last event" (kw-label (:type record)))
     (table-cell "Last seen" (time-label (:logged-at record)))
     (table-cell "Links"
      [:a {:href (str "/karbosh/admin/rooms/" room-id "/snapshot")} "Snapshot"]
      " / "
      [:a {:href (str "/karbosh/admin/rooms/" room-id "/snapshot.edn")} "Raw"])]))

(defn historical-rooms-table [records]
  (if (seq records)
    [:table {:class "admin-table"}
     [:thead
      [:tr
       [:th "Room"]
       [:th "Phase"]
       [:th "Score"]
       [:th "Winner"]
       [:th "Hands"]
       [:th "Last event"]
       [:th "Last seen"]
       [:th "Links"]]]
     [:tbody
      (for [record records]
        (historical-room-row record))]]
    [:p {:class "empty"} "No historical room records found."]))

(defn historical-panel [rooms records]
  (let [records (historical-room-records rooms records)]
    [:section {:id "admin-history-panel" :class "panel"}
     [:div {:class "section-heading"}
      [:div
       [:p "Archive"]
       [:h2 "Historical rooms"]]
      [:div {:class "admin-actions"}
       [:a {:href "/karbosh/admin/history"} "View all history"]]]
     [:div {:class "stats room-stats"}
      (for [metric (historical-stats records)]
        (stat-card (:label metric) (:value metric)))]
     [:div {:class "two-col"}
      [:section
       [:h3 "Bid trends"]
       (bid-trends-table records)]
      [:section
       [:h3 "Browse rooms"]
       (historical-rooms-table records)]]]))

(defn historical-panel-placeholder []
  [:section {:id "admin-history-panel" :class "panel"}
   [:div {:class "section-heading"}
    [:div
     [:p "Archive"]
     [:h2 "Historical rooms"]]
    [:div {:class "admin-actions"}
     [:a {:href "/karbosh/admin/history"} "View all history"]]]
   [:p {:class "empty"}
    "Historical archive is available on the full history page."]])

(defn record-game-seed [record]
  (get-in record [:room :game :initial-seed]))

(defn record-game-timestamp [record]
  (or (get-in record [:room :game-started-at])
      (get-in record [:room :created-at])
      (:logged-at record)))

(defn record-game-key [record]
  [(:room-id record) (record-game-seed record) (record-game-timestamp record)])

(defn completed-game-record? [record]
  (= :game-over (get-in record [:room :game :phase])))

(defn latest-record-by [pred records]
  (first (sort-by :logged-at > (filter pred records))))

(defn preferred-game-record [records]
  (or (latest-record-by completed-game-record? records)
      (latest-record-by (constantly true) records)))

(defn live-room-records [rooms now]
  (->> rooms
       sorted-room-entries
       (filter (fn [[_ room]] (room-played? room)))
       (map (fn [[room-id room]]
              {:schema :karbosh.audit/v1
               :type :room-live
               :logged-at now
               :room-id room-id
               :room (-> room
                         (dissoc :connections)
                         (update :seats #(into {} %)))}))))

(defn game-history-records
  ([records]
   (game-history-records {} records))
  ([rooms records]
   (let [now (System/currentTimeMillis)]
     (->> (concat (live-room-records rooms now) records)
          (filter record-game-seed)
          (filter #(room-played? (:room %)))
          (group-by record-game-key)
          vals
          (keep preferred-game-record)
          (sort-by :logged-at >)
          vec))))

(defn game-history-stats [records]
  (let [game-records (game-history-records records)
        completed (filter completed-game-record? game-records)
        winners (frequencies (keep room-winner (map room-record-room completed)))]
    [{:label "Games" :value (count game-records)}
     {:label "Completed" :value (count completed)}
     {:label "In progress" :value (- (count game-records) (count completed))}
     {:label "Team 1 wins" :value (get winners 1 0)}
     {:label "Team 2 wins" :value (get winners 2 0)}
     {:label "Latest record" :value (time-label (:logged-at (first game-records)))}]))

(defn bot-seat-style [seat]
  (or (:style seat)
      (some-> (:persona seat) room/persona-style)
      (room/persona-style {:name (:name seat)})))

(defn bot-seat-play-strategy [seat]
  (or (:play-strategy seat)
      (some-> (:persona seat) room/persona-play-strategy)
      (room/persona-play-strategy {:name (:name seat)})))

(defn bot-seat-ditch-policy [seat]
  (or (:ditch-policy seat)
      (some-> (:persona seat) room/persona-ditch-policy)
      (room/persona-ditch-policy {:name (:name seat)})))

(defn bot-seat-name [player seat]
  (or (get-in seat [:persona :name])
      (:name seat)
      (some-> player name)
      "--"))

(defn bot-outcomes [records]
  (vec
   (for [record records
         :let [room (:room record)
               state (:game room)
               winner (:winner state)]
         :when (and (= :game-over (:phase state)) winner)
         [player seat] (:seats room)
         :let [team (get-in state [:players player :team])]
         :when (and (:bot? seat) team)]
     {:player player
      :name (bot-seat-name player seat)
      :style (bot-seat-style seat)
      :play-strategy (bot-seat-play-strategy seat)
      :ditch-policy (bot-seat-ditch-policy seat)
      :team team
      :won? (= team winner)})))

(defn percent-label [n total]
  (if (pos? total)
    (format "%.0f%%" (* 100.0 (/ n total)))
    "--"))

(defn summarize-bot-outcomes [outcomes]
  (let [appearances (count outcomes)
        wins (count (filter :won? outcomes))]
    {:appearances appearances
     :wins wins
     :win-rate (percent-label wins appearances)}))

(defn bot-outcome-groups [group-f outcomes]
  (->> outcomes
       (group-by group-f)
       (map (fn [[k outcomes]]
              (assoc (summarize-bot-outcomes outcomes) :key k)))
       (sort-by (juxt (comp - :wins)
                      (comp - :appearances)
                      (comp str :key)))
       vec))

(defn known-play-strategies []
  (->> (concat (keys bot/play-strategies)
               (keep :play-strategy room/bot-personas))
       set
       (sort-by kw-label)
       vec))

(defn known-ditch-policies []
  (->> (concat bot/ditch-policies
               (keep :ditch-policy room/bot-personas))
       set
       (sort-by kw-label)
       vec))

(defn bot-strategy-rows [outcomes]
  (let [groups (bot-outcome-groups :play-strategy outcomes)
        by-key (into {} (map (juxt :key identity) groups))]
    (mapv (fn [strategy]
            (or (get by-key strategy)
                {:key strategy
                 :appearances 0
                 :wins 0
                 :win-rate "--"}))
          (known-play-strategies))))

(defn bot-strategy-table [records]
  (let [groups (bot-strategy-rows (bot-outcomes records))]
    (if (seq groups)
      [:table {:class "admin-table"}
       [:thead
        [:tr
         [:th "Strategy"]
         [:th "Seats"]
         [:th "Wins"]
         [:th "Win rate"]]]
       [:tbody
        (for [{:keys [key appearances wins win-rate]} groups]
          [:tr
           (table-cell "Strategy" (kw-label key))
           (table-cell "Seats" appearances)
           (table-cell "Wins" wins)
           (table-cell "Win rate" win-rate)])]]
      [:p {:class "empty"} "No completed bot outcomes yet."])))

(defn bot-ditch-policy-rows [outcomes]
  (let [groups (bot-outcome-groups :ditch-policy outcomes)
        by-key (into {} (map (juxt :key identity) groups))]
    (mapv (fn [policy]
            (or (get by-key policy)
                {:key policy
                 :appearances 0
                 :wins 0
                 :win-rate "--"}))
          (known-ditch-policies))))

(defn bot-ditch-policy-table [records]
  (let [groups (bot-ditch-policy-rows (bot-outcomes records))]
    (if (seq groups)
      [:table {:class "admin-table"}
       [:thead
        [:tr
         [:th "Ditch policy"]
         [:th "Seats"]
         [:th "Wins"]
         [:th "Win rate"]]]
       [:tbody
        (for [{:keys [key appearances wins win-rate]} groups]
          [:tr
           (table-cell "Ditch policy" (kw-label key))
           (table-cell "Seats" appearances)
           (table-cell "Wins" wins)
           (table-cell "Win rate" win-rate)])]]
      [:p {:class "empty"} "No completed bot outcomes yet."])))

(defn bot-persona-table [records]
  (let [groups (bot-outcome-groups
                (juxt :name :style :play-strategy :ditch-policy)
                (bot-outcomes records))]
    (if (seq groups)
      [:table {:class "admin-table"}
       [:thead
        [:tr
         [:th "Bot"]
         [:th "Style"]
         [:th "Strategy"]
         [:th "Ditch"]
         [:th "Seats"]
         [:th "Wins"]
         [:th "Win rate"]]]
       [:tbody
        (for [{:keys [key appearances wins win-rate]} groups
              :let [[name style play-strategy ditch-policy] key]]
          [:tr
           (table-cell "Bot" name)
           (table-cell "Style" (kw-label style))
           (table-cell "Strategy" (kw-label play-strategy))
           (table-cell "Ditch" (kw-label ditch-policy))
           (table-cell "Seats" appearances)
           (table-cell "Wins" wins)
           (table-cell "Win rate" win-rate)])]]
      [:p {:class "empty"} "No completed bot outcomes yet."])))

(defn game-history-link [room-id seed timestamp suffix]
  (str "/karbosh/admin/history/" room-id "/" seed "/" timestamp "/" suffix))

(defn game-history-row [record]
  (let [room (:room record)
        state (:game room)
        room-id (:room-id record)
        seed (record-game-seed record)
        timestamp (record-game-timestamp record)]
    [:tr
     (table-cell "Room" room-id)
     (table-cell "Seed" (or (some-> seed str) "--"))
     (table-cell "Started" (time-label timestamp))
     (table-cell "Phase" (kw-label (:phase state)))
     (table-cell "Score" (score-label (:scores state)))
     (table-cell "Winner" (or (some-> room room-winner team-label) "--"))
     (table-cell "Hands" (room-hand-count room))
     (table-cell "Last event" (kw-label (:type record)))
     (table-cell "Last seen" (time-label (:logged-at record)))
     (table-cell "Links"
      [:a {:href (game-history-link room-id seed timestamp "snapshot")} "Snapshot"]
      " / "
      [:a {:href (game-history-link room-id seed timestamp "snapshot.edn")} "Raw"])]))

(defn game-history-table [records]
  (let [records (game-history-records records)]
    (if (seq records)
      [:table {:class "admin-table"}
       [:thead
        [:tr
         [:th "Room"]
         [:th "Seed"]
         [:th "Started"]
         [:th "Phase"]
         [:th "Score"]
         [:th "Winner"]
         [:th "Hands"]
         [:th "Last event"]
         [:th "Last seen"]
         [:th "Links"]]]
       [:tbody
        (for [record records]
          (game-history-row record))]]
      [:p {:class "empty"} "No game history found."])))

(defn render-history-main [{:keys [rooms records]}]
  (let [records (game-history-records rooms records)]
    [:main {:id "admin-main"}
     [:div {:class "top"}
      [:div
       [:p "Karbosh admin"]
       [:h1 "Game history"]]
      [:div {:class "admin-actions"}
       [:a {:href "/karbosh/admin"} "Dashboard"]
       [:a {:href "/karbosh/"} "Back to game"]]]
     [:section {:class "panel"}
      [:div {:class "section-heading"}
       [:div
        [:p "Archive"]
        [:h2 "All games"]]]
      [:div {:class "stats room-stats"}
       (for [metric (game-history-stats records)]
         (stat-card (:label metric) (:value metric)))]
      [:div {:class "two-col"}
       [:section
        [:h3 "Bot strategies"]
        (bot-strategy-table records)]
       [:section
        [:h3 "Bot ditch policies"]
        (bot-ditch-policy-table records)]
       [:section
        [:h3 "Bot personas"]
        (bot-persona-table records)]]
      (game-history-table records)]]))

(defn seats-table [view]
  [:table {:class "admin-table"}
   [:thead
    [:tr
     [:th "Player"]
     [:th "Team"]
     [:th "Status"]
     [:th "Cards"]]]
   [:tbody
    (for [{:keys [id team name connected? bot? active? hand-count]} (:players view)]
      (let [status (str (cond
                          bot? "bot"
                          connected? "online"
                          :else "offline")
                        (when (false? active?) " / sitting out"))]
        [:tr {:class (when (= id (:current-player view)) "selected")}
         (table-cell "Player" (or name (clojure.core/name id)))
         (table-cell "Team" (team-label team))
         (table-cell "Status" status)
         (table-cell "Cards" hand-count)]))]])

(defn trick-html [view trick]
  (if (seq trick)
    [:div {:class "trick"}
     (for [{:keys [player card]} trick]
       [:div
        [:span (player-label view player)]
        (card-html card)])]
    [:p {:class "empty"} "No cards on the table."]))

(defn hands-html [view hands]
  [:div {:class "hands"}
   (for [{:keys [id]} (:players view)]
     [:article
      [:strong (player-label view id)]
      [:div (cards-html (get hands id))]])])

(defn bids-html [view]
  (if (seq (:bids-this-hand view))
    [:ol {:class "compact-list"}
     (for [bid (:bids-this-hand view)]
       [:li
        [:span (player-label view (:player bid))]
        [:strong (bid-label bid)]])]
    [:p {:class "empty"} "No bids this hand."]))

(defn event-label [{:keys [type player card suit bid-type value]} view]
  (vec
   (concat [ (kw-label type) " / " (player-label view player)]
           (when bid-type [" / " (bid-label {:bid-type bid-type :value value})])
           (when suit [" / " (cards/suit->str suit)])
           (when card [" / " (card-html card)]))))

(defn ai-selected-html [selected]
  (let [selected (if (map? selected) selected {:value selected})
        {:keys [card score risk good? winning? trump? value]} selected]
    [:span {:class "ai-selected"}
     (cond
       card (card-html card)
       value (maybe-card-html value)
       :else "--")
     (when score
       [:span {:class "ai-pill"} (str "Score " score)])
     (when (contains? selected :risk)
       [:span {:class "ai-pill"} (str "Risk " (probability-label risk))])
     (when (contains? selected :good?)
       [:span {:class (str "ai-pill" (when good? " good"))}
        (if good? "Good" "Beatable")])
     (when winning?
       [:span {:class "ai-pill winning"} "Winning"])
     (when trump?
       [:span {:class "ai-pill trump"} "Trump"])]))

(defn ai-candidates-html [candidates]
  (when (seq candidates)
    [:section {:class "ai-candidate-panel"}
     [:div {:class "ai-subhead"} "Candidate cards"]
     [:div {:class "ai-candidates"}
      (for [{:keys [card score risk good? winning?] :as candidate} candidates]
        [:span {:class (str "ai-candidate"
                            (when good? " good")
                            (when winning? " winning"))}
         (card-html card)
         [:small
          (when score
            [:span (str "Score " score)])
          (when (contains? candidate :risk)
            [:span (str "Risk " (probability-label risk))])]])]]))

(defn ai-hypergeom-html [hypergeom]
  (when (seq hypergeom)
    [:section {:class "ai-probability-panel"}
     [:div {:class "ai-subhead"} "Card-count odds"]
     [:dl {:class "ai-hypergeom"}
      [:div
       [:dt "Can be beaten"]
       [:dd (probability-label (:prob-can-beat hypergeom))]]
      [:div
       [:dt "Any higher"]
       [:dd (probability-label (:prob-any-higher hypergeom))]]
      [:div
       [:dt "Higher follow"]
       [:dd (probability-label (:prob-higher-follow hypergeom))]]
      [:div
       [:dt "Unseen higher"]
       [:dd (str (or (:higher-unseen hypergeom) 0)
                 " total / "
                 (or (:higher-follow-unseen hypergeom) 0)
                 " follow / "
                 (or (:higher-trump-unseen hypergeom) 0)
                 " trump")]]
      (when-let [ruff (:prob-void-higher-trump-by-player hypergeom)]
        [:div
         [:dt "Ruff risk"]
         [:dd
          (if (seq ruff)
            (str/join ", "
                      (map (fn [[player p]]
                             (str (name player) " " (probability-label p)))
                           ruff))
            "--")]])]]))

(defn ai-fact-html [label value]
  [:div {:class "ai-fact"}
   [:span label]
   [:strong value]])

(defn ai-decision-html [{:keys [policy engine reason selected candidates]}]
  (when policy
    [:details {:class "ai-decision"}
     [:summary {:class "ai-decision-summary"}
      [:span {:class "ai-badge"} "AI"]
      [:span {:class "ai-summary-text"}
       [:strong (kw-label reason)]
       [:em (str (kw-label policy) " via " (kw-label engine))]]]
     [:div {:class "ai-decision-body"}
      [:div {:class "ai-facts"}
       (ai-fact-html "Selected" (ai-selected-html selected))
       (ai-fact-html "Reason" (kw-label reason))
       (ai-fact-html "Engine" (kw-label engine))]
      (ai-hypergeom-html (:hypergeom selected))
      (ai-candidates-html candidates)]]))

(defn recent-events-html [view events]
  (if (seq events)
    [:ol {:class "compact-list events"}
     (for [event (take-last 18 events)]
       (into [:li] (event-label event view)))]
    [:p {:class "empty"} "No events yet."]))

(defn hand-summary-label [{:keys [points scores-after trump tricks]}]
  (str "Trump " (or (some-> trump cards/suit->str) "--")
       " / Tricks " (score-label tricks)
       " / Points " (score-label points)
       " / Score " (score-label scores-after)))

(defn hand-history-html [history]
  (if (seq history)
    [:ol {:class "compact-list"}
     (for [{:keys [hand-index bid completed-tricks] :as hand} (take-last 10 history)]
       [:li
        [:span (str "Hand " (inc hand-index))]
        [:strong (bid-label bid)]
        [:em (str (hand-summary-label hand) " / " (count completed-tricks) " tricks")]])]
    [:p {:class "empty"} "No completed hands yet."]))

(defn hand-seed [{:keys [deals]}]
  (some-> deals first :seed))

(defn current-hand [game]
  {:hand-index (:hand-index game)
   :bid (game/current-bid game)
   :trump (:trump game)
   :initial-hands (:initial-hands game)
   :final-hands (game/player-hands game)
   :deals (:hand-deals game)
   :history (:history game)
   :completed-tricks (:completed-tricks game)
   :current-trick (:current-trick game)
   :tricks (:tricks-this-hand game)
   :scores-after (:scores game)
   :phase (:phase game)
   :current? true})

(defn playable-current-hand? [game]
  (not (contains? #{:hand-complete :game-over} (:phase game))))

(defn room-hands [room]
  (let [state (:game room)]
    (cond-> (vec (:hand-history state))
      (playable-current-hand? state)
      (conj (current-hand state)))))

(defn hand-bids [hand]
  (filter #(= :bid (:type %)) (:history hand)))

(defn trick-winner [trump trick]
  (some-> (rules/winning-play trick trump) :player))

(defn trick-card-html [view trump winner {:keys [player card ai]}]
  [:div {:class (str "trick-card"
                     (when (= winner player) " winner"))}
   [:span {:class "play-player"} (player-label view player)]
   (card-html card)
   (when (= winner player)
     [:strong "Won"])
   (ai-decision-html ai)])

(defn ai-events [hand]
  (filter :ai (:history hand)))

(defn ai-policy-summary [events]
  (->> events
       (group-by :player)
       (map (fn [[player events]]
              {:player player
               :policies (frequencies (map #(get-in % [:ai :policy]) events))
               :engines (frequencies (map #(get-in % [:ai :engine]) events))}))
       (sort-by (comp str :player))
       vec))

(defn freq-items [m]
  (->> m
       (remove (comp nil? key))
       (map (fn [[k n]]
              {:label (kw-label k)
               :count n}))
       (sort-by (juxt (comp - :count) :label))))

(defn freq-chips-html [m]
  (let [items (freq-items m)]
    (if (seq items)
      [:div {:class "ai-policy-chips"}
       (for [{:keys [label count]} items]
         [:span {:class "ai-policy-chip"}
          [:span label]
          [:strong (str "x" count)]])]
      [:span {:class "empty"} "--"])))

(defn ai-policy-summary-html [view hand]
  (let [rows (ai-policy-summary (ai-events hand))]
    (when (seq rows)
      [:section {:class "ai-policy-summary"}
       [:h3 "AI Policies"]
       [:div {:class "ai-policy-grid"}
        (for [{:keys [player policies engines]} rows]
          [:article {:class "ai-policy-card"}
           [:h4 (player-label view player)]
           [:div {:class "ai-policy-columns"}
            [:div
             [:span {:class "ai-subhead"} "Policies"]
             (freq-chips-html policies)]
            [:div
             [:span {:class "ai-subhead"} "Engines"]
             (freq-chips-html engines)]]])]])))

(defn ai-decision-row-html [view event]
  (let [{:keys [policy engine reason selected]} (:ai event)
        hypergeom (:hypergeom selected)]
    [:tr
     (table-cell "Player" (player-label view (:player event)))
     (table-cell "Event" (kw-label (:type event)))
     (table-cell "Policy" (kw-label policy))
     (table-cell "Reason" (kw-label reason))
     (table-cell "Selected" (ai-selected-html selected))
     (table-cell "P beat" (probability-label (:prob-can-beat hypergeom)))
     (table-cell "Higher unseen"
                 (if hypergeom
                   (str (or (:higher-unseen hypergeom) 0)
                        " / "
                        (or (:higher-follow-unseen hypergeom) 0)
                        " follow / "
                        (or (:higher-trump-unseen hypergeom) 0)
                        " trump")
                   "--"))
     (table-cell "Engine" (kw-label engine))]))

(defn ai-decisions-table-html [view hand]
  (let [events (ai-events hand)]
    (when (seq events)
      [:section {:class "ai-decision-table-panel"}
       [:h3 "AI Decisions"]
       [:table {:class "admin-table ai-decision-table"}
        [:thead
         [:tr
          [:th "Player"]
          [:th "Event"]
          [:th "Policy"]
          [:th "Reason"]
          [:th "Selected"]
          [:th "P beat"]
          [:th "Higher unseen"]
          [:th "Engine"]]]
        [:tbody
         (for [event events]
           (ai-decision-row-html view event))]]])))

(defn trick-anchor [index]
  (str "trick-" (inc index)))

(declare trick-analysis-url)

(defn trick-detail-html [view snapshot-base-url hand trump index trick]
  (let [winner (trick-winner trump trick)]
    [:article {:id (trick-anchor index)
               :class "trick-detail"}
     [:div {:class "trick-heading"}
      [:div
       [:strong (str "Trick " (inc index))]
       [:span (str "Winner: " (player-label view winner))]]
      [:a {:class "trick-analysis-link"
           :href (trick-analysis-url snapshot-base-url hand index)}
       "Analyze"]]
     [:div {:class "trick"}
      (for [play trick]
        (trick-card-html view trump winner play))]]))

(defn current-trick-detail-html [view trump trick]
  (when (seq trick)
    [:article {:id "current-trick"
               :class "trick-detail current-trick-detail"}
     [:div {:class "trick-heading"}
      [:strong "Current trick"]
      [:span "In progress"]]
     [:div {:class "trick"}
      (for [play trick]
        (trick-card-html view trump nil play))]]))

(defn bids-detail-html [view hand]
  (if (seq (hand-bids hand))
    [:ol {:class "play-list"}
     (for [bid (hand-bids hand)]
       [:li
        [:span {:class "event-kind"} "Bid"]
        [:strong (player-label view (:player bid))]
        [:span (bid-label bid)]])]
    [:p {:class "empty"} "No bids recorded."]))

(defn sorted-cards-html [cards trump]
  (cards-html (hand-order/sorted-hand cards trump)))

(defn team-row-players [view]
  (->> (:players view)
       (sort-by (juxt :team :id))))

(defn compact-starting-hands-html [view hands trump]
  (if (seq hands)
    [:div {:class "starting-hands-strip"}
     (for [{:keys [id team]} (team-row-players view)]
       [:div {:class (str "starting-hand-row team-" team)}
        [:h4 (player-label view id)]
        [:div {:class "starting-hand-cards"}
         (sorted-cards-html (get hands id) trump)]])]
    [:p {:class "empty"} "No starting hands recorded."]))

(defn initial-hands-html [view hands]
  [:details {:class "initial-hands"}
   [:summary "Initial hands"]
   (hands-html view hands)])

(defn hand-title [hand]
  (str "Hand " (inc (:hand-index hand))
       (when (:current? hand) " (current)")))

(defn hand-stats-html [hand]
  [:div {:class "stats room-stats"}
   (stat-card "Bid" (bid-label (:bid hand)))
   (stat-card "Trump" (suit-html (:trump hand)))
   (stat-card "Tricks" (score-label (:tricks hand)))
   (stat-card "Points" (score-label (:points hand)))
   (stat-card "Score" (score-label (:scores-after hand)))
   (stat-card "Seed" (or (some-> hand hand-seed str) "--"))])

(defn hand-detail-url [snapshot-base-url hand]
  (str snapshot-base-url "/hands/" (:hand-index hand)))

(defn trick-detail-url [snapshot-base-url hand index]
  (str (hand-detail-url snapshot-base-url hand) "#" (trick-anchor index)))

(defn trick-analysis-url [snapshot-base-url hand index]
  (str (hand-detail-url snapshot-base-url hand) "/tricks/" index "/analysis"))

(defn current-trick-detail-url [snapshot-base-url hand]
  (str (hand-detail-url snapshot-base-url hand) "#current-trick"))

(defn compact-trick-link-html [view snapshot-base-url hand index trick]
  (let [winner (trick-winner (:trump hand) trick)]
    [:a {:class "trick-chip"
         :href (trick-detail-url snapshot-base-url hand index)}
     [:strong (str "T" (inc index))]
     [:span (player-label view winner)]]))

(defn compact-trick-links-html [view snapshot-base-url hand]
  (let [completed (:completed-tricks hand)
        current (:current-trick hand)]
    (when (or (seq completed) (seq current))
      [:div {:class "trick-chip-list"}
       [:span {:class "trick-chip-label"} "Tricks"]
       (for [[index trick] (map-indexed vector completed)]
         (compact-trick-link-html view snapshot-base-url hand index trick))
       (when (seq current)
         [:a {:class "trick-chip current"
              :href (current-trick-detail-url snapshot-base-url hand)}
          [:strong "Live"]
          [:span (str (count current) " played")]])])))

(defn hand-summary-row-html [snapshot-base-url hand]
  [:div {:class "hand-summary-row"}
   [:span {:class "hand-summary-title"} (hand-title hand)]
   [:span (bid-label (:bid hand))]
   [:span (or (suit-html (:trump hand)) "--")]
   [:span (str "Tricks " (score-label (:tricks hand)))]
   [:span (str "Points " (score-label (:points hand)))]
   [:span (str "Score " (score-label (:scores-after hand)))]
   [:a {:class "hand-explain-link"
        :href (hand-detail-url snapshot-base-url hand)}
    "Explain"]])

(defn hand-summary-card-html [view snapshot-base-url hand]
  [:article {:class "hand-summary-card"}
   (hand-summary-row-html snapshot-base-url hand)
   [:div {:class "hand-summary-hands"}
    [:div {:class "hand-summary-subhead"} "Starting hands"]
    (compact-starting-hands-html view (:initial-hands hand) (:trump hand))]
   (compact-trick-links-html view snapshot-base-url hand)])

(defn hand-summary-list-html [view snapshot-base-url hands]
  [:section {:class "panel hand-summary-panel"}
   [:div {:class "section-heading"}
    [:div
     [:p "Hands"]
     [:h2 "Hand history"]]]
   (if (seq hands)
     [:div {:class "hand-summary-list"}
      (for [hand hands]
        (hand-summary-card-html view snapshot-base-url hand))]
     [:p {:class "empty"} "No hands recorded."])])

(defn hand-play-by-play-html [view snapshot-base-url hand]
  [:section {:class "panel hand-detail"}
   [:div {:class "section-heading"}
    [:div
     [:p (if (:current? hand) "Live hand" "Completed hand")]
     [:h2 (hand-title hand)]]]
   (hand-stats-html hand)
   (ai-policy-summary-html view hand)
   (ai-decisions-table-html view hand)
   [:div {:class "two-col"}
    [:section
     [:h3 "Bidding"]
     (bids-detail-html view hand)]
    [:section
     [:h3 "Starting Hands"]
     (compact-starting-hands-html view (:initial-hands hand) (:trump hand))]]
   [:h3 "Play by Play"]
   (if (or (seq (:completed-tricks hand))
           (seq (:current-trick hand)))
     [:div {:class "trick-timeline"}
      (for [[index trick] (map-indexed vector (:completed-tricks hand))]
        (trick-detail-html view snapshot-base-url hand (:trump hand) index trick))
      (current-trick-detail-html view (:trump hand) (:current-trick hand))]
     [:p {:class "empty"} "No cards have been played."])
   (initial-hands-html view (:initial-hands hand))])

(defn room-hand [room hand-index]
  (first (filter #(= hand-index (:hand-index %)) (room-hands room))))

(defn room-hand-detail-main [room hand-index snapshot-base-url]
  (let [state (:game room)
        view (game/admin-view state (:seats room))]
    [:main {:id "admin-main"}
     [:div {:class "top"}
      [:div
       [:p "Karbosh hand detail"]
       [:h1 (str "Room " (:id room) " Hand " (inc hand-index))]]
      [:div {:class "admin-actions"}
       [:a {:href snapshot-base-url} "Room snapshot"]
       [:a {:href (str snapshot-base-url ".edn")} "Raw EDN"]
       [:a {:href "/karbosh/"} "Back to game"]]]
     (if-let [hand (room-hand room hand-index)]
       (hand-play-by-play-html view snapshot-base-url hand)
       [:section {:class "panel"}
        [:p {:class "empty"} "Hand not found."]])]))

(declare styles admin-layout-styles admin-card-styles snapshot-styles)

(defn rate-label [n total]
  (if (pos? (or total 0))
    (format "%.1f%%" (* 100.0 (/ (double n) total)))
    "--"))

(defn winner-count-label [view winners]
  (let [[winner n] (first (sort-by (comp - val) winners))]
    (if winner
      (str (player-label view winner) " x" n)
      "--")))

(defn result-by-card [results]
  (into {} (map (juxt :card identity) results)))

(defn probability-by-card [probabilities]
  (into {} (map (juxt :card identity) probabilities)))

(defn played-cards-html [view trick winner]
  [:div {:class "analysis-trick"}
   (for [{:keys [player card]} trick]
     [:div {:class (str "analysis-play"
                        (when (= player winner) " winner"))}
      [:span (player-label view player)]
      (card-html card)
      (when (= player winner)
        [:strong "Won"])])])

(defn known-result-row-html [view {:keys [card winner winner-team team-wins? trick]}]
  [:tr
   (table-cell "Lead" (card-html card))
   (table-cell "Winner" (player-label view winner))
   (table-cell "Team" (team-label winner-team))
   (table-cell "Actor team?" (if team-wins? "Yes" "No"))
   (table-cell "Policy trick"
               [:div {:class "analysis-mini-trick"}
                (for [{:keys [card]} trick]
                  (card-html card))])])

(defn monte-carlo-row-html [view known-by-card probability-by-card {:keys [card samples team-wins actor-wins winners]}]
  (let [known (get known-by-card card)
        probability (get probability-by-card card)
        hypergeom (get-in probability [:hypergeom])]
    [:tr
     (table-cell "Card" (card-html card))
     (table-cell "Known result"
                 (str (player-label view (:winner known))
                      " / "
                      (team-label (:winner-team known))))
     (table-cell "Team wins" (rate-label team-wins samples))
     (table-cell "Actor wins" (rate-label actor-wins samples))
     (table-cell "Top winner" (winner-count-label view winners))
     (table-cell "P beat" (probability-label (:prob-can-beat hypergeom)))
     (table-cell "Higher unseen"
                 (if hypergeom
                   (str (or (:higher-unseen hypergeom) 0)
                        " / "
                        (or (:higher-follow-unseen hypergeom) 0)
                        " follow / "
                        (or (:higher-trump-unseen hypergeom) 0)
                        " trump")
                   "--"))]))

(defn monte-carlo-table-html [view analysis]
  (let [known-by-card (result-by-card (:known-results analysis))
        probability-by-card (probability-by-card (:probabilities analysis))
        results (->> (get-in analysis [:monte-carlo :results])
                     vals
                     (sort-by (fn [{:keys [samples team-wins]}]
                                (if (pos? samples)
                                  (- (/ (double team-wins) samples))
                                  0))))]
    [:table {:class "admin-table analysis-table"}
     [:thead
      [:tr
       [:th "Card"]
       [:th "Known result"]
       [:th "Team wins"]
       [:th "Actor wins"]
       [:th "Top winner"]
       [:th "P beat"]
       [:th "Higher unseen"]]]
     [:tbody
      (for [result results]
        (monte-carlo-row-html view known-by-card probability-by-card result))]]))

(defn known-results-table-html [view results]
  [:table {:class "admin-table analysis-table"}
   [:thead
    [:tr
     [:th "Lead"]
     [:th "Winner"]
     [:th "Team"]
     [:th "Actor team?"]
     [:th "Policy trick"]]]
   [:tbody
    (for [result results]
      (known-result-row-html view result))]])

(defn analysis-console-form-html [analysis]
  (let [{:keys [requested-samples seed]} (:monte-carlo analysis)]
    [:form {:class "analysis-console-form" :method "get"}
     [:label
      [:span "Samples"]
      [:input {:type "number"
               :name "samples"
               :min "1"
               :max (str trick-lab/max-samples)
               :value (str requested-samples)}]]
     [:label
      [:span "Seed"]
      [:input {:type "number"
               :name "seed"
               :value (str seed)}]]
     [:button {:type "submit"} "Run"]]))

(defn analysis-console-main [room hand-index trick-index snapshot-base-url options]
  (let [state (:game room)
        view (game/admin-view state (:seats room))]
    [:main {:id "admin-main"}
     [:div {:class "top"}
      [:div
       [:p "Karbosh trick lab"]
       [:h1 (str "Room " (:id room)
                 " Hand " (inc hand-index)
                 " Trick " (inc trick-index))]]
      [:div {:class "admin-actions"}
       [:a {:href (hand-detail-url snapshot-base-url {:hand-index hand-index})}
        "Hand detail"]
       [:a {:href snapshot-base-url} "Room snapshot"]
       [:a {:href (str snapshot-base-url ".edn")} "Raw EDN"]]]
     (try
       (let [{:keys [actor actual trump monte-carlo] :as analysis}
             (trick-lab/analyze room hand-index trick-index options)]
         (list
          [:section {:class "panel analysis-console"}
           [:div {:class "section-heading"}
            [:div
             [:p "Console"]
             [:h2 "Counterfactual trick analysis"]]]
           (analysis-console-form-html analysis)
           [:div {:class "stats room-stats"}
            (stat-card "Actor" (player-label view actor))
            (stat-card "Actor team" (team-label (:actor-team analysis)))
            (stat-card "Trump" (suit-html trump))
            (stat-card "Actual lead" (card-html (:card actual)))
            (stat-card "Actual winner" (player-label view (:winner actual)))
            (stat-card "Samples" (str (:accepted-samples monte-carlo)
                                      " / "
                                      (:requested-samples monte-carlo)))
            (stat-card "Attempts" (str (:attempts monte-carlo)))]
           [:h3 "Actual trick"]
           (played-cards-html view (:trick actual) (:winner actual))]
          [:section {:class "panel analysis-console"}
           [:div {:class "section-heading"}
            [:div
             [:p "Known cards"]
             [:h2 "Policy finish from exact hands"]]]
           (known-results-table-html view (:known-results analysis))]
          [:section {:class "panel analysis-console"}
           [:div {:class "section-heading"}
            [:div
             [:p "Hidden worlds"]
             [:h2 "Monte Carlo by legal lead"]]]
           (monte-carlo-table-html view analysis)]))
       (catch Exception e
         [:section {:class "panel"}
          [:p {:class "empty"} (.getMessage e)]]))]))

(defn render-trick-analysis [room hand-index trick-index snapshot-base-url options]
  (str
   "<!doctype html>"
   (h/render
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
      [:title (str "Karbosh Trick Lab " (:id room))]
      [:style (str styles admin-layout-styles admin-card-styles snapshot-styles)]]
     [:body
      (analysis-console-main room
                             hand-index
                             trick-index
                             snapshot-base-url
                             options)]])))

(defn room-snapshot-main [room snapshot-base-url]
  (let [state (:game room)
        view (game/admin-view state (:seats room))
        hands (room-hands room)]
    [:main {:id "admin-main"}
     [:div {:class "top"}
      [:div
       [:p "Karbosh admin"]
       [:h1 (str "Room " (:id room) " History")]]
      [:div {:class "admin-actions"}
       [:a {:href (str "/karbosh/admin?room=" (:id room))} "Dashboard"]
       [:a {:href (str "/karbosh/admin/rooms/" (:id room) "/snapshot.edn")}
        "Raw EDN"]
       [:a {:href "/karbosh/"} "Back to game"]]]
     [:section {:class "panel"}
      [:div {:class "section-heading"}
       [:div
        [:p "Snapshot"]
        [:h2 "Room state"]]]
      [:div {:class "stats room-stats"}
       (stat-card "Phase" (kw-label (:phase state)))
       (stat-card "Score" (score-label (:scores state)))
       (stat-card "Current" (player-label view (:current-player state)))
       (stat-card "Hand" (str (inc (or (:hand-index state) 0))))
       (stat-card "Initial seed" (or (some-> (:initial-seed state) str) "--"))
       (stat-card "Room seed" (or (some-> (:seed room) str) "--"))]
      [:h3 "Seats"]
      (seats-table view)]
     (hand-summary-list-html view snapshot-base-url hands)]))

(def snapshot-styles
  (str
   ".play-list{margin:0;padding-left:0;list-style:none}"
   ".play-list li,.play-line{display:flex;gap:10px;align-items:center;border-bottom:1px solid rgba(255,255,255,.08);margin:0;padding:7px 0}"
   ".event-kind{min-width:74px;color:rgba(255,255,255,.48);font-size:.68rem;font-weight:700;letter-spacing:.12em;text-transform:uppercase}"
   ".hand-detail h3{color:white;margin:18px 0 8px}"
   ".trick-timeline{display:grid;gap:12px}"
   ".trick-detail{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.13);padding:10px}"
   ".trick-detail .trick{display:grid;grid-template-columns:repeat(auto-fit,minmax(138px,1fr));gap:8px;align-items:stretch}"
   ".trick-detail .trick-card{display:flex;flex-direction:column;width:auto;min-width:0;padding:8px}"
   ".trick-heading{display:flex;justify-content:space-between;gap:16px;align-items:center;margin-bottom:8px}"
   ".trick-heading strong{color:white}"
   ".trick-heading span{color:rgba(255,255,255,.55);font-size:.78rem;font-weight:700}"
   ".trick-card{position:relative}"
   ".trick-card.winner{border-color:rgba(245,200,91,.65);background:rgba(245,200,91,.12)}"
   ".trick-card .play-player{color:rgba(255,255,255,.68);font-size:.72rem;font-weight:800;line-height:1.1;margin-bottom:5px}"
   ".trick-card strong{display:block;color:#f5c85b;font-size:.58rem;letter-spacing:.1em;line-height:1.1;margin-top:4px;text-transform:uppercase}"
   ".ai-policy-summary{margin:0 0 12px}"
   ".ai-policy-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:7px}"
   ".ai-policy-card{border:1px solid rgba(255,255,255,.1);border-radius:7px;background:rgba(0,0,0,.13);padding:8px;min-width:0}"
   ".ai-policy-card h4{margin:0 0 7px;color:white;font-size:.72rem;font-weight:900;line-height:1.1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
   ".ai-policy-columns{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:7px;min-width:0}"
   ".ai-policy-chips{display:flex;flex-wrap:wrap;gap:4px;margin-top:4px;min-width:0}"
   ".ai-policy-chip{display:inline-flex;align-items:center;gap:4px;max-width:100%;border:1px solid rgba(255,255,255,.1);border-radius:999px;background:rgba(255,255,255,.045);color:rgba(255,255,255,.76);font-size:.58rem;font-weight:800;line-height:1.05;padding:4px 6px}"
   ".ai-policy-chip span{min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
   ".ai-policy-chip strong{color:#f5c85b;font-size:.54rem;letter-spacing:.04em}"
   ".ai-decision{margin-top:7px;border:1px solid rgba(111,208,199,.16);border-radius:6px;background:rgba(10,16,28,.52);overflow:hidden}"
   ".ai-decision[open]{border-color:rgba(111,208,199,.36);box-shadow:0 0 0 1px rgba(111,208,199,.05)}"
   ".ai-decision summary{cursor:pointer;list-style:none;color:rgba(255,255,255,.74);font-size:.62rem;font-weight:700;line-height:1.2}"
   ".ai-decision summary::-webkit-details-marker{display:none}"
   ".ai-decision-summary{display:flex;align-items:center;gap:6px;padding:6px}"
   ".ai-badge{display:inline-flex;align-items:center;justify-content:center;min-width:26px;border-radius:999px;background:rgba(111,208,199,.16);color:#6fd0c7;font-size:.55rem;font-weight:900;letter-spacing:.08em}"
   ".ai-summary-text{display:grid;gap:1px;min-width:0}"
   ".ai-summary-text strong{color:#f5c85b;font-size:.64rem;letter-spacing:0;text-transform:none;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
   ".ai-summary-text em{color:rgba(255,255,255,.5);font-size:.56rem;font-style:normal;font-weight:700;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
   ".ai-decision-body{display:grid;gap:7px;border-top:1px solid rgba(255,255,255,.08);padding:7px;color:rgba(255,255,255,.72);font-size:.66rem;line-height:1.25}"
   ".ai-facts{display:grid;grid-template-columns:repeat(auto-fit,minmax(92px,1fr));gap:5px}"
   ".ai-fact{border:1px solid rgba(255,255,255,.08);border-radius:5px;background:rgba(255,255,255,.035);padding:5px;min-width:0}"
   ".ai-fact>span,.ai-subhead{display:block;color:rgba(255,255,255,.45);font-size:.52rem;font-weight:800;letter-spacing:.08em;line-height:1;text-transform:uppercase}"
   ".ai-fact>strong{display:block;color:white;font-size:.66rem;font-weight:800;margin-top:4px;min-width:0;overflow-wrap:anywhere}"
   ".ai-selected{display:inline-flex;flex-wrap:wrap;gap:4px;align-items:center}"
   ".ai-selected .card{width:24px;min-width:24px;height:32px;margin:0;border-radius:4px;font-size:.68rem}"
   ".ai-pill{display:inline-flex;align-items:center;border:1px solid rgba(255,255,255,.1);border-radius:999px;background:rgba(255,255,255,.05);color:rgba(255,255,255,.72);font-size:.55rem;font-weight:800;line-height:1;padding:3px 5px}"
   ".ai-pill.good,.ai-candidate.good{border-color:rgba(111,208,199,.42);background:rgba(111,208,199,.1);color:#bdf4ef}"
   ".ai-pill.winning,.ai-candidate.winning{border-color:rgba(245,200,91,.42);background:rgba(245,200,91,.1);color:#f8d778}"
   ".ai-pill.trump{border-color:rgba(255,125,139,.4);background:rgba(255,125,139,.1);color:#ffb8c1}"
   ".ai-probability-panel,.ai-candidate-panel{display:grid;gap:5px;min-width:0}"
   ".ai-candidates{display:flex;flex-wrap:wrap;gap:4px;margin-top:1px}"
   ".ai-candidate{display:inline-flex;align-items:center;gap:4px;border:1px solid rgba(255,255,255,.1);border-radius:6px;padding:3px;background:rgba(255,255,255,.04);min-width:0}"
   ".ai-candidate .card{width:20px;min-width:20px;height:28px;margin:0;border-radius:4px;font-size:.62rem}"
   ".ai-candidate small{display:grid;gap:1px;color:rgba(255,255,255,.58);font-size:.54rem;font-weight:700;line-height:1.1;white-space:nowrap}"
   ".ai-hypergeom{display:grid;grid-template-columns:repeat(auto-fit,minmax(96px,1fr));gap:4px;margin:0;padding:0}"
   ".ai-hypergeom div{border:1px solid rgba(255,255,255,.08);border-radius:5px;padding:5px;background:rgba(255,255,255,.035);min-width:0}"
   ".ai-hypergeom dt{color:rgba(255,255,255,.45);font-size:.52rem;font-weight:800;letter-spacing:.06em;line-height:1;text-transform:uppercase}"
   ".ai-hypergeom dd{margin:4px 0 0;color:white;font-size:.66rem;font-weight:800;overflow-wrap:anywhere}"
   ".ai-decision-table-panel{margin:0 0 14px;overflow-x:auto}"
   ".ai-decision-table .ai-selected .card{width:24px;min-width:24px;height:32px;margin:0;border-radius:4px;font-size:.68rem}"
   ".hand-summary-panel{overflow:hidden}"
   ".hand-summary-list{display:grid;gap:10px}"
   ".hand-summary-card{border:1px solid rgba(255,255,255,.1);border-radius:7px;background:rgba(0,0,0,.12);padding:8px}"
   ".hand-summary-row{display:grid;grid-template-columns:minmax(74px,1.1fr) minmax(66px,.8fr) minmax(28px,.35fr) minmax(84px,.9fr) minmax(86px,.9fr) minmax(84px,.9fr) minmax(58px,.55fr);gap:8px;align-items:center;color:rgba(255,255,255,.76);font-size:.82rem;line-height:1.1;white-space:nowrap}"
   ".hand-summary-card:hover{background:rgba(111,208,199,.06)}"
   ".hand-summary-title{color:white;font-weight:800}"
   ".hand-summary-row .suit{font-size:1rem}"
   ".hand-explain-link{color:#6fd0c7;font-size:.66rem;font-weight:800;letter-spacing:.08em;text-align:right;text-transform:uppercase}"
   ".hand-summary-hands{margin-top:7px}"
   ".hand-summary-subhead,.trick-chip-label{color:rgba(255,255,255,.42);font-size:.56rem;font-weight:800;letter-spacing:.1em;text-transform:uppercase}"
   ".hand-summary-subhead{margin-bottom:4px}"
   ".hand-summary-card .starting-hands-strip,.hand-detail .starting-hands-strip{grid-template-columns:repeat(3,minmax(0,1fr));gap:8px 10px}"
   ".starting-hand-row{display:block;border-bottom:1px solid rgba(255,255,255,.08);padding:3px 0;min-width:0}"
   ".starting-hand-row h4{margin:0 0 3px;color:rgba(255,255,255,.62);font-size:.62rem;font-weight:800;letter-spacing:.08em;line-height:1;text-transform:uppercase;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
   ".starting-hand-cards{display:flex;flex-wrap:nowrap;gap:2px;min-width:0}"
   ".hand-summary-card .starting-hands-strip .card{width:24px;min-width:24px;height:32px;border-radius:4px;font-size:.68rem}"
   ".trick-chip-list{display:flex;flex-wrap:wrap;gap:5px;margin-top:7px}"
   ".trick-chip-label{display:inline-flex;align-items:center;padding:0 2px}"
   ".trick-chip{display:inline-flex;align-items:center;gap:4px;border:1px solid rgba(255,255,255,.13);border-radius:5px;background:rgba(255,255,255,.04);color:rgba(255,255,255,.68);font-size:.66rem;line-height:1;padding:5px 6px;text-decoration:none}"
   ".trick-chip:hover{border-color:rgba(111,208,199,.45);background:rgba(111,208,199,.1)}"
   ".trick-chip strong{color:#f5c85b;font-size:.62rem;letter-spacing:.06em;text-transform:uppercase}"
   ".trick-chip.current strong{color:#6fd0c7}"
   ".trick-analysis-link{display:inline-flex;align-items:center;border:1px solid rgba(111,208,199,.35);border-radius:999px;background:rgba(111,208,199,.09);color:#6fd0c7;font-size:.58rem;font-weight:900;letter-spacing:.08em;line-height:1;padding:5px 8px;text-decoration:none;text-transform:uppercase}"
   ".trick-analysis-link:hover{background:rgba(111,208,199,.16);border-color:rgba(111,208,199,.55)}"
   ".analysis-console{overflow-x:auto}"
   ".analysis-console-form{display:flex;flex-wrap:wrap;gap:8px;align-items:end;margin:0 0 12px}"
   ".analysis-console-form label{display:grid;gap:4px;color:rgba(255,255,255,.55);font-size:.62rem;font-weight:800;letter-spacing:.08em;text-transform:uppercase}"
   ".analysis-console-form input{width:118px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:rgba(0,0,0,.18);color:white;font:inherit;font-size:.78rem;line-height:1;padding:7px}"
   ".analysis-console-form button{border:1px solid rgba(111,208,199,.42);border-radius:6px;background:rgba(111,208,199,.14);color:#bdf4ef;font-size:.68rem;font-weight:900;letter-spacing:.08em;line-height:1;padding:9px 12px;text-transform:uppercase}"
   ".analysis-trick,.analysis-mini-trick{display:flex;flex-wrap:wrap;gap:6px;align-items:center}"
   ".analysis-play{display:grid;grid-template-columns:minmax(7.2rem,1fr) auto auto;gap:8px;align-items:center;min-width:10.5rem;border:1px solid rgba(255,255,255,.1);border-radius:7px;background:rgba(255,255,255,.04);padding:7px}"
   ".analysis-play.winner{border-color:rgba(245,200,91,.5);background:rgba(245,200,91,.1)}"
   ".analysis-play>span{color:rgba(255,255,255,.68);font-size:.64rem;font-weight:800;line-height:1.1}"
   ".analysis-play strong{color:#f5c85b;font-size:.54rem;letter-spacing:.08em;text-transform:uppercase}"
   ".analysis-console .stat .card,.analysis-play .card,.analysis-table .card,.analysis-mini-trick .card{background:#f8f5ed;color:#141821}"
   ".analysis-console .stat .card.heart,.analysis-console .stat .card.diamond,.analysis-play .card.heart,.analysis-play .card.diamond,.analysis-table .card.heart,.analysis-table .card.diamond,.analysis-mini-trick .card.heart,.analysis-mini-trick .card.diamond{color:#c62f43}"
   ".analysis-console .stat .card,.analysis-play .card{width:34px;min-width:34px;height:46px;margin:0;border-radius:5px;font-size:.88rem}"
   ".analysis-table .card,.analysis-mini-trick .card{width:30px;min-width:30px;height:40px;margin:0;border-radius:5px;font-size:.8rem}"
   ".starting-hands-strip{display:grid;gap:7px}"
   ".starting-hands-strip .card{width:28px;min-width:28px;height:38px;margin:0;padding:0;border-radius:4px;font-size:.78rem}"
   ".starting-hands-strip .empty{font-size:.72rem}"
   ".initial-hands{margin-top:14px}"
   ".initial-hands summary{cursor:pointer;color:#6fd0c7;font-weight:700;margin-bottom:10px}"
   "@media(max-width:720px){.play-list li,.play-line{align-items:flex-start;flex-direction:column;gap:4px}.trick-heading{align-items:flex-start;flex-direction:column;gap:4px}.trick-detail{padding:7px}.trick-detail .trick{grid-template-columns:repeat(2,minmax(0,1fr));gap:5px}.trick-detail .trick-card{padding:5px}.trick-card .play-player{font-size:.6rem;margin-bottom:3px}.trick-card strong{font-size:.48rem}.trick-analysis-link{font-size:.5rem;padding:4px 6px}.analysis-console-form{gap:6px}.analysis-console-form input{width:88px;padding:6px}.analysis-console-form button{padding:8px 10px}.analysis-play{grid-template-columns:minmax(54px,1fr) auto;gap:4px}.analysis-play strong{grid-column:1/-1}.analysis-table{font-size:clamp(.48rem,1.65vw,.62rem)}.analysis-table .card,.analysis-mini-trick .card{width:20px;min-width:20px;height:27px;font-size:.56rem}.ai-policy-grid{grid-template-columns:1fr;gap:5px}.ai-policy-card{padding:6px}.ai-policy-card h4{font-size:.62rem;margin-bottom:5px}.ai-policy-columns{gap:5px}.ai-policy-chip{font-size:.5rem;padding:3px 5px}.ai-policy-chip strong{font-size:.48rem}.ai-decision{margin-top:5px;border-radius:5px}.ai-decision-summary{gap:4px;padding:5px}.ai-badge{min-width:22px;font-size:.46rem}.ai-summary-text strong{font-size:.55rem}.ai-summary-text em{font-size:.48rem}.ai-decision-body{gap:5px;padding:5px;font-size:.56rem}.ai-facts{grid-template-columns:1fr;gap:4px}.ai-fact,.ai-hypergeom div{padding:4px}.ai-fact>span,.ai-subhead,.ai-hypergeom dt{font-size:.45rem;letter-spacing:.04em}.ai-fact>strong,.ai-hypergeom dd{font-size:.56rem}.ai-selected{gap:3px}.ai-selected .card{width:20px;min-width:20px;height:28px;font-size:.58rem}.ai-pill{font-size:.46rem;padding:2px 4px}.ai-hypergeom{grid-template-columns:repeat(2,minmax(0,1fr));gap:3px}.ai-candidates{gap:3px}.ai-candidate{gap:3px;padding:2px}.ai-candidate .card{width:18px;min-width:18px;height:25px;font-size:.54rem}.ai-candidate small{font-size:.46rem}.hand-summary-list{gap:7px}.hand-summary-card{padding:6px}.hand-summary-row{grid-template-columns:minmax(38px,.8fr) minmax(42px,.7fr) minmax(20px,.3fr) minmax(54px,.8fr) minmax(56px,.8fr) minmax(54px,.8fr) minmax(42px,.5fr);gap:3px;font-size:clamp(.46rem,1.85vw,.64rem);line-height:1.05}.hand-summary-row .suit{font-size:.76rem}.hand-explain-link{font-size:clamp(.42rem,1.55vw,.55rem);letter-spacing:.03em}.hand-summary-card .starting-hands-strip,.hand-detail .starting-hands-strip{grid-template-columns:repeat(2,minmax(0,1fr));gap:4px}.starting-hand-row{padding:2px 0}.starting-hand-row h4{font-size:.48rem;margin-bottom:2px;letter-spacing:.04em}.hand-summary-card .starting-hands-strip .card,.hand-detail .starting-hands-strip .card{width:16px;min-width:16px;height:22px;font-size:.48rem}.trick-chip-list{gap:3px;margin-top:5px}.trick-chip{font-size:.54rem;padding:4px}.trick-chip strong{font-size:.5rem}.ai-decision-table{font-size:clamp(.48rem,1.6vw,.62rem)}}"))

(declare styles admin-layout-styles admin-card-styles)

(defn render-room-snapshot
  ([room]
   (render-room-snapshot room (str "/karbosh/admin/rooms/" (:id room) "/snapshot")))
  ([room snapshot-base-url]
   (str
    "<!doctype html>"
    (h/render
     [:html {:lang "en"}
      [:head
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
       [:title (str "Karbosh Room " (:id room) " History")]
       [:style (str styles admin-layout-styles admin-card-styles snapshot-styles)]]
      [:body
       (room-snapshot-main room snapshot-base-url)]]))))

(defn render-room-hand-detail [room hand-index snapshot-base-url]
  (str
   "<!doctype html>"
   (h/render
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
      [:title (str "Karbosh Room " (:id room) " Hand " (inc hand-index))]
      [:style (str styles admin-layout-styles admin-card-styles snapshot-styles)]]
     [:body
      (room-hand-detail-main room hand-index snapshot-base-url)]])))

(defn selected-room [rooms selected-room-id]
  (or (live-room rooms selected-room-id)
      (some->> (sorted-room-entries rooms) first val)))

(defn choose-selected-room-id [rooms requested-room-id]
  (or (when (live-room rooms requested-room-id) requested-room-id)
      (some->> (sorted-room-entries rooms) first key)))

(defn room-detail [room]
  (when room
    (let [view (game/admin-view (:game room) (:seats room))
          debug (:debug view)
          last-trick (peek (:completed-tricks view))]
      [:section {:id "admin-room-detail" :class "panel detail"}
       [:div {:class "section-heading"}
        [:div
         [:p "Selected Room"]
         [:h2 (:id room)]]
        [:div {:class "admin-actions"}
         [:a {:href "/karbosh/admin"} "All rooms"]
         [:a {:href (str "/karbosh/admin/rooms/" (:id room) "/snapshot")}
          "Snapshot"]
         (delete-room-control (:id room))]]
       [:div {:class "stats room-stats"}
        (stat-card "Phase" (kw-label (:phase view)))
        (stat-card "Score" (score-label (:scores view)))
        (stat-card "Current" (player-label view (:current-player view)))
        (stat-card "Bid" (bid-label (:current-bid view)))
        (stat-card "Trump" (suit-html (:trump view)))
        (stat-card "Tricks" (score-label (:tricks-this-hand view)))]
       [:h3 "Seats"]
       (seats-table view)
       [:h3 "Table"]
       (trick-html view (:current-trick view))
       [:h3 "Last completed trick"]
       (trick-html view last-trick)
       [:h3 "Hands"]
       (hands-html view (:hands debug))
       [:div {:class "two-col"}
        [:section
         [:h3 "Bids"]
         (bids-html view)]
        [:section
         [:h3 "Recent events"]
         (recent-events-html view (:history debug))]]
       [:h3 "Hand history"]
       (hand-history-html (:hand-history debug))])))

(defn render-dashboard-main [{:keys [rooms
                                     selected-room-id
                                     historical-room-records
                                     include-historical?
                                     metrics
                                     pending-bot-count
                                     open-websocket-count
                                     limits
                                     started-at]}]
  (let [now (System/currentTimeMillis)
        selected-id (choose-selected-room-id rooms selected-room-id)
        room (selected-room rooms selected-id)
        stats (runtime-stats {:rooms rooms
                              :metrics metrics
                              :pending-bot-count pending-bot-count
                              :open-websocket-count open-websocket-count
                              :limits limits
                              :started-at started-at
                              :now now})]
    [:main {:id "admin-main"}
     [:div {:class "top"}
      [:div
       [:p "Karbosh admin"]
       [:h1 "Runtime dashboard"]]
      [:a {:href "/karbosh/"} "Back to game"]]

     [:section {:id "admin-stats-panel" :class "panel"}
      [:div {:class "section-heading"}
       [:div
        [:p "Performance"]
        [:h2 "Server stats"]]
       [:span "Live updates"]]
      [:div {:class "stats"}
       (for [metric stats]
         (stat-card (:label metric) (:value metric)))]]

     [:section {:id "admin-rooms-panel" :class "panel"}
      [:div {:class "section-heading"}
       [:div
        [:p "Tracking"]
        [:h2 "Active rooms"]]]
      (rooms-table rooms selected-id now)]

     (if (false? include-historical?)
       (historical-panel-placeholder)
       (historical-panel rooms historical-room-records))

     (or (room-detail room)
         [:section {:id "admin-room-detail" :class "panel detail"}
          [:p {:class "empty"} "No room selected."]])]))

(defn render-dashboard-main-html [opts]
  (h/render (render-dashboard-main opts)))

(def styles
  "body{margin:0;background:#111521;color:rgba(255,255,255,.78);font:15px/1.5 Arial,sans-serif}a{color:#6fd0c7;text-decoration:none}main{max-width:1320px;margin:0 auto;padding:24px}.top{display:flex;justify-content:space-between;gap:16px;align-items:flex-start;margin-bottom:18px}.top h1{margin:.1rem 0 0;color:white}.top p,.section-heading p{margin:0;color:rgba(255,255,255,.5);font-size:.72rem;font-weight:700;letter-spacing:.14em;text-transform:uppercase}.panel{border:1px solid rgba(255,255,255,.14);border-radius:8px;background:#18213a;padding:16px;margin-bottom:16px}.section-heading{display:flex;justify-content:space-between;gap:16px;align-items:center;margin-bottom:12px}.section-heading h2{margin:0;color:white}.admin-actions{display:flex;flex-wrap:wrap;gap:10px;align-items:center;justify-content:flex-end}.inline-form{display:inline;margin:0}button{min-height:32px;border:1px solid rgba(255,255,255,.22);border-radius:6px;background:rgba(255,255,255,.06);color:white;cursor:pointer;font-size:.68rem;font-weight:700;letter-spacing:.1em;padding:0 10px;text-transform:uppercase}button.danger{border-color:rgba(255,154,168,.55);background:rgba(255,154,168,.12);color:#ffbac3}.stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:10px}.stat{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(255,255,255,.04);padding:10px}.stat span{display:block;color:rgba(255,255,255,.5);font-size:.68rem;font-weight:700;letter-spacing:.12em;text-transform:uppercase}.stat strong{display:block;color:white;font-size:1.2rem;line-height:1.25}table{width:100%;border-collapse:collapse}th,td{border-bottom:1px solid rgba(255,255,255,.1);padding:8px;text-align:left}th{color:rgba(255,255,255,.52);font-size:.7rem;letter-spacing:.12em;text-transform:uppercase}.selected{background:rgba(111,208,199,.12)}.room-stats{margin-bottom:16px}.hands{display:grid;grid-template-columns:repeat(auto-fit,minmax(250px,1fr));gap:10px}.hands article{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.16);padding:10px}.hands strong{display:block;color:white;margin-bottom:6px}.card{display:inline-flex;align-items:center;justify-content:center;min-width:34px;height:46px;margin:0 4px 6px 0;border:1px solid rgba(0,0,0,.2);border-radius:6px;background:#f8f5ed;color:#141821;font-weight:800}.card.heart,.card.diamond{color:#c62f43}.trick{display:flex;flex-wrap:wrap;gap:10px}.trick>div{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.16);padding:8px}.trick span{display:block;color:rgba(255,255,255,.55);font-size:.72rem;font-weight:700}.two-col{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:14px}.compact-list{margin:0;padding-left:20px}.compact-list li{margin:6px 0}.compact-list span{display:inline-block;min-width:95px;color:rgba(255,255,255,.55)}.compact-list strong{color:white}.compact-list em{color:rgba(255,255,255,.55);font-style:normal}.empty{color:rgba(255,255,255,.45)}")

(def admin-layout-styles
  (str
   "*,*::before,*::after{box-sizing:border-box}"
   "main,.panel,.stats,.stat,.two-col,.two-col>*{min-width:0}"
   ".panel{max-width:100%;overflow-x:auto}"
   ".stat strong{font-size:clamp(.95rem,1.4vw,1.2rem);overflow-wrap:anywhere;word-break:break-word}"
   "table{max-width:100%;table-layout:auto}"
   "th,td{vertical-align:top;overflow-wrap:anywhere;word-break:break-word}"
   "td a{overflow-wrap:anywhere;word-break:break-word}"
   "td:last-child a{display:inline-block;max-width:100%}"
   "@media(max-width:900px){main{padding:12px}.top,.section-heading{align-items:flex-start;flex-direction:column}.admin-actions{justify-content:flex-start}.two-col{grid-template-columns:1fr}.panel table:not(.admin-table){min-width:680px}}"
   "@media(max-width:720px){body{font-size:13px;line-height:1.32}main{max-width:none;padding:8px}.top{gap:6px;margin-bottom:8px}.top h1{font-size:1.35rem;line-height:1.1}.panel{padding:8px;margin-bottom:8px;overflow-x:hidden}.section-heading{gap:6px;margin-bottom:8px}.section-heading h2{font-size:1.12rem}.admin-actions{gap:6px}.admin-actions a{display:inline-flex;align-items:center;min-height:26px;font-size:.72rem}.stats{grid-template-columns:repeat(3,minmax(0,1fr));gap:6px}.stat{padding:6px}.stat span{font-size:.48rem;letter-spacing:.08em}.stat strong{font-size:.85rem;line-height:1.12}.room-stats{margin-bottom:8px}.hands{grid-template-columns:1fr;gap:8px}.hands article{padding:8px}.trick{gap:7px}.trick>div{padding:6px}.admin-table{display:table;width:100%;max-width:100%;table-layout:fixed;border-collapse:collapse;font-size:clamp(.5rem,1.65vw,.68rem);line-height:1.12}.admin-table thead{display:table-header-group}.admin-table tbody{display:table-row-group}.admin-table tr{display:table-row;border:0;background:transparent;padding:0}.admin-table tr.selected{background:rgba(111,208,199,.14)}.admin-table th,.admin-table td{display:table-cell;border-bottom:1px solid rgba(255,255,255,.08);padding:.22rem .16rem;vertical-align:top;overflow-wrap:anywhere;word-break:break-word}.admin-table th{font-size:.48rem;letter-spacing:.05em;line-height:1.08}.admin-table td::before{content:none}.admin-table td[data-label=\"Hand\"],.admin-table td[data-label=\"Hands\"],.admin-table td[data-label=\"Conns\"],.admin-table td[data-label=\"Cards\"]{text-align:center}.admin-table td[data-label=\"Links\"] a{display:inline;max-width:none}.admin-table button{min-height:22px;border-radius:5px;padding:0 .28rem;font-size:.5rem;letter-spacing:.04em}.compact-list{padding-left:0;list-style:none}.compact-list span{min-width:0}}"
   "@media(max-width:380px){.stats{grid-template-columns:repeat(2,minmax(0,1fr))}.admin-table{font-size:clamp(.46rem,1.55vw,.6rem)}.admin-table th,.admin-table td{padding:.18rem .12rem}.admin-table button{min-height:20px;padding:0 .2rem;font-size:.46rem}}"))

(def admin-card-styles
  ".suit{color:#f7f8ff;font-weight:900}.suit.heart,.suit.diamond{color:#ff7d8b}.card{box-sizing:border-box;display:inline-flex;align-items:center;justify-content:center;width:38px;min-width:38px;height:52px;margin:0 4px 6px 0;padding:0;border:1px solid rgba(0,0,0,.24);border-radius:6px;background:#f8f5ed;color:#141821;font-size:.95rem;font-weight:800;line-height:1;letter-spacing:0;vertical-align:middle;white-space:nowrap}.card.heart,.card.diamond{color:#c62f43}.trick .card,.compact-list .card,.hands .card{display:inline-flex;width:38px;min-width:38px;height:52px;color:#141821;font-size:.95rem;font-weight:800;line-height:1}.trick .card.heart,.trick .card.diamond,.compact-list .card.heart,.compact-list .card.diamond,.hands .card.heart,.hands .card.diamond{color:#c62f43}.trick-card{width:92px;min-width:92px}.trick-card .play-player{display:block;max-width:100%;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.trick>div:not(.trick-card) .card{display:inline-flex;width:38px;min-width:38px;height:52px}")

(defn render-dashboard [{:keys [rooms
                                selected-room-id
                                historical-room-records
                                metrics
                                pending-bot-count
                                open-websocket-count
                                limits
                                started-at]}]
  (str
   "<!doctype html>"
   (h/render
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
      [:title "Karbosh Admin"]
      [:style (str styles admin-layout-styles admin-card-styles)]]
     [:body
      (render-dashboard-main {:rooms rooms
                              :selected-room-id selected-room-id
                              :historical-room-records historical-room-records
                              :metrics metrics
                              :pending-bot-count pending-bot-count
                              :open-websocket-count open-websocket-count
                              :limits limits
                              :started-at started-at})
      [:script {:src "/karbosh/assets/js/admin.js?v=20260606-scroll"}]]])))

(defn render-history [{:keys [rooms records]}]
  (str
   "<!doctype html>"
   (h/render
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
      [:title "Karbosh Game History"]
      [:style (str styles admin-layout-styles admin-card-styles)]]
     [:body
      (render-history-main {:rooms rooms
                            :records records})]])))
