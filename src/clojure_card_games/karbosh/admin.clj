(ns clojure-card-games.karbosh.admin
  (:require [clojure.string :as str]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]
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

(defn card-class [[_ suit]]
  (case suit
    :♥ " heart"
    :♦ " diamond"
    :♠ " spade"
    :♣ " club"
    ""))

(defn card-html [card]
  [:span {:class (str "card" (card-class card))}
   (cards/card->str card)])

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

(defn live-room-entry? [[_ room]]
  (and (map? room)
       (:created-at room)
       (:game room)))

(defn sorted-room-entries [rooms]
  (sort-by (comp :created-at val) (filter live-room-entry? rooms)))

(defn live-room [rooms room-id]
  (let [room (get rooms room-id)]
    (when (live-room-entry? [room-id room])
      room)))

(defn runtime-stats [{:keys [rooms
                             metrics
                             pending-bot-count
                             open-websocket-count
                             limits
                             started-at
                             now]}]
  (let [room-entries (filter live-room-entry? rooms)
        runtime (Runtime/getRuntime)
        used (- (.totalMemory runtime) (.freeMemory runtime))
        room-updates (get metrics :room-updates 0)
        total-ns (get metrics :room-update-total-ns 0)
        avg-update-ms (if (pos? room-updates)
                        (/ total-ns room-updates 1000000.0)
                        0.0)
        connections (reduce + (map #(count (:connections %)) (map val room-entries)))]
    [{:label "Uptime" :value (duration-label (- now started-at))}
     {:label "Rooms" :value (count room-entries)}
     {:label "Max rooms" :value (or (:max-rooms limits) "--")}
     {:label "Connections" :value connections}
     {:label "Open websockets" :value (or open-websocket-count 0)}
     {:label "Max websockets" :value (or (:max-websocket-connections limits) "--")}
     {:label "Max room conns" :value (or (:max-room-connections limits) "--")}
     {:label "Max message" :value (bytes-label (or (:max-message-bytes limits) 0))}
     {:label "Idle timeout" :value (duration-label (or (:idle-room-ms limits) 0))}
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
     [:td [:a {:href (str "/karbosh/admin?room=" room-id)} room-id]]
     [:td (kw-label (:phase state))]
     [:td (score-label (:scores state))]
     [:td (player-label view (:current-player state))]
     [:td (inc (or (:hand-index state) 0))]
     [:td (count (:connections room))]
     [:td (room-age now room)]
     [:td (room-idle-age now room)]
     [:td (delete-room-control room-id)]]))

(defn rooms-table [rooms selected-id now]
  (if (seq (sorted-room-entries rooms))
    [:table
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
    [:p {:class "empty"} "No rooms are currently running."]))

(defn room-record-room [record]
  (:room record))

(defn room-hand-count [room]
  (count (get-in room [:game :hand-history])))

(defn room-winner [room]
  (get-in room [:game :winner]))

(defn historical-room-records [rooms records]
  (let [live-ids (set (map first (sorted-room-entries rooms)))]
    (->> records
         (remove #(contains? live-ids (:room-id %)))
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
           [:td bid]
           [:td attempts]
           [:td made]
           [:td make-rate]
           [:td avg-margin]])]]
      [:p {:class "empty"} "No completed bid history yet."])))

(defn historical-room-row [record]
  (let [room (:room record)
        state (:game room)
        room-id (:room-id record)]
    [:tr
     [:td [:a {:href (str "/karbosh/admin/rooms/" room-id "/snapshot")} room-id]]
     [:td (kw-label (:phase state))]
     [:td (score-label (:scores state))]
     [:td (or (some-> room room-winner team-label) "--")]
     [:td (room-hand-count room)]
     [:td (kw-label (:type record))]
     [:td (time-label (:logged-at record))]
     [:td
      [:a {:href (str "/karbosh/admin/rooms/" room-id "/snapshot")} "Snapshot"]
      " / "
      [:a {:href (str "/karbosh/admin/rooms/" room-id "/snapshot.edn")} "Raw"]]]))

(defn historical-rooms-table [records]
  (if (seq records)
    [:table
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

(defn record-game-seed [record]
  (get-in record [:room :game :initial-seed]))

(defn record-game-key [record]
  [(:room-id record) (record-game-seed record)])

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

(defn game-history-link [room-id seed suffix]
  (str "/karbosh/admin/history/" room-id "/" seed "/" suffix))

(defn game-history-row [record]
  (let [room (:room record)
        state (:game room)
        room-id (:room-id record)
        seed (record-game-seed record)]
    [:tr
     [:td room-id]
     [:td (or (some-> seed str) "--")]
     [:td (kw-label (:phase state))]
     [:td (score-label (:scores state))]
     [:td (or (some-> room room-winner team-label) "--")]
     [:td (room-hand-count room)]
     [:td (kw-label (:type record))]
     [:td (time-label (:logged-at record))]
     [:td
      [:a {:href (game-history-link room-id seed "snapshot")} "Snapshot"]
      " / "
      [:a {:href (game-history-link room-id seed "snapshot.edn")} "Raw"]]]))

(defn game-history-table [records]
  (let [records (game-history-records records)]
    (if (seq records)
      [:table
       [:thead
        [:tr
         [:th "Room"]
         [:th "Seed"]
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
      (game-history-table records)]]))

(defn seats-table [view]
  [:table
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
         [:td (or name (clojure.core/name id))]
         [:td (team-label team)]
         [:td status]
         [:td hand-count]]))]])

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

(defn trick-card-html [view trump winner {:keys [player card]}]
  [:div {:class (str "trick-card"
                     (when (= winner player) " winner"))}
   [:span {:class "play-player"} (player-label view player)]
   (card-html card)
   (when (= winner player)
     [:strong "Won"])])

(defn trick-detail-html [view trump index trick]
  (let [winner (trick-winner trump trick)]
    [:article {:class "trick-detail"}
     [:div {:class "trick-heading"}
      [:strong (str "Trick " (inc index))]
      [:span (str "Winner: " (player-label view winner))]]
     [:div {:class "trick"}
      (for [play trick]
        (trick-card-html view trump winner play))]]))

(defn current-trick-detail-html [view trump trick]
  (when (seq trick)
    [:article {:class "trick-detail current-trick-detail"}
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

(defn compact-starting-hands-html [view hands]
  (if (seq hands)
    [:div {:class "starting-hands-strip"}
     (for [{:keys [id]} (:players view)]
       [:div {:class "starting-hand-row"}
        [:strong (player-label view id)]
        [:div {:class "starting-hand-cards"}
         (cards-html (get hands id))]])]
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
   (stat-card "Trump" (or (some-> (:trump hand) cards/suit->str) "--"))
   (stat-card "Tricks" (score-label (:tricks hand)))
   (stat-card "Points" (score-label (:points hand)))
   (stat-card "Score" (score-label (:scores-after hand)))
   (stat-card "Seed" (or (some-> hand hand-seed str) "--"))])

(defn hand-play-by-play-html [view hand]
  [:section {:class "panel hand-detail"}
   [:div {:class "section-heading"}
    [:div
     [:p (if (:current? hand) "Live hand" "Completed hand")]
     [:h2 (hand-title hand)]]]
   (hand-stats-html hand)
   [:div {:class "two-col"}
    [:section
     [:h3 "Bidding"]
     (bids-detail-html view hand)]
    [:section
     [:h3 "Starting Hands"]
     (compact-starting-hands-html view (:initial-hands hand))]]
   [:h3 "Play by Play"]
   (if (or (seq (:completed-tricks hand))
           (seq (:current-trick hand)))
     [:div {:class "trick-timeline"}
      (for [[index trick] (map-indexed vector (:completed-tricks hand))]
        (trick-detail-html view (:trump hand) index trick))
      (current-trick-detail-html view (:trump hand) (:current-trick hand))]
     [:p {:class "empty"} "No cards have been played."])
   (initial-hands-html view (:initial-hands hand))])

(defn room-snapshot-main [room]
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
     (if (seq hands)
       (for [hand hands]
         (hand-play-by-play-html view hand))
       [:section {:class "panel"}
        [:p {:class "empty"} "No hands recorded."]])]))

(def snapshot-styles
  (str
   ".play-list{margin:0;padding-left:0;list-style:none}"
   ".play-list li,.play-line{display:flex;gap:10px;align-items:center;border-bottom:1px solid rgba(255,255,255,.08);margin:0;padding:7px 0}"
   ".event-kind{min-width:74px;color:rgba(255,255,255,.48);font-size:.68rem;font-weight:700;letter-spacing:.12em;text-transform:uppercase}"
   ".hand-detail h3{color:white;margin:18px 0 8px}"
   ".trick-timeline{display:grid;gap:10px}"
   ".trick-detail{border:1px solid rgba(255,255,255,.11);border-radius:8px;background:rgba(0,0,0,.12);padding:10px}"
   ".trick-heading{display:flex;justify-content:space-between;gap:16px;align-items:center;margin-bottom:8px}"
   ".trick-heading strong{color:white}"
   ".trick-heading span{color:rgba(255,255,255,.55);font-size:.78rem;font-weight:700}"
   ".trick-card{position:relative}"
   ".trick-card.winner{border-color:rgba(245,200,91,.65);background:rgba(245,200,91,.12)}"
   ".trick-card .play-player{color:rgba(255,255,255,.58);font-size:.72rem;font-weight:700}"
   ".trick-card strong{display:block;color:#f5c85b;font-size:.66rem;letter-spacing:.12em;text-transform:uppercase}"
   ".starting-hands-strip{display:grid;gap:7px}"
   ".starting-hand-row{display:grid;grid-template-columns:minmax(84px,112px) 1fr;gap:8px;align-items:center;border-bottom:1px solid rgba(255,255,255,.08);padding:4px 0}"
   ".starting-hand-row strong{color:rgba(255,255,255,.72);font-size:.72rem;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
   ".starting-hand-cards{display:flex;flex-wrap:wrap;gap:3px}"
   ".starting-hands-strip .card{width:24px;min-width:24px;height:32px;margin:0;padding:0;border-radius:4px;font-size:.68rem}"
   ".starting-hands-strip .empty{font-size:.72rem}"
   ".initial-hands{margin-top:14px}"
   ".initial-hands summary{cursor:pointer;color:#6fd0c7;font-weight:700;margin-bottom:10px}"
   "@media(max-width:720px){.play-list li,.play-line{align-items:flex-start;flex-direction:column;gap:4px}.trick-heading{align-items:flex-start;flex-direction:column;gap:2px}.starting-hand-row{grid-template-columns:1fr;gap:4px}}"))

(declare styles admin-card-styles)

(defn render-room-snapshot [room]
  (str
   "<!doctype html>"
   (h/render
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
      [:title (str "Karbosh Room " (:id room) " History")]
      [:style (str styles admin-card-styles snapshot-styles)]]
     [:body
      (room-snapshot-main room)]])))

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
        (stat-card "Trump" (or (some-> (:trump view) cards/suit->str) "--"))
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
        [:h2 "Running rooms"]]]
      (rooms-table rooms selected-id now)]

     (historical-panel rooms historical-room-records)

     (or (room-detail room)
         [:section {:id "admin-room-detail" :class "panel detail"}
          [:p {:class "empty"} "No room selected."]])]))

(defn render-dashboard-main-html [opts]
  (h/render (render-dashboard-main opts)))

(def styles
  "body{margin:0;background:#111521;color:rgba(255,255,255,.78);font:15px/1.5 Arial,sans-serif}a{color:#6fd0c7;text-decoration:none}main{max-width:1320px;margin:0 auto;padding:24px}.top{display:flex;justify-content:space-between;gap:16px;align-items:flex-start;margin-bottom:18px}.top h1{margin:.1rem 0 0;color:white}.top p,.section-heading p{margin:0;color:rgba(255,255,255,.5);font-size:.72rem;font-weight:700;letter-spacing:.14em;text-transform:uppercase}.panel{border:1px solid rgba(255,255,255,.14);border-radius:8px;background:#18213a;padding:16px;margin-bottom:16px}.section-heading{display:flex;justify-content:space-between;gap:16px;align-items:center;margin-bottom:12px}.section-heading h2{margin:0;color:white}.admin-actions{display:flex;flex-wrap:wrap;gap:10px;align-items:center;justify-content:flex-end}.inline-form{display:inline;margin:0}button{min-height:32px;border:1px solid rgba(255,255,255,.22);border-radius:6px;background:rgba(255,255,255,.06);color:white;cursor:pointer;font-size:.68rem;font-weight:700;letter-spacing:.1em;padding:0 10px;text-transform:uppercase}button.danger{border-color:rgba(255,154,168,.55);background:rgba(255,154,168,.12);color:#ffbac3}.stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:10px}.stat{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(255,255,255,.04);padding:10px}.stat span{display:block;color:rgba(255,255,255,.5);font-size:.68rem;font-weight:700;letter-spacing:.12em;text-transform:uppercase}.stat strong{display:block;color:white;font-size:1.2rem;line-height:1.25}table{width:100%;border-collapse:collapse}th,td{border-bottom:1px solid rgba(255,255,255,.1);padding:8px;text-align:left}th{color:rgba(255,255,255,.52);font-size:.7rem;letter-spacing:.12em;text-transform:uppercase}.selected{background:rgba(111,208,199,.12)}.room-stats{margin-bottom:16px}.hands{display:grid;grid-template-columns:repeat(auto-fit,minmax(250px,1fr));gap:10px}.hands article{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.16);padding:10px}.hands strong{display:block;color:white;margin-bottom:6px}.card{display:inline-flex;align-items:center;justify-content:center;min-width:34px;height:46px;margin:0 4px 6px 0;border:1px solid rgba(0,0,0,.2);border-radius:6px;background:#f8f5ed;color:#141821;font-weight:800}.card.heart,.card.diamond{color:#c62f43}.trick{display:flex;flex-wrap:wrap;gap:10px}.trick>div{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.16);padding:8px}.trick span{display:block;color:rgba(255,255,255,.55);font-size:.72rem;font-weight:700}.two-col{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:14px}.compact-list{margin:0;padding-left:20px}.compact-list li{margin:6px 0}.compact-list span{display:inline-block;min-width:95px;color:rgba(255,255,255,.55)}.compact-list strong{color:white}.compact-list em{color:rgba(255,255,255,.55);font-style:normal}.empty{color:rgba(255,255,255,.45)}")

(def admin-card-styles
  ".card{box-sizing:border-box;display:inline-flex;align-items:center;justify-content:center;width:38px;min-width:38px;height:52px;margin:0 4px 6px 0;padding:0;border:1px solid rgba(0,0,0,.24);border-radius:6px;background:#f8f5ed;color:#141821;font-size:.95rem;font-weight:800;line-height:1;letter-spacing:0;vertical-align:middle;white-space:nowrap}.card.heart,.card.diamond{color:#c62f43}.trick .card,.compact-list .card,.hands .card{display:inline-flex;width:38px;min-width:38px;height:52px;color:#141821;font-size:.95rem;font-weight:800;line-height:1}.trick .card.heart,.trick .card.diamond,.compact-list .card.heart,.compact-list .card.diamond,.hands .card.heart,.hands .card.diamond{color:#c62f43}.trick-card{width:92px;min-width:92px}.trick-card .play-player{display:block;max-width:100%;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.trick>div:not(.trick-card) .card{display:inline-flex;width:38px;min-width:38px;height:52px}")

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
      [:style (str styles admin-card-styles)]]
     [:body
      (render-dashboard-main {:rooms rooms
                              :selected-room-id selected-room-id
                              :historical-room-records historical-room-records
                              :metrics metrics
                              :pending-bot-count pending-bot-count
                              :open-websocket-count open-websocket-count
                              :limits limits
                              :started-at started-at})
      [:script {:src "/karbosh/assets/js/admin.js?v=20260604-stream"}]]])))

(defn render-history [{:keys [rooms records]}]
  (str
   "<!doctype html>"
   (h/render
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
      [:title "Karbosh Game History"]
      [:style (str styles admin-card-styles)]]
     [:body
      (render-history-main {:rooms rooms
                            :records records})]])))
