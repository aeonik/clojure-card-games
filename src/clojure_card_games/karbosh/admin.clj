(ns clojure-card-games.karbosh.admin
  (:require [clojure.string :as str]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.hiccup :as h])
  (:import [java.lang.management ManagementFactory]))

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

     (or (room-detail room)
         [:section {:id "admin-room-detail" :class "panel detail"}
          [:p {:class "empty"} "No room selected."]])]))

(defn render-dashboard-main-html [opts]
  (h/render (render-dashboard-main opts)))

(def styles
  "body{margin:0;background:#111521;color:rgba(255,255,255,.78);font:15px/1.5 Arial,sans-serif}a{color:#6fd0c7;text-decoration:none}main{max-width:1320px;margin:0 auto;padding:24px}.top{display:flex;justify-content:space-between;gap:16px;align-items:flex-start;margin-bottom:18px}.top h1{margin:.1rem 0 0;color:white}.top p,.section-heading p{margin:0;color:rgba(255,255,255,.5);font-size:.72rem;font-weight:700;letter-spacing:.14em;text-transform:uppercase}.panel{border:1px solid rgba(255,255,255,.14);border-radius:8px;background:#18213a;padding:16px;margin-bottom:16px}.section-heading{display:flex;justify-content:space-between;gap:16px;align-items:center;margin-bottom:12px}.section-heading h2{margin:0;color:white}.admin-actions{display:flex;flex-wrap:wrap;gap:10px;align-items:center;justify-content:flex-end}.inline-form{display:inline;margin:0}button{min-height:32px;border:1px solid rgba(255,255,255,.22);border-radius:6px;background:rgba(255,255,255,.06);color:white;cursor:pointer;font-size:.68rem;font-weight:700;letter-spacing:.1em;padding:0 10px;text-transform:uppercase}button.danger{border-color:rgba(255,154,168,.55);background:rgba(255,154,168,.12);color:#ffbac3}.stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:10px}.stat{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(255,255,255,.04);padding:10px}.stat span{display:block;color:rgba(255,255,255,.5);font-size:.68rem;font-weight:700;letter-spacing:.12em;text-transform:uppercase}.stat strong{display:block;color:white;font-size:1.2rem;line-height:1.25}table{width:100%;border-collapse:collapse}th,td{border-bottom:1px solid rgba(255,255,255,.1);padding:8px;text-align:left}th{color:rgba(255,255,255,.52);font-size:.7rem;letter-spacing:.12em;text-transform:uppercase}.selected{background:rgba(111,208,199,.12)}.room-stats{margin-bottom:16px}.hands{display:grid;grid-template-columns:repeat(auto-fit,minmax(250px,1fr));gap:10px}.hands article{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.16);padding:10px}.hands strong{display:block;color:white;margin-bottom:6px}.card{display:inline-flex;align-items:center;justify-content:center;min-width:34px;height:46px;margin:0 4px 6px 0;border:1px solid rgba(0,0,0,.2);border-radius:6px;background:#f8f5ed;color:#141821;font-weight:800}.card.heart,.card.diamond{color:#c62f43}.trick{display:flex;flex-wrap:wrap;gap:10px}.trick>div{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.16);padding:8px}.trick span{display:block;color:rgba(255,255,255,.55);font-size:.72rem;font-weight:700}.two-col{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:14px}.compact-list{margin:0;padding-left:20px}.compact-list li{margin:6px 0}.compact-list span{display:inline-block;min-width:95px;color:rgba(255,255,255,.55)}.compact-list strong{color:white}.compact-list em{color:rgba(255,255,255,.55);font-style:normal}.empty{color:rgba(255,255,255,.45)}")

(defn render-dashboard [{:keys [rooms
                                selected-room-id
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
      [:style styles]]
     [:body
      (render-dashboard-main {:rooms rooms
                              :selected-room-id selected-room-id
                              :metrics metrics
                              :pending-bot-count pending-bot-count
                              :open-websocket-count open-websocket-count
                              :limits limits
                              :started-at started-at})
      [:script {:src "/karbosh/assets/js/admin.js?v=20260604-stream"}]]])))
