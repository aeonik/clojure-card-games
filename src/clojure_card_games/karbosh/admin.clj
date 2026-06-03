(ns clojure-card-games.karbosh.admin
  (:require [clojure.string :as str]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game])
  (:import [java.lang.management ManagementFactory]))

(defn escape-html [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

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
  (str "<span class=\"card" (card-class card) "\">"
       (escape-html (cards/card->str card))
       "</span>"))

(defn cards-html [cards]
  (if (seq cards)
    (apply str (map card-html cards))
    "<span class=\"empty\">--</span>"))

(defn player-label [view player]
  (or (some->> (:players view)
               (filter #(= player (:id %)))
               first
               :name)
      (some-> player name)
      "--"))

(defn stat-card [label value]
  (str "<div class=\"stat\"><span>" (escape-html label) "</span><strong>"
       (escape-html value) "</strong></div>"))

(defn runtime-stats [{:keys [rooms
                             metrics
                             pending-bot-count
                             open-websocket-count
                             limits
                             started-at
                             now]}]
  (let [runtime (Runtime/getRuntime)
        used (- (.totalMemory runtime) (.freeMemory runtime))
        room-updates (get metrics :room-updates 0)
        total-ns (get metrics :room-update-total-ns 0)
        avg-update-ms (if (pos? room-updates)
                        (/ total-ns room-updates 1000000.0)
                        0.0)
        connections (reduce + (map #(count (:connections %)) (vals rooms)))]
    [{:label "Uptime" :value (duration-label (- now started-at))}
     {:label "Rooms" :value (count rooms)}
     {:label "Max rooms" :value (or (:max-rooms limits) "--")}
     {:label "Connections" :value connections}
     {:label "Open websockets" :value (or open-websocket-count 0)}
     {:label "Max websockets" :value (or (:max-websocket-connections limits) "--")}
     {:label "Max room conns" :value (or (:max-room-connections limits) "--")}
     {:label "Max message" :value (bytes-label (or (:max-message-bytes limits) 0))}
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
     {:label "Threads" :value (.getThreadCount (ManagementFactory/getThreadMXBean))}]))

(defn room-age [now room]
  (duration-label (- now (:created-at room))))

(defn room-summary-row [now selected-id [room-id room]]
  (let [state (:game room)
        view (game/admin-view state (:seats room))]
    (str "<tr" (when (= selected-id room-id) " class=\"selected\"") ">"
         "<td><a href=\"/karbosh/admin?room=" (escape-html room-id) "\">"
         (escape-html room-id) "</a></td>"
         "<td>" (escape-html (kw-label (:phase state))) "</td>"
         "<td>" (escape-html (score-label (:scores state))) "</td>"
         "<td>" (escape-html (player-label view (:current-player state))) "</td>"
         "<td>" (inc (or (:hand-index state) 0)) "</td>"
         "<td>" (count (:connections room)) "</td>"
         "<td>" (room-age now room) "</td>"
         "</tr>")))

(defn rooms-table [rooms selected-id now]
  (if (seq rooms)
    (str "<table><thead><tr><th>Room</th><th>Phase</th><th>Score</th>"
         "<th>Current</th><th>Hand</th><th>Conns</th><th>Age</th></tr></thead><tbody>"
         (apply str (map #(room-summary-row now selected-id %)
                         (sort-by (comp :created-at val) rooms)))
         "</tbody></table>")
    "<p class=\"empty\">No rooms are currently running.</p>"))

(defn seats-table [view]
  (str "<table><thead><tr><th>Player</th><th>Team</th><th>Status</th><th>Cards</th></tr></thead><tbody>"
       (apply str
              (for [{:keys [id team name connected? bot? active? hand-count]} (:players view)]
                (str "<tr" (when (= id (:current-player view)) " class=\"selected\"") ">"
                     "<td>" (escape-html (or name (clojure.core/name id))) "</td>"
                     "<td>" (escape-html (team-label team)) "</td>"
                     "<td>" (escape-html (str (cond
                                                bot? "bot"
                                                connected? "online"
                                                :else "offline")
                                              (when (false? active?) " / sitting out"))) "</td>"
                     "<td>" hand-count "</td>"
                     "</tr>")))
       "</tbody></table>"))

(defn trick-html [view trick]
  (if (seq trick)
    (str "<div class=\"trick\">"
         (apply str
                (for [{:keys [player card]} trick]
                  (str "<div><span>" (escape-html (player-label view player))
                       "</span>" (card-html card) "</div>")))
         "</div>")
    "<p class=\"empty\">No cards on the table.</p>"))

(defn hands-html [view hands]
  (str "<div class=\"hands\">"
       (apply str
              (for [{:keys [id]} (:players view)]
                (str "<article><strong>" (escape-html (player-label view id))
                     "</strong><div>" (cards-html (get hands id)) "</div></article>")))
       "</div>"))

(defn bids-html [view]
  (if (seq (:bids-this-hand view))
    (str "<ol class=\"compact-list\">"
         (apply str
                (for [bid (:bids-this-hand view)]
                  (str "<li><span>" (escape-html (player-label view (:player bid)))
                       "</span><strong>" (escape-html (bid-label bid)) "</strong></li>")))
         "</ol>")
    "<p class=\"empty\">No bids this hand.</p>"))

(defn event-label [{:keys [type player card suit bid-type value]} view]
  (str (escape-html (kw-label type))
       " / " (escape-html (player-label view player))
       (when bid-type (str " / " (escape-html (bid-label {:bid-type bid-type :value value}))))
       (when suit (str " / " (escape-html (cards/suit->str suit))))
       (when card (str " / " (cards-html [card])))))

(defn recent-events-html [view events]
  (if (seq events)
    (str "<ol class=\"compact-list events\">"
         (apply str
                (for [event (take-last 18 events)]
                  (str "<li>" (event-label event view) "</li>")))
         "</ol>")
    "<p class=\"empty\">No events yet.</p>"))

(defn hand-summary-label [{:keys [points scores-after trump tricks]}]
  (str "Trump " (or (some-> trump cards/suit->str) "--")
       " / Tricks " (score-label tricks)
       " / Points " (score-label points)
       " / Score " (score-label scores-after)))

(defn hand-history-html [history]
  (if (seq history)
    (str "<ol class=\"compact-list\">"
         (apply str
                (for [{:keys [hand-index bid completed-tricks] :as hand} (take-last 10 history)]
                  (str "<li><span>Hand " (inc hand-index) "</span><strong>"
                       (escape-html (bid-label bid)) "</strong><em>"
                       (escape-html (hand-summary-label hand))
                       " / " (count completed-tricks) " tricks</em></li>")))
         "</ol>")
    "<p class=\"empty\">No completed hands yet.</p>"))

(defn selected-room [rooms selected-room-id]
  (or (get rooms selected-room-id)
      (some->> rooms (sort-by (comp :created-at val)) first val)))

(defn choose-selected-room-id [rooms requested-room-id]
  (or (when (contains? rooms requested-room-id) requested-room-id)
      (some->> rooms (sort-by (comp :created-at val)) first key)))

(defn room-detail [room]
  (when room
    (let [view (game/admin-view (:game room) (:seats room))
          debug (:debug view)
          last-trick (peek (:completed-tricks view))]
      (str "<section class=\"panel detail\"><div class=\"section-heading\"><div>"
           "<p>Selected Room</p><h2>" (escape-html (:id room)) "</h2></div>"
           "<a href=\"/karbosh/admin\">All rooms</a></div>"
           "<div class=\"stats room-stats\">"
           (stat-card "Phase" (kw-label (:phase view)))
           (stat-card "Score" (score-label (:scores view)))
           (stat-card "Current" (player-label view (:current-player view)))
           (stat-card "Bid" (bid-label (:current-bid view)))
           (stat-card "Trump" (or (some-> (:trump view) cards/suit->str) "--"))
           (stat-card "Tricks" (score-label (:tricks-this-hand view)))
           "</div>"
           "<h3>Seats</h3>" (seats-table view)
           "<h3>Table</h3>" (trick-html view (:current-trick view))
           "<h3>Last completed trick</h3>" (trick-html view last-trick)
           "<h3>Hands</h3>" (hands-html view (:hands debug))
           "<div class=\"two-col\"><section><h3>Bids</h3>" (bids-html view)
           "</section><section><h3>Recent events</h3>"
           (recent-events-html view (:history debug)) "</section></div>"
           "<h3>Hand history</h3>" (hand-history-html (:hand-history debug))
           "</section>"))))

(def styles
  "body{margin:0;background:#111521;color:rgba(255,255,255,.78);font:15px/1.5 Arial,sans-serif}a{color:#6fd0c7;text-decoration:none}main{max-width:1320px;margin:0 auto;padding:24px}.top{display:flex;justify-content:space-between;gap:16px;align-items:flex-start;margin-bottom:18px}.top h1{margin:.1rem 0 0;color:white}.top p,.section-heading p{margin:0;color:rgba(255,255,255,.5);font-size:.72rem;font-weight:700;letter-spacing:.14em;text-transform:uppercase}.panel{border:1px solid rgba(255,255,255,.14);border-radius:8px;background:#18213a;padding:16px;margin-bottom:16px}.section-heading{display:flex;justify-content:space-between;gap:16px;align-items:center;margin-bottom:12px}.section-heading h2{margin:0;color:white}.stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:10px}.stat{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(255,255,255,.04);padding:10px}.stat span{display:block;color:rgba(255,255,255,.5);font-size:.68rem;font-weight:700;letter-spacing:.12em;text-transform:uppercase}.stat strong{display:block;color:white;font-size:1.2rem;line-height:1.25}table{width:100%;border-collapse:collapse}th,td{border-bottom:1px solid rgba(255,255,255,.1);padding:8px;text-align:left}th{color:rgba(255,255,255,.52);font-size:.7rem;letter-spacing:.12em;text-transform:uppercase}.selected{background:rgba(111,208,199,.12)}.room-stats{margin-bottom:16px}.hands{display:grid;grid-template-columns:repeat(auto-fit,minmax(250px,1fr));gap:10px}.hands article{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.16);padding:10px}.hands strong{display:block;color:white;margin-bottom:6px}.card{display:inline-flex;align-items:center;justify-content:center;min-width:34px;height:46px;margin:0 4px 6px 0;border:1px solid rgba(0,0,0,.2);border-radius:6px;background:#f8f5ed;color:#141821;font-weight:800}.card.heart,.card.diamond{color:#c62f43}.trick{display:flex;flex-wrap:wrap;gap:10px}.trick>div{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.16);padding:8px}.trick span{display:block;color:rgba(255,255,255,.55);font-size:.72rem;font-weight:700}.two-col{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:14px}.compact-list{margin:0;padding-left:20px}.compact-list li{margin:6px 0}.compact-list span{display:inline-block;min-width:95px;color:rgba(255,255,255,.55)}.compact-list strong{color:white}.compact-list em{color:rgba(255,255,255,.55);font-style:normal}.empty{color:rgba(255,255,255,.45)}")

(defn render-dashboard [{:keys [rooms
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
    (str "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
         "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
         "<meta http-equiv=\"refresh\" content=\"8\">"
         "<title>Karbosh Admin</title><style>" styles "</style></head><body><main>"
         "<div class=\"top\"><div><p>Karbosh admin</p><h1>Runtime dashboard</h1></div>"
         "<a href=\"/karbosh/\">Back to game</a></div>"
         "<section class=\"panel\"><div class=\"section-heading\"><div><p>Performance</p>"
         "<h2>Server stats</h2></div><span>Auto-refreshes every 8s</span></div>"
         "<div class=\"stats\">" (apply str (map #(stat-card (:label %) (:value %)) stats))
         "</div></section>"
         "<section class=\"panel\"><div class=\"section-heading\"><div><p>Tracking</p>"
         "<h2>Running rooms</h2></div></div>"
         (rooms-table rooms selected-id now)
         "</section>"
         (or (room-detail room) "")
         "</main></body></html>")))
