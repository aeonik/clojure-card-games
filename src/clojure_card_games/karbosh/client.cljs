(ns clojure-card-games.karbosh.client
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.rules :as rules]))

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
         :join-modal nil
         :pending-card nil
         :pending-auto? false
         :error nil}))

(def play-animation-ms 1150)
(def trick-popup-ms 3400)
(def bid-popup-ms 1600)

(defn el [id]
  (.getElementById js/document id))

(defn qs [selector]
  (.querySelector js/document selector))

(defn html! [node content]
  (set! (.-innerHTML node) content))

(defn text! [node content]
  (set! (.-textContent node) content))

(defn escape-html [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

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

(defn remove-first-card [card hand]
  (let [[before after] (split-with #(not= card %) hand)]
    (vec (concat before (rest after)))))

(defn visible-hand [hand pending-card]
  (if (and pending-card (some #(= pending-card %) hand))
    (remove-first-card pending-card hand)
    hand))

(defn player-class [player]
  (str "player-" (kw-name player)))

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

(defn ws-url []
  (let [params (js/URLSearchParams. (.-search js/location))
        explicit (.get params "ws")
        protocol (if (= "https:" (.-protocol js/location)) "wss://" "ws://")]
    (or explicit (str protocol (.-host js/location) "/karbosh/ws"))))

(defn save-session! [room-id player name]
  (.setItem js/localStorage "karbosh-room" room-id)
  (.setItem js/localStorage "karbosh-player" (kw-name player))
  (.setItem js/localStorage "karbosh-name" name))

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

(defn stored-room-id []
  (let [room (.getItem js/localStorage "karbosh-room")]
    (when-not (str/blank? room)
      (str/trim room))))

(defn stored-player []
  (let [p (.getItem js/localStorage "karbosh-player")]
    (when-not (str/blank? p) (keyword p))))

(defn player-name []
  (let [input (el "player-name")
        value (str/trim (.-value input))]
    (if (str/blank? value) "Player" value)))

(defn set-share-link! [room-id]
  (let [url (js/URL. (.-href js/location))]
    (set! (.-search url) (str "?room=" room-id))
    (text! (el "share-link") (.-href url))))

(defn render-status! []
  (let [{:keys [connected? room-id player error]} @app]
    (text! (el "connection-status")
           (cond
             error error
             connected? "Connected"
             :else "Disconnected"))
    (text! (el "room-code") (or room-id "--"))
    (text! (el "seat-code") (or (some-> player name) "--"))))

(defn room-preview-url [room-id]
  (str "/karbosh/api/room/" (js/encodeURIComponent room-id)))

(declare close-join-modal!
         join-from-modal!
         join-room!)

(defn occupied-preview-players [preview]
  (filterv (complement :open?) (:players preview)))

(defn joinable-seat-count [preview]
  (count (filter :joinable? (:players preview))))

(defn join-modal-full? [preview]
  (and preview (zero? (joinable-seat-count preview))))

(defn join-modal-disabled? [loading? preview error]
  (or loading? error (join-modal-full? preview)))

(defn preview-player-html [{:keys [name bot? connected?]}]
  (str "<li><strong>" (escape-html (or name "Open")) "</strong>"
       "<span>" (cond
                  bot? "Bot"
                  connected? "Online"
                  :else "Offline")
       "</span></li>"))

(defn preview-players-html [preview]
  (let [players (occupied-preview-players preview)
        available-count (joinable-seat-count preview)]
    (str "<ul class=\"join-modal-players\">"
         (if (seq players)
           (apply str (map preview-player-html players))
           "<li><strong>No players yet</strong><span>Open table</span></li>")
         "</ul>"
         "<p class=\"join-modal-count\">" available-count " available "
         (if (= 1 available-count) "seat" "seats") "</p>")))

(defn render-join-modal! []
  (let [{:keys [room-id loading? preview error]} (:join-modal @app)]
    (html! (el "modal-root")
           (if room-id
             (str "<div class=\"modal-backdrop\">"
                  "<section class=\"join-modal\" role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"join-modal-title\">"
                  "<p class=\"eyebrow\">Karbosh table</p>"
                  "<h2 id=\"join-modal-title\">Join room " (escape-html room-id) "</h2>"
                  (cond
                    loading?
                    "<p class=\"join-modal-muted\">Loading players...</p>"

                    error
                    (str "<p class=\"join-modal-error\">" (escape-html error) "</p>")

                    :else
                    (str "<h3>Current players</h3>"
                         (preview-players-html preview)))
                  "<label><span>Name</span>"
                  "<input id=\"join-modal-name\" type=\"text\" maxlength=\"24\" value=\""
                  (escape-html (player-name)) "\"></label>"
                  "<div class=\"join-modal-actions\">"
                  "<button id=\"join-modal-cancel\" type=\"button\">Cancel</button>"
                  "<button id=\"join-modal-submit\" type=\"button\""
                  (when (join-modal-disabled? loading? preview error) " disabled")
                  ">Join Table</button></div>"
                  "</section></div>")
             "")))
  (when (:join-modal @app)
    (when-let [cancel (el "join-modal-cancel")]
      (.addEventListener cancel "click" close-join-modal!))
    (when-let [submit (el "join-modal-submit")]
      (.addEventListener submit "click" join-from-modal!))
    (when-let [input (el "join-modal-name")]
      (.focus input)
      (.select input)
      (.addEventListener input "keydown"
                         (fn [event]
                           (when (= "Enter" (.-key event))
                             (join-from-modal!)))))))

(defn close-join-modal! []
  (swap! app assoc :join-modal nil)
  (render-join-modal!))

(defn join-from-modal! []
  (let [{:keys [room-id loading? preview error]} (:join-modal @app)
        input (el "join-modal-name")
        name (if input
               (str/trim (.-value input))
               "")]
    (when (and room-id
               (not (join-modal-disabled? loading? preview error)))
      (when-not (str/blank? name)
        (set! (.-value (el "player-name")) name))
      (set! (.-value (el "join-room-id")) room-id)
      (close-join-modal!)
      (join-room! room-id nil))))

(defn seat-state-label [{:keys [bot? connected?]}]
  (cond
    bot? "bot"
    connected? "online"
    :else "open"))

(defn hand-backs-html [hand-count]
  (apply str
         (for [n (range (min 5 hand-count))]
           (str "<i style=\"--i:" n "\"></i>"))))

(defn player-seat-html [view {:keys [id team name connected? bot? active? hand-count] :as seat}]
  (let [current? (= id (:current-player view))
        you? (= id (:you view))]
    (str "<div class=\"player-seat " (player-class id)
         (when connected? " is-connected")
         (when bot? " is-bot")
         (when (false? active?) " is-sitting-out")
         (when current? " is-current")
         (when you? " is-you")
         "\">"
         "<div><strong>" (escape-html (or name (clojure.core/name id))) "</strong>"
         "<small>" (team-label team) " / " hand-count " cards / " (seat-state-label seat) "</small>"
         (when-let [bid (latest-bid view id)]
           (str "<em class=\"bid-chip\">" (escape-html (bid-label bid)) "</em>"))
         "</div>"
         "<div class=\"seat-hand-backs\">" (hand-backs-html hand-count) "</div>"
         (when current? "<em class=\"turn-badge\">Current</em>")
         "</div>")))

(defn mobile-seat-roster-html [view]
  (str "<ul class=\"mobile-seat-roster\" aria-label=\"Players\">"
       (apply str
              (for [{:keys [id team name connected? bot? active? hand-count] :as seat} (:players view)]
                (let [current? (= id (:current-player view))
                      you? (= id (:you view))]
                  (str "<li class=\"" (player-class id)
                       (when connected? " is-connected")
                       (when bot? " is-bot")
                       (when (false? active?) " is-sitting-out")
                       (when current? " is-current")
                       (when you? " is-you")
                       "\">"
                       "<div><strong>" (escape-html (or name (clojure.core/name id))) "</strong>"
                       "<span>" (team-label team) " / " hand-count " cards / "
                       (seat-state-label seat) "</span></div>"
                       (if-let [bid (latest-bid view id)]
                         (str "<em>" (escape-html (bid-label bid)) "</em>")
                         "<em>--</em>")
                       "</li>"))))
       "</ul>"))

(defn trick-card-html [view {:keys [player card]}]
  (str "<li class=\"trick-card " (player-class player) "\">"
       "<span>" (escape-html (player-label view player)) "</span>"
       "<strong class=\"card-face" (card-suit-class card) "\">"
       (card-label card)
       "</strong></li>"))

(defn play-animation-html [view {:keys [player card]}]
  (when (and player card)
    (str "<li class=\"trick-card is-animating " (player-class player)
         " from-" (player-class player) "\">"
         "<span>" (escape-html (player-label view player)) "</span>"
         "<strong class=\"card-face" (card-suit-class card) "\">"
         (card-label card)
         "</strong></li>")))

(defn trick-html [view trick animation]
  (let [cards (apply str (map #(trick-card-html view %) trick))
        animation (play-animation-html view animation)]
    (if (or (seq trick) animation)
      (str cards (or animation ""))
      "<li class=\"trick-empty\"><span>No cards played</span></li>")))

(defn same-play? [a b]
  (and (= (:player a) (:player b))
       (= (:card a) (:card b))))

(defn settled-trick [trick animation]
  (if animation
    (vec (remove #(same-play? % animation) trick))
    trick))

(defn trick-popup-html [view {:keys [player card]}]
  (when player
    (str "<div class=\"trick-winner-popup\">"
         "<span>Trick winner</span>"
         "<strong>" (escape-html (player-label view player)) "</strong>"
         (when card
           (str "<em>with <span class=\"card-face" (card-suit-class card) "\">"
                (card-label card)
                "</span></em>"))
         "</div>")))

(defn bid-popup-html [view {:keys [player] :as bid}]
  (when player
    (str "<div class=\"bid-popup\">"
         "<span>" (escape-html (player-label view player)) "</span>"
         "<strong>" (escape-html (bid-label bid)) "</strong>"
         "</div>")))

(defn bid-log-html [view]
  (when (seq (:bids-this-hand view))
    (str "<ol class=\"bid-log\">"
         (apply str
                (for [bid (:bids-this-hand view)]
                  (str "<li><span>" (escape-html (player-label view (:player bid)))
                       "</span><strong>" (escape-html (bid-label bid)) "</strong></li>")))
         "</ol>")))

(defn trump-value-html [suit]
  (if suit
    (str "<strong class=\"trump-symbol" (suit-class suit) "\">"
         (cards/suit->str suit)
         "</strong>")
    "<strong class=\"trump-symbol is-empty\">--</strong>"))

(defn table-hand-status-html [view]
  (str "<div class=\"table-hand-status\">"
       "<div><span>Trump</span>" (trump-value-html (:trump view)) "</div>"
       "<div><span>Bid</span><strong>" (escape-html (bid-label (:current-bid view))) "</strong></div>"
       "<div><span>Tricks</span><strong>" (get-in view [:tricks-this-hand 1] 0)
       " / " (get-in view [:tricks-this-hand 2] 0)
       "</strong></div>"
       "</div>"))

(defn table-status-html [view bid-popup]
  (str "<div class=\"table-status\">"
       "<div class=\"turn-summary\"><span>Current player</span><strong>"
       (escape-html (player-label view (:current-player view)))
       "</strong></div>"
       "<div class=\"bid-trail\"><span>Bid trail</span>"
       (or (bid-log-html view) "<ol class=\"bid-log is-empty\"><li><strong>--</strong></li></ol>")
       "</div>"
       (or (bid-popup-html view bid-popup) "")
       "</div>"))

(defn table-surface-html [view animation trick-popup queued-trick-popup]
  (let [trick (if-let [completed-trick (:trick trick-popup)]
                completed-trick
                (if-let [queued-trick (:trick queued-trick-popup)]
                  (settled-trick queued-trick animation)
                  (settled-trick (:current-trick view) animation)))]
    (str "<div class=\"table-surface\">"
         (table-hand-status-html view)
         "<div class=\"felt-oval\"></div>"
         (apply str (map #(player-seat-html view %) (:players view)))
         "<div class=\"table-center\">"
         "<ul class=\"trick-pile\">" (trick-html view trick animation) "</ul>"
         "</div>"
         (or (trick-popup-html view trick-popup) "")
         "</div>")))

(defn card-button [{:keys [card disabled?]}]
  (str "<button class=\"card-button" (card-suit-class card) "\" data-card=\""
       (escape-html (pr-str card))
       "\""
       (when disabled? " disabled")
       ">"
       (card-label card)
       "</button>"))

(def auto-play-phases
  #{:bidding
    :trump-selection
    :karbosh-donation
    :karbosh-discard
    :trick-playing})

(defn bid-controls [view active?]
  (when (= :bidding (:phase view))
    (let [disabled (not active?)]
      (str "<div class=\"control-group\">"
           "<button data-bid=\"pass\"" (when disabled " disabled") ">Pass</button>"
           (apply str
                  (for [n (range 1 9)]
                    (str "<button data-bid-value=\"" n "\""
                         (when disabled " disabled")
                         ">" n "</button>")))
           "<button data-bid=\"karbosh\"" (when disabled " disabled") ">Karbosh</button>"
           "<button data-bid=\"double-karbosh\"" (when disabled " disabled") ">Double</button>"
           "</div>"))))

(defn trump-controls [view active?]
  (when (= :trump-selection (:phase view))
    (str "<div class=\"control-group trump-control-group\">"
         (apply str
                (for [suit cards/suits]
                  (str "<button class=\"trump-button" (suit-class suit) "\""
                       " data-trump=\"" (pr-str suit) "\""
                       (when-not active? " disabled")
                       ">" (cards/suit->str suit) "</button>")))
         "</div>")))

(defn auto-play-controls [view active? paused? pending?]
  (when (auto-play-phases (:phase view))
    (str "<div class=\"control-group auto-play-control\">"
         "<button class=\"auto-play-button\" data-auto-play=\"true\""
         (when (or (not active?) paused? pending?) " disabled")
         ">Auto Play</button>"
         "</div>")))

(defn hand-title [view]
  (case (:phase view)
    :karbosh-donation "Donate one card"
    :karbosh-discard "Discard two cards"
    "Your hand"))

(defn card-disabled? [view hand card pending-card paused?]
  (let [active? (= (:you view) (:current-player view))]
    (or pending-card
        paused?
        (not active?)
        (case (:phase view)
          :trick-playing
          (not (rules/legal-play? hand (:current-trick view) card (:trump view)))

          (:karbosh-donation :karbosh-discard)
          false

          true))))

(defn hand-panel-html [view pending-card paused?]
  (let [hand (visible-hand (:hand view) pending-card)]
    (str "<section class=\"hand-panel\">"
         "<div class=\"hand-heading\"><h2>" (escape-html (hand-title view)) "</h2><span>"
         (count hand)
         " cards</span></div>"
         "<div class=\"hand-row\">"
         (apply str
                (for [card hand]
                  (card-button {:card card
                                :disabled? (card-disabled? view hand card pending-card paused?)})))
         "</div></section>")))

(defn game-over-html [view]
  (when (= :game-over (:phase view))
    (str "<section class=\"game-over-panel\">"
         "<span>Game over</span>"
         "<strong>" (escape-html (team-label (:winner view))) " wins</strong>"
         "<em>Final score " (get-in view [:scores 1] 0)
         " / " (get-in view [:scores 2] 0) "</em>"
         "</section>")))

(defn next-hand-controls [view]
  (case (:phase view)
    :hand-complete
    "<div class=\"control-group\"><button data-new-hand=\"true\">New hand</button></div>"

    :game-over
    "<div class=\"control-group\"><button data-new-game=\"true\">New game</button></div>"

    nil))

(defn render-controls [view paused? pending-auto?]
  (let [active? (= (:you view) (:current-player view))]
    (str (or (bid-controls view active?) "")
         (or (trump-controls view active?) "")
         (or (auto-play-controls view active? paused? pending-auto?) "")
         (or (next-hand-controls view) ""))))

(defn render-game! []
  (let [{:keys [view room-id play-animation trick-popup queued-trick-popup bid-popup pending-card pending-auto?]} @app]
    (if-not view
      (html! (el "game-root") "<section class=\"panel empty-panel\"><h2>Open a table</h2></section>")
      (do
        (set-share-link! room-id)
        (html! (el "game-root")
               (str
                "<section class=\"table-grid\">"
                "<div class=\"panel table-panel\">"
               "<div class=\"panel-heading\"><p class=\"eyebrow\">Karbosh table</p>"
               "<h1>Room " (escape-html room-id) "</h1>"
               "<p class=\"status-line\">"
                (escape-html (phase-label (:phase view)))
                " / Current: " (escape-html (player-label view (:current-player view)))
                "</p></div>"
                "<div class=\"score-row\"><span>Team 1 <strong>" (get-in view [:scores 1] 0)
                "</strong></span><span>Team 2 <strong>" (get-in view [:scores 2] 0)
                "</strong></span></div>"
                (or (game-over-html view) "")
                (table-status-html view bid-popup)
                (table-surface-html view play-animation trick-popup queued-trick-popup)
                "<div class=\"controls\">"
                (render-controls view (or (some? trick-popup)
                                          (some? queued-trick-popup))
                                 pending-auto?)
                "</div>"
                (hand-panel-html view pending-card (or (some? trick-popup)
                                                       (some? queued-trick-popup)))
                (mobile-seat-roster-html view)
                "</div>"
                "</section>"))))))

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
      (render-game!)
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
    (render-game!)))

(defn show-trick-popup! [popup]
  (swap! app assoc
         :play-animation nil
         :queued-trick-popup nil
         :trick-popup popup)
  (render-game!)
  (js/setTimeout #(clear-trick-popup! (:id popup)) trick-popup-ms))

(defn clear-play-animation! [animation-id]
  (when (= animation-id (:id (:play-animation @app)))
    (if-let [popup (:queued-trick-popup @app)]
      (show-trick-popup! popup)
      (do
        (swap! app assoc :play-animation nil)
        (render-game!)))))

(defn bid-event [old-view new-view]
  (when old-view
    (let [old-count (count (:bids-this-hand old-view))
          new-bids (:bids-this-hand new-view)]
      (when (> (count new-bids) old-count)
        (nth new-bids old-count)))))

(defn clear-bid-popup! [popup-id]
  (when (= popup-id (:id (:bid-popup @app)))
    (swap! app assoc :bid-popup nil)
    (render-game!)))

(defn handle-server-message! [raw]
  (let [message (reader/read-string raw)]
    (case (:op message)
      :state
      (let [view (:view message)
            animation (played-card-event (:view @app) view)
            trick-winner (won-trick-event (:view @app) view)
            bid (bid-event (:view @app) view)
            now (.now js/Date)
            animation-id (when animation
                           (str now "-" (kw-name (:player animation))))
            popup-id (when trick-winner
                       (str now "-trick-winner"))
            popup (some-> trick-winner (assoc :id popup-id))
            queue-popup? (and animation popup)
            bid-popup-id (when bid
                           (str now "-bid-" (kw-name (:player bid))))]
        (swap! app assoc
               :room-id (:room-id message)
               :player (:player message)
               :view view
               :play-animation (some-> animation (assoc :id animation-id))
               :trick-popup (when-not queue-popup? popup)
               :queued-trick-popup (when queue-popup? popup)
               :bid-popup (some-> bid (assoc :id bid-popup-id))
               :pending-card nil
               :pending-auto? false
               :error nil)
        (save-session! (:room-id message) (:player message) (player-name))
        (render-status!)
        (render-game!)
        (when animation-id
          (js/setTimeout #(clear-play-animation! animation-id) play-animation-ms))
        (when (and popup (not queue-popup?))
          (js/setTimeout #(clear-trick-popup! popup-id) trick-popup-ms))
        (when bid-popup-id
          (js/setTimeout #(clear-bid-popup! bid-popup-id) bid-popup-ms)))

      :error
      (do
        (swap! app assoc
               :error (:message message)
               :pending-card nil
               :pending-auto? false)
        (render-status!))

      :pong nil
      nil)))

(defn connect! [after-open]
  (when-let [old (:socket @app)]
    (.close old))
  (let [socket (js/WebSocket. (ws-url))]
    (swap! app assoc :socket socket :connected? false :error nil)
    (set! (.-onopen socket)
          (fn []
            (swap! app assoc :connected? true)
            (render-status!)
            (after-open)))
    (set! (.-onclose socket)
          (fn []
            (swap! app assoc :connected? false)
            (render-status!)))
    (set! (.-onerror socket)
          (fn []
            (swap! app assoc :error "Connection error")
            (render-status!)))
    (set! (.-onmessage socket)
          #(handle-server-message! (.-data %)))))

(defn create-room! []
  (connect! #(send! {:op :create-room :name (player-name)})))

(defn join-room! [room-id player]
  (connect! #(send! {:op :join-room
                     :room-id room-id
                     :player player
                     :name (player-name)})))

(defn fill-bots! []
  (send! {:op :fill-bots}))

(defn auto-play! []
  (swap! app assoc :pending-auto? true)
  (render-game!)
  (send! {:op :auto-play}))

(defn bind-controls! []
  (.addEventListener (el "create-room") "click" create-room!)
  (.addEventListener (el "join-room") "click"
                     (fn []
                       (let [room-id (str/trim (.-value (el "join-room-id")))]
                         (join-room! room-id nil))))
  (.addEventListener (el "copy-link") "click"
                     (fn []
                       (when-let [text (not-empty (.-textContent (el "share-link")))]
                         (.. js/navigator -clipboard (writeText text)))))
  (.addEventListener (el "fill-bots") "click" fill-bots!)
  (.addEventListener (el "game-root") "click"
                     (fn [event]
                       (let [target (.-target event)]
                         (cond
                           (.hasAttribute target "data-bid")
                           (let [bid (keyword (.getAttribute target "data-bid"))]
                             (action! {:type :bid :bid-type bid}))

                           (.hasAttribute target "data-bid-value")
                           (action! {:type :bid
                                     :bid-type :bid
                                     :value (js/Number (.getAttribute target "data-bid-value"))})

                           (.hasAttribute target "data-trump")
                           (action! {:type :trump-selection
                                     :suit (reader/read-string (.getAttribute target "data-trump"))})

                           (.hasAttribute target "data-card")
                           (play-card! (reader/read-string (.getAttribute target "data-card")))

                           (.hasAttribute target "data-auto-play")
                           (auto-play!)

                           (.hasAttribute target "data-reshuffle-hand")
                           (action! {:type :reshuffle-hand})

                           (.hasAttribute target "data-new-game")
                           (action! {:type :new-game})

                           (.hasAttribute target "data-new-hand")
                           (action! {:type :new-hand}))))))

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
                                :error nil})
  (render-join-modal!)
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
                                        :error nil}
                                       {:room-id room
                                        :loading? false
                                        :preview nil
                                        :error (:message data)}))
                              (render-join-modal!)))))))
      (.catch (fn [_]
                (swap! app assoc :join-modal {:room-id room
                                              :loading? false
                                              :preview nil
                                              :error "Could not load this room."})
                (render-join-modal!)))))

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
  (render-status!)
  (render-game!)
  (if-let [room (query-room-param)]
    (prepare-shared-room! room)
    (when-let [room (stored-room-id)]
      (restore-saved-room! room))))

(set! (.-onload js/window) init!)
