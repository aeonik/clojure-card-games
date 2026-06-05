(ns clojure-card-games.karbosh.client
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [clojure-card-games.karbosh.hiccup :as h]))

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
         :public-rooms {:loading? false
                        :rooms []
                        :error nil}
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
  (set! (.-innerHTML node) (if (string? content)
                             content
                             (h/render content))))

(defn text! [node content]
  (set! (.-textContent node) content))

(defn active-game-layout! [active?]
  (.toggle (.-classList (.-body js/document)) "has-karbosh-game" active?))

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

(defn player-name []
  (let [input (el "player-name")
        value (str/trim (.-value input))]
    (if (str/blank? value) "Player" value)))

(defn create-public-room? []
  (boolean (some-> (el "create-public-room") .-checked)))

(defn room-page-url [room-id]
  (let [url (js/URL. (.-href js/location))]
    (.set (.-searchParams url) "room" room-id)
    (.-href url)))

(defn set-share-link! [room-id]
  (text! (el "share-link") (room-page-url room-id)))

(defn set-room-url! [room-id]
  (.replaceState js/history nil "" (room-page-url room-id)))

(defn clear-room-url! []
  (let [url (js/URL. (.-href js/location))]
    (.delete (.-searchParams url) "room")
    (.replaceState js/history nil "" (.-href url))))

(defn clear-session! []
  (.removeItem js/localStorage "karbosh-room")
  (.removeItem js/localStorage "karbosh-player"))

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

(defn public-rooms-url []
  "/karbosh/api/public-rooms")

(declare close-join-modal!
         join-from-modal!
         select-join-player!
         prepare-shared-room!
         load-public-rooms!
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
              :data-join-player (kw-name id)}
     [:span "Seat " (last (kw-name id))]
     [:strong (preview-seat-name player)]
     [:em (str (team-label team) " / " (preview-seat-status player))]]))

(defn team-preview-html [selected-player team players]
  [:section {:class "join-team"}
   [:h3 (team-label team)]
   [:div {:class "join-seat-grid"}
    (for [player players]
      (preview-seat-html selected-player player))]])

(defn preview-players-html [preview]
  (let [selected-player (:player (:join-modal @app))
        team-groups (group-by :team (:players preview))
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

(defn render-public-rooms! []
  (when-let [root (el "public-rooms-root")]
    (let [{:keys [loading? rooms error]} (:public-rooms @app)]
        (html! root
               (cond
                 error
                 [:p {:class "public-rooms-empty"} error]

                 (seq rooms)
                 [:div {:class "public-room-list"}
                  (for [room rooms] (public-room-html room))]

                 loading?
                 [:p {:class "public-rooms-empty"} "Loading rooms..."]

                 :else
                 [:p {:class "public-rooms-empty"} "No public rooms."])))))

(defn render-join-modal! []
  (let [{:keys [room-id loading? preview player error]} (:join-modal @app)]
    (html! (el "modal-root")
           (if room-id
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
                  (preview-players-html preview)])
               [:label
                [:span "Name"]
                [:input {:id "join-modal-name"
                         :type "text"
                         :maxlength "24"
                         :value (player-name)}]]
               [:div {:class "join-modal-actions"}
                [:button {:id "join-modal-cancel" :type "button"} "Cancel"]
                [:button {:id "join-modal-submit"
                          :type "button"
                          :disabled (join-modal-disabled? loading? player error)}
                 "Join Table"]]
               ]]
             "")))
  (when (:join-modal @app)
    (when-let [cancel (el "join-modal-cancel")]
      (.addEventListener cancel "click" close-join-modal!))
    (when-let [submit (el "join-modal-submit")]
      (.addEventListener submit "click" join-from-modal!))
    (let [buttons (.querySelectorAll (el "modal-root") "[data-join-player]")]
      (dotimes [n (.-length buttons)]
        (let [button (.item buttons n)]
          (.addEventListener button "click"
                             (fn []
                               (select-join-player!
                                (keyword (.getAttribute button "data-join-player"))))))))
    (when-let [input (el "join-modal-name")]
      (.focus input)
      (.select input)
      (.addEventListener input "keydown"
                         (fn [event]
                           (when (= "Enter" (.-key event))
                             (join-from-modal!)))))))

(defn select-join-player! [player]
  (swap! app assoc-in [:join-modal :player] player)
  (render-join-modal!))

(defn close-join-modal! []
  (swap! app assoc :join-modal nil)
  (render-join-modal!))

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

(defn player-seat-html [view {:keys [id team name connected? bot? active? hand-count] :as seat}]
  (let [current? (= id (:current-player view))
        you? (= id (:you view))]
    [:div {:class (str "player-seat " (player-class id)
                       (when connected? " is-connected")
                       (when bot? " is-bot")
                       (when (false? active?) " is-sitting-out")
                       (when current? " is-current")
                       (when you? " is-you"))}
     [:div
      [:strong (or name (clojure.core/name id))]
      [:small (team-label team) " / " hand-count " cards / " (seat-state-label seat)]
      (when-let [bid (latest-bid view id)]
        [:em {:class "bid-chip"} (bid-label bid)])]
     [:div {:class "seat-hand-backs"} (hand-backs-html hand-count)]
     (when current? [:em {:class "turn-badge"} "Current"])]))

(defn mobile-seat-roster-html [view]
  (into [:ul {:class "mobile-seat-roster" :aria-label "Players"}]
        (for [{:keys [id team name connected? bot? active? hand-count] :as seat} (:players view)]
          (let [current? (= id (:current-player view))
                you? (= id (:you view))]
            [:li {:class (str (player-class id)
                              (when connected? " is-connected")
                              (when bot? " is-bot")
                              (when (false? active?) " is-sitting-out")
                              (when current? " is-current")
                              (when you? " is-you"))}
             [:div
              [:strong (or name (clojure.core/name id))]
              [:span (team-label team) " / " hand-count " cards / "
               (seat-state-label seat)]]
             (if-let [bid (latest-bid view id)]
               [:em (bid-label bid)]
               [:em "--"])]))))

(defn trick-card-html [view {:keys [player card]}]
  [:li {:class (str "trick-card " (player-class player))}
   [:span (player-label view player)]
   [:strong {:class (str "card-face" (card-suit-class card))}
    (card-label card)]])

(defn play-animation-html [view {:keys [player card]}]
  (when (and player card)
    [:li {:class (str "trick-card is-animating " (player-class player) " from-" (player-class player))}
     [:span (player-label view player)]
     [:strong {:class (str "card-face" (card-suit-class card))}
      (card-label card)]]))

(defn trick-html [view trick animation]
  (let [cards (map #(trick-card-html view %) trick)
        animation (play-animation-html view animation)]
    (if (or (seq trick) animation)
      (vec (concat cards (when animation [animation])))
      [[:li {:class "trick-empty"} [:span "No cards played"]]])))

(defn same-play? [a b]
  (and (= (:player a) (:player b))
       (= (:card a) (:card b))))

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

(defn table-surface-html [view animation trick-popup queued-trick-popup]
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
            (or (trick-popup-html view trick-popup) "")]))))

(defn card-button [{:keys [card disabled?]}]
  [:button {:class (str "card-button" (card-suit-class card))
            :type "button"
            :data-card (pr-str card)
            :disabled disabled?}
   (card-label card)])

(def auto-play-phases
  #{:bidding
    :trump-selection
    :karbosh-donation
    :karbosh-discard
    :trick-playing})

(defn bid-controls [view active?]
  (when (= :bidding (:phase view))
    (let [disabled (not active?)]
      [:div {:class "control-group"}
       [:button {:type "button"
                 :data-bid "pass"
                 :disabled disabled}
        "Pass"]
       (for [n (range 1 9)]
         [:button {:type "button"
                   :data-bid-value n
                   :disabled disabled}
          n])
       [:button {:type "button"
                 :data-bid "karbosh"
                 :disabled disabled}
        "Karbosh"]
       [:button {:type "button"
                 :data-bid "double-karbosh"
                 :disabled disabled}
        "Double"]])))

(defn trump-controls [view active?]
  (when (= :trump-selection (:phase view))
    [:div {:class "control-group trump-control-group"}
     (for [suit cards/suits]
       [:button {:type "button"
                 :class (str "trump-button" (suit-class suit))
                 :data-trump (pr-str suit)
                 :disabled (not active?)}
        (cards/suit->str suit)])]))

(defn auto-play-controls [view active? paused? pending?]
  (when (auto-play-phases (:phase view))
    [:div {:class "control-group auto-play-control"}
     [:button {:type "button"
               :class "auto-play-button"
               :data-auto-play true
               :disabled (or (not active?) paused? pending?)}
      "Auto Play"]]))

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
    [:section {:class "hand-panel"}
     [:div {:class "hand-heading"}
      [:h2 (hand-title view)]
      [:span (count hand) " cards"]]
     [:div {:class "hand-row"}
      (for [card hand]
        (card-button {:card card
                      :disabled? (card-disabled? view hand card pending-card paused?)}))]]))

(defn game-over-html [view]
  (when (= :game-over (:phase view))
    [:section {:class "game-over-panel"}
     [:span "Game over"]
     [:strong (team-label (:winner view)) " wins"]
     [:em "Final score " (get-in view [:scores 1] 0) " / " (get-in view [:scores 2] 0)]]))

(defn next-hand-controls [view]
  (case (:phase view)
    :hand-complete
    [:div {:class "control-group"}
     [:button {:type "button" :data-new-hand true} "New hand"]]
    :game-over
    [:div {:class "control-group"}
     [:button {:type "button" :data-new-game true} "New game"]]

    nil))

(defn leave-room-controls []
  [:div {:class "control-group leave-room-control"}
   [:button {:class "leave-room-button"
             :type "button"
             :data-leave-room true}
    "Leave Room"]])

(defn room-visibility-controls [view]
  (let [public? (:public? view)]
    [:div {:class "control-group room-visibility-control"}
     [:button {:class "visibility-button"
               :type "button"
               :data-room-public (if public? "false" "true")}
      (if public? "Make Private" "Make Public")]]))

(defn render-controls [view paused? pending-auto?]
  (let [active? (= (:you view) (:current-player view))]
    (filter identity
            [(bid-controls view active?)
             (trump-controls view active?)
             (auto-play-controls view active? paused? pending-auto?)
             (next-hand-controls view)
             (room-visibility-controls view)
             (leave-room-controls)])))

(defn render-game! []
  (let [{:keys [view room-id play-animation trick-popup queued-trick-popup bid-popup pending-card pending-auto?]} @app]
    (active-game-layout! (some? view))
    (if-not view
      (html! (el "game-root")
             [:section {:class "panel empty-panel"}
              [:h2 "Open a table"]])
      (do
        (set-share-link! room-id)
        (html! (el "game-root")
               [:section {:class "table-grid"}
                [:div {:class "panel table-panel"}
                 [:div {:class "panel-heading"}
                  [:p {:class "eyebrow"} "Karbosh table"]
                  [:h1 "Room " room-id]
                  [:p {:class "status-line"}
                   (phase-label (:phase view)) " / Current: "
                   (player-label view (:current-player view))]]
                 [:section {:class "score-summary" :aria-label "Total scores"}
                  [:span {:class "score-summary-label"} "Total scores"]
                  [:div {:class "score-row"}
                   [:span "Team 1 " [:strong (get-in view [:scores 1] 0)]]
                   [:span "Team 2 " [:strong (get-in view [:scores 2] 0)]]]]
                 (or (game-over-html view) "")
                 (table-status-html view bid-popup)
                 (table-surface-html view play-animation trick-popup queued-trick-popup)
                 (hand-panel-html view pending-card (or (some? trick-popup)
                                                       (some? queued-trick-popup)))
                 [:div {:class "controls"}
                  (render-controls view (or (some? trick-popup)
                                           (some? queued-trick-popup))
                                   pending-auto?)]
                 (mobile-seat-roster-html view)]])))))

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
         :join-modal nil
         :pending-card nil
         :pending-auto? false
         :error message)
  (render-status!)
  (render-game!)
  (render-join-modal!))

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
        (set-room-url! (:room-id message))
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
      :left-room
      (reset-room-state! nil)

      :room-closed
      (reset-room-state! "Room closed")

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
  (connect! #(send! {:op :create-room
                     :name (player-name)
                     :public? (create-public-room?)})))

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

(defn leave-room! []
  (send! {:op :leave-room}))

(defn set-room-visibility! [public?]
  (send! {:op :set-room-visibility :public? public?})
  (js/setTimeout load-public-rooms! 500))

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
                           (when (.hasAttribute target "data-public-room")
                             (prepare-shared-room!
                              (.getAttribute target "data-public-room")))))))
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
                                :player nil
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
                                        :player (default-join-player data)
                                        :error nil}
                                       {:room-id room
                                        :loading? false
                                        :preview nil
                                        :player nil
                                        :error (:message data)}))
                              (render-join-modal!)))))))
      (.catch (fn [_]
                (swap! app assoc :join-modal {:room-id room
                                              :loading? false
                                              :preview nil
                                              :player nil
                                              :error "Could not load this room."})
                (render-join-modal!)))))

(defn load-public-rooms! []
  (swap! app update :public-rooms
         (fn [public-rooms]
           (assoc public-rooms :loading? true :error nil)))
  (render-public-rooms!)
  (-> (js/fetch (public-rooms-url))
      (.then (fn [response]
               (-> (.text response)
                   (.then (fn [body]
                            (let [data (reader/read-string body)]
                              (swap! app assoc
                                     :public-rooms
                                     (if (:ok data)
                                       {:loading? false
                                        :rooms (:rooms data)
                                        :error nil}
                                       {:loading? false
                                        :rooms []
                                        :error "Could not load public rooms."}))
                              (render-public-rooms!)))))))
      (.catch (fn [_]
                (swap! app assoc :public-rooms {:loading? false
                                                :rooms []
                                                :error "Could not load public rooms."})
                (render-public-rooms!)))))

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
  (render-public-rooms!)
  (load-public-rooms!)
  (js/setInterval load-public-rooms! 8000)
  (if-let [room (query-room-param)]
    (if (and (stored-player)
             (same-room-id? room (stored-room-id)))
      (restore-saved-room! room)
      (prepare-shared-room! room))
    (when-let [room (stored-room-id)]
      (restore-saved-room! room))))

(set! (.-onload js/window) init!)
