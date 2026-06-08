(ns clojure-card-games.karbosh.client
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.hand-order :as hand-order]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [clojure-card-games.karbosh.hiccup :as h]))

(def fast-mode-storage-key "karbosh-fast-mode")

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

(defn stored-fast-mode? []
  (try
    (= "true" (.getItem js/localStorage fast-mode-storage-key))
    (catch :default _
      false)))

(defn persist-fast-mode! [enabled?]
  (try
    (.setItem js/localStorage fast-mode-storage-key (if enabled? "true" "false"))
    (catch :default _
      nil)))

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
         :fast-mode? (stored-fast-mode?)
         :last-reconnect-at 0
         :error nil}))

(def reconnect-throttle-ms 1200)

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
  (set! (.-innerHTML node) (if (string? content)
                             content
                             (h/render content))))

(defn text! [node content]
  (set! (.-textContent node) content))

(defn active-game-layout! [active?]
  (let [classes (.-classList (.-body js/document))]
    (.toggle classes "has-karbosh-game" active?)
    (.toggle classes "karbosh-fast-mode" (:fast-mode? @app))))

(defn timing-ms [k]
  (get (if (:fast-mode? @app) fast-timings normal-timings) k))

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

(defn fit-seat-name-node! [node]
  (let [style (.-style node)]
    (set! (.-fontSize style) "")
    (let [computed (js/getComputedStyle node)
          max-size (js/parseFloat (.-fontSize computed))
          min-size (fit-number-attr node "data-fit-min" 4.5)
          available (.-clientWidth node)
          measured (text-width (computed-font computed) (.-textContent node))]
      (when (and (pos? available) (pos? max-size) (pos? measured))
        (let [target (* max-size (/ (- available 1) measured))
              size (-> target
                       (min max-size)
                       (max min-size))]
          (set! (.-fontSize style) (str size "px")))))))

(defn fit-seat-names! []
  (let [nodes (.querySelectorAll js/document ".seat-name")]
    (doseq [idx (range (.-length nodes))]
      (fit-seat-name-node! (.item nodes idx)))))

(defn schedule-fit-seat-names! []
  (js/requestAnimationFrame
   (fn []
     (fit-seat-names!)
     (js/requestAnimationFrame fit-seat-names!))))

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

(def invalid-seed ::invalid-seed)

(defn set-error! [message]
  (swap! app assoc :error message)
  (render-status!))

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
           :title (when occupied? "Player options")}
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
                  :title (when occupied? "Player options")}
             (when dealer-seat? (dealer-chip-html))
             [:div {:class "seat-copy"}
              [:strong {:class "seat-name"
                        :data-fit-min 3.2}
               (or name (clojure.core/name id))]
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

(defn seat-popover-html [view player]
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

(defn render-seat-popover! []
  (when-let [root (el "seat-popover-root")]
    (let [{:keys [view seat-popover-player]} @app]
      (html! root (or (seat-popover-html view seat-popover-player)
                      "")))))

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

(defn auto-play-button [active? paused? pending?]
  [:button {:type "button"
            :class "auto-play-button"
            :data-auto-play true
            :disabled (or (not active?) paused? pending?)}
   "Auto Play"])

(defn hand-primary-action-button [view active? paused? pending?]
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
      (auto-play-button active? paused? pending?))))

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

(defn sort-hand-button [disabled?]
  [:button {:class "sort-hand-button"
            :type "button"
            :data-sort-hand true
            :disabled disabled?}
   "Sort"])

(defn fast-mode-button [enabled?]
  [:button {:class (str "fast-mode-button" (when enabled? " is-active"))
            :type "button"
            :data-fast-mode (if enabled? "false" "true")
            :aria-pressed (if enabled? "true" "false")}
   "Fast"])

(defn hand-panel-html [view pending-card paused? hand-order card-drag hand-animating? pending-auto?]
  (let [hand (displayed-hand view pending-card hand-order)
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
       (fast-mode-button (:fast-mode? @app))
       (hand-primary-action-button view active? paused? pending-auto?)]]
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
                      :disabled? (card-disabled? view hand card pending-card paused?)
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

(defn room-visibility-button [view]
  (let [public? (:public? view)]
    [:button {:class "visibility-button"
              :type "button"
              :data-room-public (if public? "false" "true")}
     (if public? "Make Private" "Make Public")]))

(defn table-top-actions [view]
  [:div {:class "table-top-actions"}
   (fill-bots-button)
   [:div {:class "table-room-actions"}
    (table-new-game-button)
    (room-visibility-button view)
    (leave-room-button)]])

(defn render-controls [view]
  (let [active? (= (:you view) (:current-player view))]
    (filter identity
            [(bid-controls view active?)])))

(defn render-trump-picker! []
  (when-not (:join-modal @app)
    (let [{:keys [view bid-popup]} @app
          active? (= (:you view) (:current-player view))
          delayed? (some? bid-popup)]
      (html! (el "modal-root")
             (or (when-not delayed?
                   (trump-picker-html view active?))
                 "")))))

(defn render-game! []
  (let [{:keys [view room-id play-animation trick-popup queued-trick-popup bid-popup
                fireworks hand-order card-drag hand-animating? pending-card pending-auto?]} @app]
    (active-game-layout! (some? view))
    (if-not view
      (html! (el "game-root") "")
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
                   (player-label view (:current-player view))]
                  (table-top-actions view)]
                 [:section {:class "score-summary" :aria-label "Total scores"}
                  [:span {:class "score-summary-label"} "Total scores"]
                  [:div {:class "score-row"}
                   [:span "Team 1 " [:strong (get-in view [:scores 1] 0)]]
                   [:span "Team 2 " [:strong (get-in view [:scores 2] 0)]]]]
                 (or (game-over-html view) "")
                 (when (panel-fireworks? fireworks)
                   (fireworks-html view fireworks))
                 (table-surface-html view
                                     play-animation
                                     trick-popup
                                     queued-trick-popup
                                     (when-not (panel-fireworks? fireworks)
                                       fireworks))
                 [:div {:class "play-controls-panel"}
                  (hand-panel-html view pending-card (or (some? trick-popup)
                                                        (some? queued-trick-popup))
                                   hand-order
                                   card-drag
                                   hand-animating?
                  pending-auto?)
                  [:div {:class "controls"}
                   (render-controls view)]]
                 [:div {:id "seat-popover-root"}]
                 (mobile-seat-roster-html view)]])))
    (render-seat-popover!)
    (schedule-fit-seat-names!)
    (render-trump-picker!)))

(defn show-seat-popover! [player]
  (swap! app assoc :seat-popover-player player)
  (render-seat-popover!))

(defn close-seat-popover! []
  (swap! app assoc :seat-popover-player nil)
  (render-seat-popover!))

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
  (js/setTimeout #(clear-trick-popup! (:id popup)) (timing-ms :trick-popup)))

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
    (render-game!)))

(defn clear-fireworks! [fireworks-id]
  (when (= fireworks-id (:id (:fireworks @app)))
    (swap! app assoc :fireworks nil)
    (render-game!)))

(defn fireworks-duration-ms [fireworks]
  (let [base (timing-ms :fireworks)]
    (if (= :double-karbosh (:kind fireworks))
      (js/Math.round (* 1.7 base))
      base)))

(defn show-fireworks! [fireworks]
  (when (and (= (:room-id fireworks) (:room-id @app))
             (= (:hand-index fireworks) (get-in @app [:view :hand-index])))
    (swap! app assoc :fireworks fireworks)
    (render-game!)
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
         :join-modal nil
         :hand-order nil
         :card-drag nil
         :hand-animating? false
         :suppress-card-click? false
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
      (let [old-view (:view @app)
            view (:view message)
            fast-mode? (true? (:fast-mode? view))
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
               :fast-mode? fast-mode?
               :error nil)
        (persist-fast-mode! fast-mode?)
        (set-room-url! (:room-id message))
        (save-session! (:room-id message) (:player message) (player-name))
        (render-status!)
        (render-game!)
        (when animation-id
          (js/setTimeout #(clear-play-animation! animation-id) (timing-ms :play-animation)))
        (when (and popup (not queue-popup?))
          (js/setTimeout #(clear-trick-popup! popup-id) (timing-ms :trick-popup)))
        (when bid-popup-id
          (js/setTimeout #(clear-bid-popup! bid-popup-id) (timing-ms :bid-popup)))
        (when fireworks
          (js/setTimeout #(show-fireworks! fireworks)
                         (fireworks-delay-ms fireworks animation popup))))

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
            (render-status!)
            (after-open)))
    (set! (.-onclose socket)
          (fn []
            (when (= socket (:socket @app))
              (swap! app assoc :connected? false)
              (render-status!))))
    (set! (.-onerror socket)
          (fn []
            (when (= socket (:socket @app))
              (swap! app assoc :error "Connection error")
              (render-status!))))
    (set! (.-onmessage socket)
          (fn [event]
            (when (= socket (:socket @app))
              (handle-server-message! (.-data event)))))))

(defn create-room! []
  (let [seed (create-room-seed)]
    (if (= invalid-seed seed)
      (set-error! "Seed must be an integer")
      (connect! #(send! (cond-> {:op :create-room
                                 :name (player-name)
                                 :public? (create-public-room?)
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
      (render-status!)
      (join-room! room-id player))))

(defn reconnect-on-wake! []
  (when-not (.-hidden js/document)
    (reconnect-visible-room!)))

(defn fill-bots! []
  (send! {:op :fill-bots}))

(defn auto-play! []
  (swap! app assoc :pending-auto? true)
  (render-game!)
  (send! {:op :auto-play}))

(defn leave-room! []
  (send! {:op :leave-room}))

(defn kick-player! [player]
  (close-seat-popover!)
  (send! {:op :kick-player :player (kw-name player)}))

(defn set-room-visibility! [public?]
  (send! {:op :set-room-visibility :public? public?})
  (js/setTimeout load-public-rooms! 500))

(defn clear-hand-animation! []
  (when (:hand-animating? @app)
    (swap! app assoc :hand-animating? false)
    (render-game!)))

(defn pulse-hand-animation! []
  (swap! app assoc :hand-animating? true)
  (js/setTimeout clear-hand-animation! (timing-ms :hand-animation)))

(defn sort-hand! []
  (when-let [view (:view @app)]
    (let [sorted-hand (hand-order/sorted-hand (:hand view) (:trump view))]
      (swap! app assoc
             :hand-order {:hand-index (:hand-index view)
                          :cards sorted-hand})
      (pulse-hand-animation!)
      (render-game!))))

(defn set-fast-mode! [enabled?]
  (persist-fast-mode! enabled?)
  (swap! app assoc :fast-mode? enabled?)
  (render-game!)
  (send! {:op :set-fast-mode :fast-mode? enabled?}))

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

(defn rect-center [rect]
  [(+ (.-left rect) (/ (.-width rect) 2))
   (+ (.-top rect) (/ (.-height rect) 2))])

(defn card-node-distance [node x y]
  (let [rect (.getBoundingClientRect node)
        [cx cy] (rect-center rect)
        dx (- x cx)
        dy (- y cy)]
    (+ (* dx dx) (* dy dy))))

(defn nearest-card-node [x y]
  (when-let [nodes (seq (hand-card-nodes))]
    (apply min-key #(card-node-distance % x y) nodes)))

(defn after-card? [node x y]
  (let [rect (.getBoundingClientRect node)
        [cx cy] (rect-center rect)
        dx (- x cx)
        dy (- y cy)]
    (if (> (js/Math.abs dy) (* 0.65 (.-height rect)))
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
                         :pointer-id (.-pointerId event)
                         :start-x (.-clientX event)
                         :start-y (.-clientY event)
                         :dragging? false})
      nil)))

(defn update-card-drag! [event]
  (when-let [{:keys [card pointer-id start-x start-y dragging?] :as drag} (:card-drag @app)]
    (when (= pointer-id (.-pointerId event))
      (let [x (.-clientX event)
            y (.-clientY event)
            dx (- x start-x)
            dy (- y start-y)
            moved? (> (js/Math.sqrt (+ (* dx dx) (* dy dy))) drag-threshold-px)]
        (when (or dragging? moved?)
          (.preventDefault event)
          (let [target (nearest-card-node x y)
                target-index (some-> target read-hand-index-attr)
                after? (when target (after-card? target x y))
                before @app
                after (-> before
                          (assoc :card-drag (assoc drag :dragging? true))
                          (cond-> target-index
                            (reorder-dragged-card target-index after?)))]
            (when (not= before after)
              (reset! app after)
              (render-game!))))))))

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
        (pulse-hand-animation!)
        (render-game!)))))

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
                           (auto-play!)))))
  (.addEventListener (el "game-root") "pointerdown" begin-card-drag!)
  (.addEventListener js/window "pointermove" update-card-drag!)
  (.addEventListener js/window "pointerup" finish-card-drag!)
  (.addEventListener js/window "pointercancel" finish-card-drag!)
  (.addEventListener js/window "resize" schedule-fit-seat-names!)
  (.addEventListener js/window "focus" reconnect-on-wake!)
  (.addEventListener js/window "pageshow" reconnect-on-wake!)
  (.addEventListener js/document "visibilitychange" reconnect-on-wake!)
  (.addEventListener (el "game-root") "click"
                     (fn [event]
                       (let [target (.-target event)
                             card-target (card-button-node target)
                             seat-target (closest target "[data-seat-player]")
                             close-seat-target (closest target "[data-close-seat-popover]")
                             kick-target (closest target "[data-kick-player]")
                             fast-target (closest target "[data-fast-mode]")
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

                           seat-target
                           (do
                             (.preventDefault event)
                             (show-seat-popover!
                              (keyword (.getAttribute seat-target "data-seat-player"))))

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
                           (auto-play!)

                           (.hasAttribute target "data-sort-hand")
                           (sort-hand!)

                           fast-target
                           (set-fast-mode!
                            (= "true" (.getAttribute fast-target "data-fast-mode")))

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
