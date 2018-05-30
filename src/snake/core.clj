(ns snake.core
  (:gen-class)
  (:require [clojure.core.async :as a
             :refer [>!! <! chan go]]))
(use 'seesaw.core
     'seesaw.graphics)

; debug mode
(if true
  (do
    (use 'clojure.repl)
    (require 'clojure.tools.namespace.repl)
    (def re clojure.tools.namespace.repl/refresh)))

(def gridWidth 30)
(def gridHeight 20)
(def radius 15)
(def border 20)
(def gameWidth (* gridWidth radius))
(def gameHeight (* gridHeight radius))

(defrecord Game [direction prev-dir snake fruit progress])
(defn newFruit [game]
  (let [snake (:snake game)
        in-snake (fn [node] (some #{node} snake))]
    (assoc game :fruit
           ; make sure snake not full, otherwise infinite loop
           (if (< (count (:snake game))
                  (* gridWidth gridHeight))
             ; make one randomly until not in snake
             ; efficiency even at worse case is
             ; comparable to exhaustive iteration
             (->> (fn [] {:x (rand-int gridWidth)
                          :y (rand-int gridHeight)})
                  (repeatedly)
                  (drop-while in-snake)
                  (first))))))
(defn newGame []
  (newFruit
    (Game. :right :right
           '({:x 1 :y 0} {:x 0 :y 0}) 0 :started)))

(defn new-head [head direction]
  (case direction
    :up (update head :y dec)
    :down (update head :y inc)
    :left (update head :x dec)
    :right (update head :x inc)))

(defn game-over [game]
  (let [head (new-head (first (:snake game))
                       (:direction game))]
    (not (and
           ; border inbound
           (<= 0 (:x head))
           (<= 0 (:y head))
           (< (:x head) gridWidth)
           (< (:y head) gridHeight)
           ; not running back on itself
           (nil? (some #{head} (:snake game)))))))

(defn tick [game]
  "One time tick in a game"
  (let [head (new-head (first (:snake game))
                       (:direction game))
        eaten (= head (:fruit game))]
    ; game over if head meets body
    (-> (assoc game :progress
               (if (game-over game)
                 :loss :started))
        ; put head on
        (update :snake conj head)
        ; take one off tail unless eat fruit
        (update :snake (if eaten identity drop-last))
        ; make new fruit if old one eaten
        ((if eaten newFruit identity))
        (assoc :prev-dir (:direction game))
        )))

(defn opp-dir [a b]
  "true if two directions are opposite"
  ; use (conj #{a} b) in case (= a b)
  (contains? #{#{:up :down} #{:left :right}} (conj #{a} b)))

(defn change-dir [game direction]
  "change direction of snake if it's not 180 from current
  direction"
  (if (or (nil? direction)
          (opp-dir direction (:prev-dir game)))
    game
    (assoc game :direction direction)))

(defn drawCircle [{x :x y :y}]
  (ellipse (+ border (* radius x))
           (+ border (* radius y)) radius))

(defn paintGame [game]
  (fn [c g]
    (draw g (rect border border gameWidth gameHeight)
          (style :foreground :white))
    (doseq [dot (:snake @game)]
      (draw g (drawCircle dot)
            (style :background :yellow)))
    (draw g (drawCircle (:fruit @game))
          (style :background :red))))

(defn game-keys-handler [game]
  (fn [e] (swap! game
                 change-dir
                 (case (java.awt.event.KeyEvent/getKeyText
                         (.getKeyCode e))
                   "Left" :left
                   "Right" :right
                   "Up" :up
                   "Down" :down
                   nil))))

(defn gameGui [game]
  "create a gui for a game instance"
  (let [c (canvas :id :canvas :background "black"
                  :size [(+ (* 2 border) gameWidth) :by
                         (+ (* 2 border) gameHeight)]
                  :paint (paintGame game))
        f (frame :title "Snake --."
                 :content c
                 :listen [:key-pressed
                          (game-keys-handler game)])]
    (-> f
        pack!
        show!)
    ; return canvas for repaint access
    c))

(def g (atom (newGame)))

(defn -main
  [& args]
  (let [game (atom (newGame))
        c (gameGui game)
        game-end (chan)
        t (timer (fn [e]
                   (time (swap! game tick))
                   (repaint! c)
                   (if (= :loss (:progress @game))
                     (>!! game-end 0)))
                 :delay 300)]
    (go
      (<! game-end)
      (println "game ended")
      (println @game)
      (.stop t))))
