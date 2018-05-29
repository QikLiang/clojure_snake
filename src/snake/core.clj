(ns snake.core
  (:gen-class)
  (:require [clojure.tools.namespace.repl]))
(def re clojure.tools.namespace.repl/refresh)
(use 'seesaw.core
     'seesaw.graphics
     'seesaw.color
     'clojure.repl)

(def gridWidth 30)
(def gridHeight 20)
(def radius 15)
(def border 20)
(def gameWidth (* gridWidth radius))
(def gameHeight (* gridHeight radius))

(defrecord Game [direction snake fruit progress])
(defn newFruit [game]
  (assoc game
    :fruit {:x (rand-int gridWidth)
            :y (rand-int gridHeight)}))
(defn newGame []
  (newFruit
    (Game. :down '({:x 1 :y 0} {:x 0 :y 0}) 0 :started)))

(defn new-head [head direction]
  (case direction
    :up (update head :y dec)
    :down (update head :y inc)
    :left (update head :x dec)
    :right (update head :x inc)))

(defn tick [game]
  "One time tick in a game"
  (let [head (new-head (first (:snake game))
                       (:direction game))
        eaten (= head (:fruit game))]
    ; game over if head meets body
    (-> (assoc game :progress
               (if (some #{head} (:snake game))
                 :loss :started))
        ; put head on
        (update :snake conj head)
        ; take one off tail unless eat fruit
        (update :snake (if eaten identity drop-last))
        )))

(def game (newGame))

(defn opp-dir [a b]
  "true if two directions are opposite"
  ; use (conj #{a} b) in case (= a b)
  (contains? #{#{:up :down} #{:left :right}} (conj #{a} b)))

(defn change-dir [game direction]
  "change direction of snake if it's not 180 from current
  direction"
  (if (opp-dir direction (:direction game))
    game
    (assoc game :direction direction)))

(defn drawCircle [{x :x y :y}]
  (ellipse (+ border (* radius x))
           (+ border (* radius y)) radius))

(defn paintGame [c g]
  (draw g (rect border border gameWidth gameHeight)
        (style :foreground :white))
  (doseq [dot (:snake game)]
    (draw g (drawCircle dot)
          (style :background :yellow)))
  (draw g (drawCircle (:fruit game))
        (style :background :red)))

(defn on-key-press [e]
  (println
    (change-dir game
                (case (java.awt.event.KeyEvent/getKeyText
                        (.getKeyCode e))
                  "Left" :left
                  "Right" :right
                  "Up" :up
                  "Down" :down
                  (.getKeyCode e)
                  ))))

(defn gameGui [game]
  "create a gui for a game instance"
  (let [c (canvas :id :canvas :background "black"
                  :size [(+ (* 2 border) gameWidth) :by
                         (+ (* 2 border) gameHeight)]
                  :paint paintGame)
        f (frame :title "Snake --."
                 :content c
                 :listen [:key-released on-key-press])]
    (-> f
        pack!
        show!)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (gameGui game))
