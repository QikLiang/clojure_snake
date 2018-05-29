(ns snake.core
  (:gen-class)
  (:require [clojure.tools.namespace.repl]))
(def re clojure.tools.namespace.repl/refresh)

(def width 30)
(def height 20)
(def radius 15)

(defrecord Game [direction snake fruit progress])
(defn newFruit [game]
  (assoc game
    :fruit {:x (rand-int width), :y (rand-int height)}))
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

(def g (newGame))

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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
