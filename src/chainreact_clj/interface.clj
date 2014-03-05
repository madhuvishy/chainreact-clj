(ns chainreact-clj.interface
  (:use [chainreact-clj.core]
        [clojure.pprint]
        [clojure.string :only (split)]))

(defn split-input 
  [input]
  (map #(read-string %) (split input #" ")))

(defn get-print-value
  [cell]
  (let [owner (:owner cell)
        curr (:curr-orbs cell)
        maxm (:max-orbs cell)]
    (str 
      (apply str (repeat curr owner)) 
      (apply str (repeat (- maxm curr) "_")))))


(defn get-row-values
  [row board board-size]
  (for [col (range board-size)] 
    (let [cell (get-in board [row col])] 
      (if (= cell nil) 
        "____" 
        (get-print-value cell)))))


(defn draw-board 
  [board board-size]
  (pprint (for [row (range board-size)]
            (get-row-values row board board-size)))
  )

(defn get-next-player
  [old-player player-size]
  (if (= player-size (inc old-player))
        0
        (inc old-player)))   


(defn -main [& args]
  (let [board-size (read-string (first args))
        player-size (read-string (second args))]
        (draw-board {} board-size) 
        (loop [input (read-line)
               board {}
               player 0]
          (when-not (= ":q" input)
            (let [new-board (get-new-board 
                              board 
                              (first (split-input input))
                              (second (split-input input))
                              player
                              board-size)
                  new-player (if (= new-board board) 
                               player
                               (get-next-player player player-size))]
              (draw-board new-board board-size) 
              (recur (read-line) new-board new-player ))))))
