(ns chainreact-clj.interface
  (:use [chainreact-clj.core]
        [clojure.pprint]
        [clojure.string :only (split)]))

(defn split-input 
  [input]
  (map #(read-string %) (split input #" ")))

(defn draw-board 
  [board board-size]
  ;(pprint (for [x (range board-size)] 
  ;            (repeat board-size "++++") 
  ;          ))
  (println board)
  )

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
                              board-size)]
              (draw-board new-board board-size) 
              (recur (read-line) new-board player))))))
