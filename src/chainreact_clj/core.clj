(ns chainreact-clj.core
  (:require [clojure.data.json :as json])
  (:use seesaw.core)
)

(def board-size 8)
(def player-size 2)
(def orb-counts {:corner 2 :edge 3 :all 4})
  
;Given row and column, get an element from a 2D vector
(defn get-elem-from-2Dvector 
  [v row col] 
  (nth (nth v row) col))

;Given row and column, return how many maximum orbs a cell can have
;Corners have 2, Edges 3 and Rest 4
(defn get-max-orbs
  [row col]
  (cond
    ;Check if valid row and column, if not return nil
    (or (< row 0) (< col 0) (>= row board-size) (>= col board-size))
      nil
    ;If row and col are (0,0) (0,boardsize-1) (boardsize-1,0) (boardsize-1,boardsize-1)
    ;It's a corner so return 2   
    (and (= (mod row (- board-size 1))  0)  (= (mod col (- board-size 1)) 0) )
      (:corner orb-counts)
    ;One of row or column being 0 or boardsize-1 and 
    ;other not being one of these two decides edge
    (or 
      (and (= (mod row (- board-size 1))  0)  (not= (mod col (- board-size 1)) 0) )
      (and (not= (mod row (- board-size 1))  0)  (= (mod col (- board-size 1)) 0) ))
      (:edge orb-counts)
    ;Anything else returns 4
    :else
      (:all orb-counts)
    ))

(defn -main [& args])
