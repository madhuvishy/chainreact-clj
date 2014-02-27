(ns chainreact-clj.core
  (:require [clojure.data.json :as json])
  (:use seesaw.core)
)

(def board-size 8)
(def player-size 2)
(def initial-player 1)

(def orb-counts {:corner 2 :edge 3 :all 4})
  
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

;Given a row and a column returns the initial state of the cell
(defn get-initial-state
  [row col]
  (let [max-orb-count (get-max-orbs row col)]
    {:owner nil :max-orbs max-orb-count :curr-orbs nil}
    ))

;Given a row and column and the player returns an altered version of the board
;Cases:
; 1. If the row doesn't exist, assoc row and column, set player, max orbs, set current orbs as 1 and return
; 2. If row exists but column doesnt, add column, set player, max orbs, set current orbs as 1 and return
; finally, if old-board doesn't have the specific row col value, add it, set player, max orbs, set current orbs as 1 and return
(defn get-new-board
  [old-board row col player]
  (cond
    (= (old-board row) nil) 
      (assoc old-board row {col {:owner player :max-orbs (get-max-orbs row col) :curr-orbs 1}})
    (= ((old-board row) col) nil)
      (assoc old-board row (assoc (old-board row) col {:owner player :max-orbs (get-max-orbs row col) :curr-orbs 1}))
    ))

(defn -main [& args])
