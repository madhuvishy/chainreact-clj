(ns chainreact-clj.core)

;;Defines a map of the possible orb counts for a type of cell
(def orb-counts {:corner 2 :edge 3 :all 4})

;;Defines a map of the 4 possible move combinations for every row, col pair
(def moves '({:x -1 :y 0} {:x 1 :y 0} {:x 0 :y -1} {:x 0 :y 1}))

(defn new-board [board-size]
  (with-meta {} {:board-size board-size}))

(defn get-board-size [board]
  (if (map? board)
    (:board-size (meta board))
    (throw (Exception. (str "Invalid board, got " (type board))))))

(declare get-new-board)

;;Checks if a row and column are valid and returns a Boolean result
(defn is-valid-rc
  [row col board-size]
  (and (>= row 0) (< row board-size) (>= col 0) (< col board-size)))

;;Given row and column, return how many maximum orbs a cell can have
;;Corners have 2, Edges 3 and Rest 4
(defn get-max-orbs
  [row col board-size]
  (cond
   ;;Check if valid row and column, if not return nil
   (not (is-valid-rc row col board-size))
   nil
   ;;If row and col are (0,0) (0,boardsize-1) (boardsize-1,0) (boardsize-1,boardsize-1)
   ;;It's a corner so return 2
   (and (= (mod row (- board-size 1))  0)  (= (mod col (- board-size 1)) 0) )
   (:corner orb-counts)
   ;;One of row or column being 0 or boardsize-1 and
   ;;other not being one of these two decides edge
   (or
    (and (= (mod row (- board-size 1))  0)  (not= (mod col (- board-size 1)) 0) )
    (and (not= (mod row (- board-size 1))  0)  (= (mod col (- board-size 1)) 0) ))
   (:edge orb-counts)
   ;;Anything else returns 4
   :else
   (:all orb-counts)
   ))

;;Creates a replicated list of row col maps
(defn rc-values [row col]
  (replicate 4 {:x row :y col}))

;;Sums each move combination with the row col pair and returns all moves
(defn all-moves
  [row col]
  (map
   (fn [move rc]
     {:x (+ (:x move)(:x rc))
      :y (+ (:y move) (:y rc))})
   moves
   (rc-values row col)))

;;Filters the result of all-moves and returns only valid ones
(defn valid-moves
  [row col board-size]
  (filter
   (fn [move]
     (and
      (>= (:x move) 0) (>= (:y move) 0)
      (< (:x move) board-size) (< (:y move) board-size)))
   (all-moves row col)))


;; THE CHAIN REACTION- The Magic Happens Here
;; For every possible valid move around the given cell, increment curr orbs, set owner to player and recurse

(defn chain-react
  [old-board row col player]
  (let [moves-list (valid-moves row col (get-board-size old-board))]
    (loop [moves moves-list index 0 board old-board]
      (if (>= index (count moves-list))
        board
        (when (seq moves)
          (let [x (:x (first moves))
                y (:y (first moves))]
            (recur (rest moves) (inc index) (get-new-board old-board [x y] player true))))
        ))))


;; Given a row and column and the player returns an altered version of the board
;; Cases:
;; IF Valid Row and Column, then:
;; 1. If the row doesn't exist, assoc row and column, set player, max orbs, set current orbs as 1 and return
;; 2. If row exists but column doesnt, add column, set player, max orbs, set current orbs as 1 and return
;; 3. If the row and column exist, one of these things happen
;;    3a. :curr-orbs+1 is < :max-orbs
;;         3a1. :owner and player don't match -> Do nothing
;;         3a2. :owner and player match -> Increment current orbs and send back a new map
;;    3b. :curr-orbs+1 is >= :max-orbs
;;         3b1. Chain react, all orbs in the path now have :owner = player

(defn get-new-board
  ([old-board [row col] player]
     (get-new-board old-board [row col] player false))
  ([old-board [row col] player is-react]
     (cond
      (= (is-valid-rc row col (get-board-size old-board)) true)
      (cond
       (= (old-board row) nil)
       (assoc old-board row {col {:owner player :max-orbs (get-max-orbs row col (get-board-size old-board)) :curr-orbs 1}})
       (= ((old-board row) col) nil)
       (assoc old-board row (assoc (old-board row) col {:owner player :max-orbs (get-max-orbs row col (get-board-size old-board)) :curr-orbs 1}))
       (< (inc (:curr-orbs ((old-board row) col))) (:max-orbs ((old-board row) col)))
       (cond
        (or (= player (:owner ((old-board row) col))) (= is-react true))
        (assoc old-board row (assoc (old-board row) col
                                    {:owner player
                                     :max-orbs (:max-orbs ((old-board row) col))
                                     :curr-orbs  (inc (:curr-orbs ((old-board row) col)))
                                     }))
        :else
        old-board
        )
       (>= (inc (:curr-orbs ((old-board row) col))) (:max-orbs ((old-board row) col)))
       (chain-react (assoc old-board row (dissoc (get old-board row) col)) row col player)
       ))))


;;This method finds which player the next turn belongs to
(defn get-next-player
  [old-player player-size]
  (if (= player-size (inc old-player))
    0
    (inc old-player)))

;;Helper Methods to find the Winner, if any

;;Given the board map this functions extracts only the cells for non nil row col values
(defn get-all-cells
  [board]
  (let [board-size (get-board-size board)]
    (filter
     #(not= % nil)
     (for
         [x (range board-size) y (range board-size)]
       (get-in board [x y])))))

;;This method returns count of all cells that have :owner nil
(defn count-of-nil-cells
  [board]
  (count (filter #(= (:owner %) nil) (get-all-cells board))))

;;This method returns the count of cells a player owns, given board and player
(defn count-of-player-cells
  [board player]
  (count (filter #(= (:owner %) player) (get-all-cells board))))

;;This method excludes the :owner nil cells and returns the count of valid(owned by a player) cells on the board
(defn count-of-valid-cells
  [board]
  (- (count (get-all-cells board)) (count-of-nil-cells board)))

(defn winner
  "Returns a winner if one exists on the board. A player cannot win on the first move."
  [board player-size]
  ;; the board looks like this: {0 {0 {:curr-orbs 1 ...}}}
  (let [board-size (get-board-size board)]
    (when (not (and (= 1 (count board))
                    (= 1 (count (second (first board))))
                    (= 1 (:curr-orbs (second (first (second (first board))))))))
      (first
       (filter
        #(and (> (count-of-player-cells board %) )
              (= (count-of-player-cells board %) (count-of-valid-cells board)))
        (range player-size))))))

;; (defn -main [& args]
;;  (try (println(get-new-board {1 {2 {:owner 1, :max-orbs 4, :curr-orbs 3}}} 1 2 1 8 2 ))
;;       (catch Exception e (println e))))
