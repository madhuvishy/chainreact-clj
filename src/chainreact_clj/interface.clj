(ns chainreact-clj.interface
  (:use [chainreact-clj.core]
        [clojure.pprint]
        [clojure.string :only (split)]))

;;Splits command line input in style "row col" and returns '(row col)
(defn split-input
  [input]
  (map #(read-string %) (split input #" ")))

;;Returns printable value for a given cell
(defn get-print-value
  [cell]
  (let [owner (:owner cell)
        curr (:curr-orbs cell)
        maxm (:max-orbs cell)]
    (str
     (apply str (repeat curr owner))
     (apply str (repeat (- maxm curr) "_"))
     (apply str (repeat (- 4 maxm) " ")))))

;;Returns a list of printable values for all cells of a given row
(defn get-row-values
  [row board board-size]
  (for [col (range board-size)]
    (let [cell (get-in board [row col])
          length (get-max-orbs row col board-size)]
      (if (= cell nil)
        (str (apply str (repeat length "+" )) (apply str (repeat (- 4 length) " ")))
        (get-print-value cell)))))

;;Draws the board to the console using pprint
(defn draw-board
  [board board-size]
  (pprint (for [row (range board-size)]
            (get-row-values row board board-size)))
  )

;;The main method that enters into read loop, reads the move position
;; from the player and calls draw-board to draw a new board to the console
(defn play
  [board-size player-size]
  (loop [input (read-line)
         board {}
         player 0
         move-count 0]
    (when-not (= ":q" input)
      (let [new-board (get-new-board
                       board
                       (first (split-input input))
                       (second (split-input input))
                       player
                       board-size)
            new-player (if (= new-board board)
                         player
                         (get-next-player player player-size))
            new-move-count (if (= new-board board)
                             move-count
                             (inc move-count))]
        (draw-board new-board board-size)
        (println (str "Player " new-player "'s turn"))
        (if (and
             (= (count (winner new-board board-size player-size)) 1)
             (> new-move-count 2))
          (println (str "Player " (first (winner new-board board-size player-size)) " WON"))
          (recur (read-line) new-board new-player new-move-count))))))


;;Reads command line arguments for board-size and player-size and starts the Play
(defn -main [& args]
  (let [board-size (read-string (first args))
        player-size (read-string (second args))]
    (draw-board {} board-size)
    (println (str "Player 0's turn"))
    (play board-size player-size)
    ))
