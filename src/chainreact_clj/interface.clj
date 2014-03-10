(ns chainreact-clj.interface
  (:use [chainreact-clj.core]
        [clojure.pprint]
        [clojure.string :only (split)]))

;;Splits command line input in style "row col" and returns '(row col)
(defn split-input
  [input]
  (map #(Long/parseLong %) (split input #" ")))

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
  [row board]
  (for [col (range (get-board-size board))]
    (let [cell (get-in board [row col])
          length (get-max-orbs row col (get-board-size board))]
      (if (= cell nil)
        (str (apply str (repeat length "+" )) (apply str (repeat (- 4 length) " ")))
        (get-print-value cell)))))

;;Draws the board to the console using pprint
(defn draw-board
  [board]
  (pprint (for [row (range (get-board-size board))]
            (get-row-values row board))))

(defn get-move!
  "Get a row and col from next player, exiting the process if :q is
  passed."
  [board-size]
  (let [input (read-line)]
    (if (= ":q" input)
      ::quit
      (try
        (let [[row col] (split-input input)]
          (if (and row col (is-valid-rc row col board-size))
            [row col]
            (do (println "Move out of bounds")
                (get-move! board-size))))
        (catch Throwable e
          (println "Invalid input")
          (get-move! board-size))))))

;;The main method that enters into read loop, reads the move position
;; from the player and calls draw-board to draw a new board to the console
(defn play
  [board player-size]
  (loop [board board
         player 0]
    (let [move (get-move! (get-board-size board))]
      (when-not (= move ::quit)
        (let [new-board (get-new-board
                         board
                         move
                         player)
              new-player (if (= new-board board)
                           player
                           (get-next-player player player-size))]
          (draw-board new-board)
          (if-let [player-winner (winner new-board player-size)]
            (println (str "Player " player-winner " WON"))
            (do (println (str "Player " new-player "'s turn"))
                (recur new-board new-player))))))))

;;Reads command line arguments for board-size and player-size and starts the Play
(defn -main [& [board-size player-size]]
  (let [board-size (Long/parseLong board-size)
        player-size (Long/parseLong player-size)
        board (new-board board-size)]
    (draw-board board)
    (println (str "Player 0's turn"))
    (play board player-size)))
