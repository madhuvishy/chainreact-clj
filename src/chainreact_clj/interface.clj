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
  [board-size player-size]
  (loop [board {}
         player 0]
    (let [move (get-move! board-size)]
      (when-not (= move ::quit)
        (let [new-board (get-new-board
                         board
                         move
                         player
                         board-size)
              new-player (if (= new-board board)
                           player
                           (get-next-player player player-size))]
          (draw-board new-board board-size)
          (if (winner new-board board-size player-size)
            (println (str "Player " (winner new-board board-size player-size) " WON"))
            (do (println (str "Player " new-player "'s turn"))
                (recur new-board new-player))))))))


;;Reads command line arguments for board-size and player-size and starts the Play
(defn -main [& [board-size player-size]]
  (let [board-size (Long/parseLong board-size)
        player-size (Long/parseLong player-size)]
    (draw-board {} board-size)
    (println (str "Player 0's turn"))
    (play board-size player-size)))
