(ns tic-tac-toe.core
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.test :refer [is deftest testing with-test run-tests]]))

(defn new-game []
  {:messages []
   :board [[0 0 0]
           [0 0 0]
           [0 0 0]]})

(with-test

  (defn valid-move?
    [previous row col]
    (and (= 0 previous)
         (>= 2 row 0)
         (>= 2 col 0)))

  (is (not (valid-move? nil 0 0)))
  (is (not (valid-move? 0 10 0)))
  (is (valid-move? 0 0 0))
  (is (valid-move? 0 2 2)))

(defn row-winner?
  [board row]
  (apply = (get board row)))

(defn col-winner?
  [board col]
  (apply = (map #(get % col) board)))

(with-test

  (defn ldiag-winner?
    [board]
    (apply = (for [n (range 3)]
               (get-in board [n n]))))

  (is (= false (ldiag-winner? [[0 1 2]
                               [2 1 0]
                               [1 2 0]])))
  (is (= true (ldiag-winner? [[2 1 0]
                              [1 2 0]
                              [0 1 2]]))))

(with-test

  (defn rdiag-winner?
    [board]
    (apply = (for [n (range 3)]
               (get-in board [n (- 2 n)]))))

  (is (= false (rdiag-winner? [[0 1 2]
                               [2 1 0]
                               [1 2 0]])))
  (is (= true (rdiag-winner? [[0 1 2]
                              [1 2 0]
                              [2 1 0]]))))

(with-test

  (defn winner
    [board]
    (cond
      (row-winner? board 0) (get-in board [0 0])
      (row-winner? board 1) (get-in board [1 0])
      (row-winner? board 2) (get-in board [2 0])
      (col-winner? board 0) (get-in board [0 0])
      (col-winner? board 1) (get-in board [0 1])
      (col-winner? board 2) (get-in board [0 2])
      (ldiag-winner? board) (get-in board [0 0])
      (rdiag-winner? board) (get-in board [0 2])
      :else nil))

  (is (= nil (winner [[1 1 2]
                      [1 2 2]
                      [0 2 1]])))
  (is (= 1 (winner [[1 1 1]
                    [1 2 2]
                    [2 2 0]])))
  (is (= 1 (winner [[1 1 2]
                    [1 2 2]
                    [1 2 0]])))
  (is (= 2 (winner [[1 1 2]
                    [0 2 2]
                    [2 2 1]]))))

(with-test

  (defn take-turn
    "apply move with `piece` at `row` `col` to game state in `board`"
    [state [piece row col]]
    (let [previous (get-in state [:board row col])]
      (if (valid-move? previous row col)
        (assoc-in state [:board row col] piece)
        (update state :messages
                #(conj % (format "Invalid move for %s at [%s %s]" piece row col))))))

  (is (= [[0 0 0]
          [0 0 0]
          [0 0 0]]
         (:board (take-turn (new-game) [2 0 3]))))
  (is (= [[0 0 0]
          [0 0 0]
          [0 0 1]]
         (:board (take-turn (new-game) [1 2 2]))))
  (is (= [[0 0 2]
          [0 0 0]
          [0 0 0]]
         (:board (take-turn (new-game) [2 0 2]))))
  (is (= {:messages ["Invalid move for 1 at [0 3]"]
          :board  [[1 0 0]
                   [0 1 2]
                   [2 0 1]]}
         (reduce take-turn (new-game)
                 [[1 0 3] [1 0 0] [2 1 2] [1 1 1] [2 2 0] [1 2 2]]))))

(comment
  (pprint (reductions take-turn (new-game)
                      [[1 0 0] [2 0 0] [1 0 0] [2 0 1] [1 1 1]
                       [2 2 2] [1 1 0] [2 1 2] [1 2 0]])))

(run-tests)
