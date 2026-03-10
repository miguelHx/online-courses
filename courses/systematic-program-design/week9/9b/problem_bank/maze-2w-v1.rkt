;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname maze-2w-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Solve simple square mazes

;; maze-v1.rkt


;; Constants:

;; Data definitions:

;; Maze is (listof Boolean)
;; interp. a square maze
;;         each side length is (sqrt (length <maze>))
;;         true  (aka #t) means open, can move through this
;;         false (aka #f) means a wall, cannot move into or through a wall


(define O #t) ;Open
(define W #f) ;Wall

(define M0
  (list O W W W
        W W W W
        W W W W
        W W W W))

(define M1
  (list O W W W W
        O O W O O
        W O W W W
        O O W W W
        O O O O O))

(define M2
  (list O O O O O
        O W W W O
        O W W W O
        O W W W O
        O W W W O))

(define M3            
  (list O O O O O
        O W W W W
        O W W W W
        O W W W W 
        O O O O O))

(define M4
  (list O O O O O
        O W W W O
        O W O O O
        O W O W W
        W W O O O))

(define-struct pos (x y))
;; Pos is (make-pos Integer Integer)
;; interp. an x, y position in the maze.
;;         0, 0 is upper left.
;;         a position is only valid for a given maze if:
;;            - (<= 0 x (sub1 <size>))
;;            - (<= 0 y (sub1 <size>))
;;            - there is a true in the given cell
;;
(define P0 (make-pos 0 0)) ;upper left  in 4x4 maze
(define P1 (make-pos 3 0)) ;upper right  "  "   "
(define P2 (make-pos 0 3)) ;lower left   "  "   "
(define P3 (make-pos 3 3)) ;bottom right "  "   "

;; Functions:

;; Maze Pos -> Boolean
;; produce contents of given square in given maze
(check-expect (mref (list #t #f #f #f) (make-pos 0 0)) #t)
(check-expect (mref (list #t #t #f #f) (make-pos 0 1)) #f)

(define (mref m p)
  (local [(define s (sqrt (length m))) ;each side length
          (define x (pos-x p))
          (define y (pos-y p))]
    
    (list-ref m (+ x (* y s)))))

;; Maze -> Boolean
;; produces true if maze is solvable, otherwise false
;; A maze is solvable Solvable means that it is possible to start at the upper left,
;; and make it all the way to the lower right.
;; Your final path can only move down or right one square at a time. BUT, it
;; is permissible to backtrack if you reach a dead end.
(check-expect (solvable? M0) false)
(check-expect (solvable? M1) true)
(check-expect (solvable? M2) true)
(check-expect (solvable? M3) true)
(check-expect (solvable? M4) false)

;(define (solvable? m) false) ;stub

(define (solvable? d)
  (local [(define s (sqrt (length d)))
          (define (solvable?-gen-rec x y)
            (cond [(or (false? (and (<= 0 x (sub1 s))
                                    (<= 0 y (sub1 s))))
                       (false? (mref d (make-pos x y))))
                   false]
                  [(at-end? x y) true]
                  [else
                   (local [(define try-right (solvable?-gen-rec (+ x 1) y))
                           (define try-bottom (solvable?-gen-rec x (+ y 1)))]
                     (or try-right try-bottom))]))
          (define (at-end? x y)
            (and (= x (sub1 s)) (= y (sub1 s))))]
    (solvable?-gen-rec 0 0)))





