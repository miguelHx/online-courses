;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 4)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define TANK-Y-POS (- HEIGHT 10))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 8)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; =================
;; Functions:

;; Game -> Game
;; called to make the game start; Use G examples above
;; no tests for main function
(define (main c)
  (big-bang c
    (on-tick next-game)       ; Game -> Game
    (to-draw render-game)     ; Game -> Image
    (on-key  handle-key)      ; Game KeyEvent -> Game
    (stop-when handle-stop))) ; Game -> Boolean

;; ListOfInvaders -> ListOfInvaders
;; takes in a list of invaders and spawns new invader at random x position
;; by returning same list with new invader appended
(check-random (append-rand-invader (list I1 I2 I3)) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED) I1 I2 I3))

(define (append-rand-invader loi)
  (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi))

;; Game -> Game
;; produce filtered and ticked game state
(check-expect (next-game (make-game (list I1) (list M1) T0))
              (make-game (list (make-invader 162 (+ 100 INVADER-Y-SPEED) 12))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))
                         (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))

;<template as function composition>

(define (next-game g)
  (tick-game g))

;; Game -> Game
;; produce next game state. Game cannot be empty
(check-expect (tick-game G2)
              (make-game (list (make-invader 162 101.5 12))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))
                         (make-tank 52 1)))

;<template from ListOfDrop>

(define (tick-game g)
  (make-game (if (= (random INVADE-RATE) 1)
                 (append-rand-invader (game-invaders g))
                 (tick-invaders (non-collisions-only-loi (game-invaders g) (game-missiles g))))
             (tick-missiles (onscreen-only (non-collisions-only-lom (game-invaders g) (game-missiles g))))
             (tick-tank (game-tank g))))

;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; filter out missile collisions. Colissions are when an invader's x and y position + 10 meets
;; a missiles x and y position.
(check-expect (non-collisions-only-lom (list I1) (list M1)) (list M1))
(check-expect (non-collisions-only-lom (list I1 I2 I3) (list M1 M2 M3)) (list M1))

;(define (non-collisions-only-lom loi lom) lom) ;stub

(define (non-collisions-only-lom loi lom)
  (cond [(empty? lom) empty]
        [else 
         (if (did-collide-m? (first lom) loi)
             (non-collisions-only-lom loi (rest lom))
             (cons (first lom) (non-collisions-only-lom loi (rest lom))))]))

;; Missile ListOfInvaders -> Boolean
;; returns true if missile collides with any invader in lom, false otherwise
(check-expect (did-collide-m? M1 (list I1 I2 I3)) false)
(check-expect (did-collide-m? M2 (list I1 I2 I3)) true)

;(define (did-collide? i lom) true) ;stub

(define (did-collide-m? m loi)
  (cond [(empty? loi) false]
        [else 
         (if (hit? (first loi) m)
             true
             (did-collide-m? m (rest loi)))]))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; filter out invader collisions. Colissions are when an invader's x and y position + 10 meets
;; a missiles x and y position.
(check-expect (non-collisions-only-loi (list I1) (list M1)) (list I1))
(check-expect (non-collisions-only-loi (list I1 I2 I3) (list M1 M2 M3)) (list I2 I3))

;(define (non-collisions-only-loi loi lom) loi) ;stub

(define (non-collisions-only-loi loi lom)
  (cond [(empty? loi) empty]
        [else 
         (if (did-collide? (first loi) lom)
             (non-collisions-only-loi (rest loi) lom)
             (cons (first loi) (non-collisions-only-loi (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; returns true if invader collides with any missile in lom, false otherwise
(check-expect (did-collide? I2 (list M1 M2 M3)) false)
(check-expect (did-collide? I1 (list M1 M2 M3)) true)

;(define (did-collide? i lom) true) ;stub

(define (did-collide? i lom)
  (cond [(empty? lom) false]
        [else 
         (if (hit? i (first lom))
             true
             (did-collide? i (rest lom)))]))

;; Invader Missile -> Boolean
;; returns true if missiles y pos is same as invaders y + 10 or y + 5 position, and same x position.
(check-expect (hit? I1 M1) false)
(check-expect (hit? I1 M2) true)
(check-expect (hit? I1 M3) true)

(define (hit? i m)
  (and (within-range?
        (missile-x m)
        (- (invader-x i) (/ (image-width INVADER) 2))
        (+ (invader-x i) (/ (image-width INVADER) 2)))
       (within-range?
        (missile-y m)
        (- (invader-y i) (/ (image-height INVADER) 2))
        (+ (invader-y i) (/ (image-height INVADER) 2)))))

;; Integer Integer Integer -> Boolean
;; checks if first arg is within range of lower bound and upper bound
(check-expect (within-range? 100 85 95) false)
(check-expect (within-range? 100 95 105) true)

(define (within-range? x lower upper)
  (and (>= x lower) (<= x upper)))

;; ListOfInvaders -> ListOfInvaders
;; produces new list of invaders that is invader-dx pixels farther
;; down the screen.
(check-expect (tick-invaders (list I1)) (list (make-invader 162 101.5 12)))

;(define (tick-invaders loi) empty) ;stub

;<template for list of items>

(define (tick-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (tick-invader (first loi))
               (tick-invaders (rest loi)))]))

;; Invader -> Invader
;; produces new invader at x + dx and y + dy pixels.
;; if an invader is about to pass edge, change direction
(check-expect (tick-invader (make-invader 100 200 12)) (make-invader 112 (+ 200 INVADER-Y-SPEED) 12))
(check-expect (tick-invader (make-invader (- WIDTH 10) 200 12)) (make-invader (- WIDTH 10) 200 -12))
(check-expect (tick-invader (make-invader 10 200 -12)) (make-invader 10 200 12))

(define (tick-invader i)
  (cond [(or (>= (+ (invader-x i) (invader-dx i)) WIDTH)
             (<= (+ (invader-x i) (invader-dx i)) 0))
         (make-invader (invader-x i) (invader-y i) (* (invader-dx i) -1))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))


;; ListOfMissiles -> ListOfMissiles
;; produces new list of missiles that are at y coord + missile speed
(check-expect (tick-missiles (list M1)) (list (make-missile 150 (- 300 MISSILE-SPEED))))

;(define (tick-missiles lom) empty) ;stub

;<template for list of items>

(define (tick-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (tick-missile (first lom))
               (tick-missiles (rest lom)))]))

;; Missile -> Missile
;; produces new missile at y + MISSILE-SPEED pixels.
(check-expect (tick-missile (make-missile 100 200)) (make-missile 100 (- 200 MISSILE-SPEED)))

(define (tick-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissile -> ListOfMissile
;; produce a list containing only those missiles in lom that are onscreen?
(check-expect (onscreen-only empty) empty)
(check-expect (onscreen-only (list (make-missile 100 100) (make-missile 100 -1)))
              (list (make-missile 100 100)))

;<template from ListOfMissile

(define (onscreen-only lom)
  (cond [(empty? lom) empty]
        [else
         (if (onscreen? (first lom))
             (cons (first lom) (onscreen-only (rest lom)))
             (onscreen-only (rest lom)))]))


;; Drop -> Boolean
;; produce true if d has not fallen off the bottom of MTS
(check-expect (onscreen? (make-missile 2 -1)) false)
(check-expect (onscreen? (make-missile 2  0)) true)
(check-expect (onscreen? (make-missile 2  1)) true)
(check-expect (onscreen? (make-missile 2 (- HEIGHT 1))) true)
(check-expect (onscreen? (make-missile 2    HEIGHT))    true)

;<template from Missile

(define (onscreen? m)  
  (<= 0 (missile-y m) HEIGHT))

;; Tank -> Tank
;; produces new tank going x + or - TANK-SPEED. if we about to reach edge, change direction
(check-expect (tick-tank (make-tank 50 1)) (make-tank (+ 50 TANK-SPEED) 1)) ; going right
(check-expect (tick-tank (make-tank (- WIDTH 1) 1)) (make-tank (- WIDTH 1) -1)) ; reached right edge
(check-expect (tick-tank (make-tank 1 -1)) (make-tank 1 1)) ; reached left edge
(check-expect (tick-tank (make-tank 50 -1)) (make-tank (- 50 TANK-SPEED) -1)) ; going left


(define (tick-tank t)
  (cond [(or (>= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) WIDTH)
             (<= (+ (tank-x t) (* TANK-SPEED (tank-dir t))) 0))
         (make-tank (tank-x t) (* (tank-dir t) -1))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))

;; Game -> Image
;; renders game objects onto BACKGROUND image
(check-expect (render-game G3)
              (place-image TANK 50 TANK-Y-POS
                           (place-image INVADER 150 100
                                        (place-image INVADER 150 HEIGHT
                                                     (place-image MISSILE 150 108
                                                                  (place-image MISSILE 150 300 BACKGROUND))))))

(define (render-game g)
  (place-tank (game-tank g)
              (place-invaders (game-invaders g)
                              (place-missiles (game-missiles g) BACKGROUND))))

;; Tank -> Image
;; places tank on given image
(check-expect (place-tank T1 BACKGROUND) (place-image TANK 50 TANK-Y-POS BACKGROUND))

(define (place-tank t img)
  (place-image TANK (tank-x t) TANK-Y-POS img))

;; ListOfInvaders -> Image
;; given a list of invaders place them onto the given image
(check-expect (place-invaders (list I1 I2) BACKGROUND)
              (place-image INVADER 150 100
                           (place-image INVADER 150 HEIGHT BACKGROUND)))

;(define (place-invaders loi) BACKGROUND) ; stub

(define (place-invaders loi img)
  (cond [(empty? loi) img]
        [else 
         (place-invader (first loi)
                        (place-invaders (rest loi) img))]))

;; Invader Image -> Image
;; takes an invader and places it on given image
(check-expect (place-invader I1 BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))

(define (place-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

;; ListOfMissiles -> Image
;; given a list of missiles place them onto the given image
(check-expect (place-missiles (list M1 M2) BACKGROUND)
              (place-image MISSILE 150 300
                           (place-image MISSILE 150 108 BACKGROUND)))

;(define (place-missiles lom) BACKGROUND) ; stub

(define (place-missiles lom img)
  (cond [(empty? lom) img]
        [else 
         (place-missile (first lom)
                        (place-missiles (rest lom) img))]))

;; Invader Image -> Image
;; takes an invader and places it on given image
(check-expect (place-missile M1 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))

(define (place-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))


;; Game KeyEvent -> Game
;; handles when spacebar is pressed, generate a new missile at tank position x and y pos = TANK-Y-POS - 5



(define (handle-key g ke)
  (cond [(key=? ke " ") 
         (make-game (game-invaders g)
                    (append-missile (game-missiles g) (game-tank g))
                    (game-tank g))]
        [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) 1))]
        [else g]))

;; ListOfMissiles Tank -> ListOfMissiles
;; append new missle to list of missiles at tank's x position and y pos = TANK-Y-POS - 5
(check-expect (append-missile (list M1 M2) T1) (list (make-missile 50 (- TANK-Y-POS 5)) M1 M2))

; (define (append-missile lom t) lom) ;stub

(define (append-missile lom t)
  (cons (make-missile (tank-x t) (- TANK-Y-POS 5)) lom))

;; Game -> Boolean
;; stop game by returning true if any invader reaches tanks y pos
(check-expect (handle-stop G2) false)
(check-expect (handle-stop (make-game (list (make-invader 100 (- HEIGHT 10) 12) I1)
                                      (list M1)
                                      T1)) true)

;(define (handle-stop g) false) ;stub

(define (handle-stop g)
  (check-invaders (game-invaders g)))

;; ListOfInvaders -> Boolean
;; given a loi return true if one of them reaches height TANK-Y-POS
(check-expect (check-invaders (list I1)) false)
(check-expect (check-invaders (list (make-invader 100 (- HEIGHT 10) 12) I1)) true)

; (define (check-invaders loi) false) ; stub

(define (check-invaders loi)
  (cond [(empty? loi) false]
        [else 
         (if (reached-bottom? (first loi))
             true
             (check-invaders (rest loi)))]))

;; Invader -> Boolean
;; returns true if invader y pos is at bottom or TANK-Y-POS, false otherwise
(check-expect (reached-bottom? (make-invader 100 (- HEIGHT 12) 12)) false)
(check-expect (reached-bottom? (make-invader 100 (- HEIGHT 10) 12)) true)

(define (reached-bottom? i)
  (>= (invader-y i) (- HEIGHT 10)))

