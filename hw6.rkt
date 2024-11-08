;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Language : Intermediate Student with Lambda

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;; Data Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Direction is one of:
; - "up"
; - "down"
; - "left"
; - "right"

; Direction constants
(define LEFT "left")
(define RIGHT "right")
(define DOWN "down")
(define UP "up")

; screen
(define SCREEN-WIDTH 500)
(define SCREEN-HEIGHT 600)
(define ROW-HEIGHT (/ SCREEN-HEIGHT 25)) ; x = 0 - 24
(define COLUMN-WIDTH (/ SCREEN-WIDTH 20)) ; y = 0 - 24
(define EMPTYSCENE (empty-scene SCREEN-WIDTH SCREEN-HEIGHT "dark green"))
(define ROAD (rectangle SCREEN-WIDTH (* ROW-HEIGHT 8) "solid" "dark gray"))
(define RIVER (rectangle SCREEN-WIDTH (* ROW-HEIGHT 4.25) "solid" "dark blue"))
(define GAME-BACKGROUND (place-image
                         (overlay/offset RIVER 0 (* ROW-HEIGHT 6.5) ROAD)
                         (/ SCREEN-WIDTH 2)
                         (+ (/ SCREEN-HEIGHT 2) 5)
                         EMPTYSCENE))

;; A Player is a (make-player Number Number Direction)
;; Interpretation: x and y are the coordinates of the player, and
;; dir is the direction the player is facing
(define-struct player (x y dir))

; player-temp : Player -> ?
#;(define (player-temp plyr)
    ( ... (player-x plyr) ... (player-y plyr) ...
          ... (player-dir plyr) ... ))

; data example:
(define initial-player (make-player 10 22 UP))

;; A Vehicle is a (make-vehicle Number Number Direction)
;; Interpretation: x and y are the coordinates of the vehicle
;; and dir is the direction of it
(define-struct vehicle (x y dir))

; vehicle data example:
(define vehicle1 (make-vehicle 5 12 RIGHT))
(define vehicle2 (make-vehicle 10 12 RIGHT))
(define vehicle3 (make-vehicle 15 12 RIGHT))
(define vehicle4 (make-vehicle 19 12 RIGHT))
(define vehicle5 (make-vehicle 0 14 LEFT))
(define vehicle6 (make-vehicle 3 14 LEFT))
(define vehicle7 (make-vehicle 7 14 LEFT))
(define vehicle8 (make-vehicle 14 14 LEFT))
(define vehicle9 (make-vehicle 3 16 RIGHT))
(define vehicle10 (make-vehicle 4 16 RIGHT))
(define vehicle11 (make-vehicle 10 16 RIGHT))
(define vehicle12 (make-vehicle 15 16 RIGHT))
(define vehicle13 (make-vehicle 4 18 LEFT))
(define vehicle14 (make-vehicle 7 18 LEFT))
(define vehicle15 (make-vehicle 17 18 LEFT))
(define vehicle16 (make-vehicle 19 18 LEFT))

; vehicle-temp : Vehicle -> ?
#;(define (vehicle-temp v)
    ( ... (vehicle-x v) ... (vehicle-y v) ...
          ... (vehicle-dir v) ... ))

;; A Set of Vehicles (VSet) is one of:
;; - empty
;; - (cons Vehicle VSet)
;; Represents a set of vehicles

; vset-temp : VSet -> ?
#;(define (vset-temp vs)
    (cond
      [(empty? vs) ... ]
      [(cons? vs) ... (vehicle-temp (first vs)) ...
                  ... (vset-temp (rest vs)) ... ]))

; example vehicle list
(define initial-vehicles (list vehicle1 vehicle2 vehicle3 vehicle4
                               vehicle5 vehicle6 vehicle7 vehicle8
                               vehicle9 vehicle10 vehicle11 vehicle12
                               vehicle13 vehicle14 vehicle15 vehicle16))

;; A Turtle is a (make-turtle Number Number Direction)
;; Interpretation: x and y are the coordinates of the turtle
;; and dir is the direction the turtle is moving
(define-struct turtle (x y dir))

; turtle-temp : Turtle -> ?
#;(define (turtle-temp t)
    ( ... (turtle-x t) ... (turtle-y t)
          ... (turtle-dir t) ... ))

; turtle examples
(define turtle1 (make-turtle 1 7 LEFT))
(define turtle2 (make-turtle 3 7 LEFT))
(define turtle3 (make-turtle 5 7 LEFT))
(define turtle4 (make-turtle 7 7 LEFT))
(define turtle5 (make-turtle 10 7 LEFT))
(define turtle6 (make-turtle 15 7 LEFT))
(define turtle7 (make-turtle 5 9 LEFT))
(define turtle8 (make-turtle 10 9 LEFT))
(define turtle9 (make-turtle 16 9 LEFT))
(define turtle10 (make-turtle 7 9 LEFT))
(define turtle11 (make-turtle 12 9 LEFT))
(define turtle12 (make-turtle 14 9 LEFT))

;; A Set of Turtles (TSet) is one of:
;; - empty
;; - (cons Turtle TSet)
;; Represents a set of turtles

; tset-temp : TSet -> ?
#;(define (tset-temp ts)
    (cond
      [(empty? ts) ... ]
      [(cons? ts) ... (turtle-temp (first ts)) ...
                  ... (tset-temp (rest ts)) ... ]))

; TSet example
(define initial-turtles (list turtle1 turtle2 turtle3 turtle4
                              turtle5 turtle6 turtle7 turtle8
                              turtle9 turtle10 turtle11 turtle12))

;; A Plank is a (make-plank Number Number Direction)
;; Interpretation: x and y are the coordinates of the plank
;; and dir is the direction the plank is moving
(define-struct plank (x y dir))

; plank-temp : Plank -> ?
#;(define (plank-temp a-plank)
    ( ... (plank-x a-plank) ... (plank-y a-plank) ...
          ... (plank-dir a-plank) ... ))

; plank examples
(define plank1a (make-plank 5 8 RIGHT))
(define plank1b (make-plank 6 8 RIGHT))
(define plank1c (make-plank 7 8 RIGHT))
(define plank1d (make-plank 8 8 RIGHT))
(define plank2a (make-plank 11 8 RIGHT))
(define plank2b (make-plank 12 8 RIGHT))
(define plank2c (make-plank 13 8 RIGHT))

(define plank3a (make-plank 7 10 RIGHT))
(define plank3b (make-plank 8 10 RIGHT))
(define plank3c (make-plank 9 10 RIGHT))
(define plank3d (make-plank 10 10 RIGHT))
(define plank4a (make-plank 1 10 RIGHT))
(define plank4b (make-plank 2 10 RIGHT))
(define plank4c (make-plank 3 10 RIGHT))

;; A Set of Planks (PSet) is one of:
;; - empty
;; - (cons Plank PSet)
;; Represents a set of planks

; pset-temp : PSet -> ?
#;(define (pset-temp ps)
    (cond
      [(empty? ps) ... ]
      [(cons? ps) ... (plank-temp (first ps)) ...
                  ... (pset-temp (rest ps)) ... ]))

; PSet example
(define initial-planks (list plank1a plank1b plank1c plank1d
                             plank2a plank2b plank2c
                             plank3a plank3b plank3c plank3d
                             plank4a plank4b plank4c))

;; A World is a (make-world Player VSet TSet PSet)
;; The VSet represents the set of vehicles moving across the screen
(define-struct world (player vehicles turtles planks))

; world-temp : World -> ?
#;(define (world-temp w)
    ( ... (player-temp (world-player w)) ...
          ... (vset-temp (world-vehicles w)) ...
          ... (tset-temp (world-turtles w)) ...
          ... (pset-temp (world-planks w)) ... ))

; world example
(define initial-world (make-world initial-player initial-vehicles
                                  initial-turtles initial-planks))

; player and vehicle sprite
(define PLAYER-SPRITE (triangle COLUMN-WIDTH "solid" "medium green"))
(define VEHICLE-SPRITE (rectangle COLUMN-WIDTH ROW-HEIGHT "solid" "medium red"))
(define TURTLE-SPRITE (ellipse COLUMN-WIDTH ROW-HEIGHT "solid" "light brown"))
(define PLANK-SPRITE (rectangle COLUMN-WIDTH ROW-HEIGHT "solid" "medium brown"))

;;;;;;;;;;;;;;;;;;;;;;; Moving Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-all-vehicles : VSet -> VSet
;; moves all the vehicles by a unit in their given direction
(define (move-all-vehicles vs)
  (map move-vehicle vs))

; move-all-vehicles tests
(check-expect (move-all-vehicles (list (make-vehicle 21 4 RIGHT)
                                       (make-vehicle 1 4 LEFT)))
              (list (make-vehicle 1 4 RIGHT) (make-vehicle 0 4 LEFT)))
(check-expect (move-all-vehicles (list (make-vehicle 0 4 RIGHT)
                                       (make-vehicle 21 4 LEFT)))
              (list (make-vehicle 1 4 RIGHT) (make-vehicle 20 4 LEFT)))

;; move-vehicle : Vehicle -> Vehicle
;; moves a vehicle
(define (move-vehicle v)
  (cond
    [(string=? (vehicle-dir v) RIGHT)
     (make-vehicle (modulo (+ (vehicle-x v) 1) 21)
                   (vehicle-y v)
                   RIGHT)]
    [(string=? (vehicle-dir v) LEFT)
     (make-vehicle (modulo (- (vehicle-x v) 1) 21)
                   (vehicle-y v)
                   LEFT)]))

; Test moving a vehicle to the right
(check-expect (move-vehicle (make-vehicle 20 4 RIGHT)) (make-vehicle 0 4 RIGHT))
(check-expect (move-vehicle (make-vehicle 0 4 RIGHT)) (make-vehicle 1 4 RIGHT))
; Test moving a vehicle to the left
(check-expect (move-vehicle (make-vehicle 0 4 LEFT)) (make-vehicle 20 4 LEFT))
(check-expect (move-vehicle (make-vehicle 19 4 LEFT)) (make-vehicle 18 4 LEFT))

;; move-all-turtles : TSet -> TSet
;; moves all turtles one unit in the given direction
(define (move-all-turtles ts)
  (map move-turtle ts))

(check-expect (move-all-turtles (list (make-turtle 1 7 LEFT)
                                      (make-turtle 2 7 LEFT)))
              (list (make-turtle 0 7 LEFT) (make-turtle 1 7 LEFT)))
(check-expect (move-all-turtles (list (make-turtle 0 7 LEFT)
                                      (make-turtle 20 7 LEFT)))
              (list (make-turtle 19 7 LEFT) (make-turtle 19 7 LEFT)))

;; move-turtle : Turtle -> Turtle
;; moves a turtle one unit in the given direction
(define (move-turtle t)
  (cond
    [(string=? (turtle-dir t) LEFT)
     (make-turtle (modulo (- (turtle-x t) 1) 20)
                  (turtle-y t)
                  LEFT)]))

; move-turtle tests
(check-expect (move-turtle (make-turtle 1 7 LEFT)) (make-turtle 0 7 LEFT))
(check-expect (move-turtle (make-turtle 0 7 LEFT)) (make-turtle 19 7 LEFT))

;; move-all-planks : PSet -> PSet
;; moves all planks one unit in the given direction
(define (move-all-planks ps)
  (map move-plank ps))

; move-all-planks tests
(check-expect (move-all-planks (list (make-plank 1 8 RIGHT)
                                     (make-plank 2 8 RIGHT)))
              (list (make-plank 2 8 RIGHT) (make-plank 3 8 RIGHT)))
(check-expect (move-all-planks (list (make-plank 19 8 RIGHT)
                                     (make-plank 20 8 RIGHT)))
              (list (make-plank 0 8 RIGHT) (make-plank 1 8 RIGHT)))

;; move-plank : Plank -> Plank
;; moves a plank one unit in the given direction
(define (move-plank p)
  (cond
    [(string=? (plank-dir p) RIGHT)
     (make-plank (modulo (+ (plank-x p) 1) 20)
                 (plank-y p)
                 RIGHT)]))

; move-plank tests
(check-expect (move-plank (make-plank 1 8 RIGHT)) (make-plank 2 8 RIGHT))
(check-expect (move-plank (make-plank 19 8 RIGHT)) (make-plank 0 8 RIGHT))

;;;;;;;;;;;;;;;;;;;;; Collision Functions ;;;;;;;;;;;;;;;;;;;;;;;

;; any-collision? : Player VSet -> Boolean
;; true if a player position is equal a vehicle position, false otherwise
(define (any-collision? plyr vs)
  (ormap (λ (v) (a-collision? plyr v)) vs))

; any-collision? tests
(check-expect (any-collision? (make-player 1 1 "up") (list (make-vehicle 1 1 "right")
                                                           (make-vehicle 2 2 "left")))
              true)
(check-expect (any-collision? (make-player 0 0 "up") (list (make-vehicle 1 1 "right")
                                                           (make-vehicle 2 2 "left")))
              false)

;; a-collision? : Player Vehicle -> Boolean
;; true if a player coordinate equals given vehicle coordinate
(define (a-collision? plyr v)
  (and (= (player-x plyr) (vehicle-x v))
       (= (player-y plyr) (vehicle-y v))))

; a-collision? tests
(check-expect (a-collision? (make-player 1 1 "up") (make-vehicle 1 1 "right")) true)
(check-expect (a-collision? (make-player 0 0 "up") (make-vehicle 1 1 "right")) false)

;; on-turtle-or-plank? : Player TSet PSet -> Boolean
;; true if player is on turtle or plank, only in river section, false otherwise
(define (on-turtle-or-plank? plyr ts ps)
  (or (ormap (λ (t) (on-turtle? plyr t)) ts)
      (ormap (λ (p) (on-plank? plyr p)) ps)))

; on-turtle-or-plank? tests
(check-expect (on-turtle-or-plank? (make-player 1 1 "up")
                                   (list (make-turtle 1 1 "left"))
                                   (list (make-plank 2 2 "right")))
              true)
(check-expect (on-turtle-or-plank? (make-player 0 0 "up")
                                   (list (make-turtle 1 1 "left"))
                                   (list (make-plank 2 2 "right")))
              false)

;; on-turtle? : Player Turtle -> Boolean
;; checks if player is on a turtle
(define (on-turtle? plyr t)
  (and (= (player-x plyr) (turtle-x t))
       (= (player-y plyr) (turtle-y t))))

; on-turtle? tests
(check-expect (on-turtle? (make-player 1 1 "up") (make-turtle 1 1 "left")) true)
(check-expect (on-turtle? (make-player 0 0 "up") (make-turtle 1 1 "left")) false)

;; on-plank? : Player Plank -> Boolean
;; checks if player is on a plank
(define (on-plank? plyr p)
  (and (= (player-x plyr) (plank-x p))
       (= (player-y plyr) (plank-y p))))

; on-plank? tests
(check-expect (on-plank? (make-player 1 1 "up") (make-plank 1 1 "right")) true)
(check-expect (on-plank? (make-player 0 0 "up") (make-plank 1 1 "right")) false)

;;;;;;;;;;;;;;;;;;;;; Placing/Drawing Functions ;;;;;;;;;;;;;;;;;;;

;; draw-vehicles : VSet -> Scene
;; draws each vehicle in a VSet onto a scene
(define (draw-vehicles vs scene)
  (foldr (λ (v s) (place-vehicle v s)) scene vs))

; draw-vehicles tests
(check-expect (draw-vehicles (list (make-vehicle 1 1 "right"))
                             (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image VEHICLE-SPRITE (* COLUMN-WIDTH 1) (* ROW-HEIGHT 1)
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))
(check-expect (draw-vehicles (list (make-vehicle 1 1 "right")
                                   (make-vehicle 2 2 "left"))
                             (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image VEHICLE-SPRITE (* COLUMN-WIDTH 1) (* ROW-HEIGHT 1)
                           (place-image VEHICLE-SPRITE (* COLUMN-WIDTH 2) (* ROW-HEIGHT 2)
                                        (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))))

; place-vehicle : Vehicle scene -> scene
; places a vehicle onto a scene
(define (place-vehicle v scene)
  (place-image VEHICLE-SPRITE
               (* COLUMN-WIDTH (vehicle-x v))
               (* ROW-HEIGHT (vehicle-y v))
               scene))

; place-vehicle tests
(check-expect (place-vehicle (make-vehicle 1 1 "right")
                             (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image VEHICLE-SPRITE (* COLUMN-WIDTH 1) (* ROW-HEIGHT 1)
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))
(check-expect (place-vehicle (make-vehicle 2 2 "left")
                             (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image VEHICLE-SPRITE (* COLUMN-WIDTH 2) (* ROW-HEIGHT 2)
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))

;; draw-turtles : TSet Scene -> Scene
;; draws each turtle of a TSet onto a given Scene
(define (draw-turtles ts scene)
  (foldr (λ (t s) (place-turtle t s)) scene ts))

; draw-turtles tests
(check-expect (draw-turtles (list (make-turtle 1 1 "left"))
                            (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image TURTLE-SPRITE (* COLUMN-WIDTH 1) (* ROW-HEIGHT 1)
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))
(check-expect (draw-turtles (list (make-turtle 1 1 "left")
                                  (make-turtle 2 2 "left"))
                            (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image TURTLE-SPRITE (* COLUMN-WIDTH 1) (* ROW-HEIGHT 1)
                           (place-image TURTLE-SPRITE (* COLUMN-WIDTH 2) (* ROW-HEIGHT 2)
                                        (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))))

;; place-turtle : Turtle Scene -> Scene
;; draws a turtle onto a given Scene
(define (place-turtle t scene)
  (place-image TURTLE-SPRITE
               (* COLUMN-WIDTH (turtle-x t))
               (* ROW-HEIGHT (turtle-y t))
               scene))

; place-turtle tests
(check-expect (place-turtle (make-turtle 1 1 "left")
                            (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image TURTLE-SPRITE (* COLUMN-WIDTH 1) (* ROW-HEIGHT 1)
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))
(check-expect (place-turtle (make-turtle 2 2 "left")
                            (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image TURTLE-SPRITE (* COLUMN-WIDTH 2) (* ROW-HEIGHT 2)
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))

;; draw-planks : PSet Scene -> Scene
;; draws each Plank of a PSet onto a given Scene
(define (draw-planks ps scene)
  (foldr (λ (p s) (place-plank p s)) scene ps))

; draw-planks tests
(check-expect (draw-planks (list (make-plank 1 1 "right"))
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image PLANK-SPRITE (* COLUMN-WIDTH 1) (* ROW-HEIGHT 1)
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))
(check-expect (draw-planks (list (make-plank 1 1 "right")
                                 (make-plank 2 2 "right"))
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image PLANK-SPRITE (* COLUMN-WIDTH 1) (* ROW-HEIGHT 1)
                           (place-image PLANK-SPRITE (* COLUMN-WIDTH 2) (* ROW-HEIGHT 2)
                                        (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))))

;; place-plank : Plank Scene -> Scene
;; draws a Plank onto a given Scene
(define (place-plank p scene)
  (place-image PLANK-SPRITE
               (* COLUMN-WIDTH (plank-x p))
               (* ROW-HEIGHT (plank-y p))
               scene))

; place-plank tests
(check-expect (place-plank (make-plank 1 1 "right")
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image PLANK-SPRITE (* COLUMN-WIDTH 1) (* ROW-HEIGHT 1)
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))
(check-expect (place-plank (make-plank 2 2 "right")
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))
              (place-image PLANK-SPRITE (* COLUMN-WIDTH 2) (* ROW-HEIGHT 2)
                           (empty-scene SCREEN-WIDTH SCREEN-HEIGHT)))

; draw-world : World -> Scene
; draws the vehicles, turtles, planks, and player onto a scene
(define (draw-world w)
  (place-image (cond
                 [(string=? (player-dir (world-player w)) "left")
                  (rotate 90 PLAYER-SPRITE)]
                 [(string=? (player-dir (world-player w)) "right")
                  (rotate 270 PLAYER-SPRITE)]
                 [(string=? (player-dir (world-player w)) "up")
                  PLAYER-SPRITE]
                 [(string=? (player-dir (world-player w)) "down")
                  (rotate 180 PLAYER-SPRITE)])
               (* COLUMN-WIDTH (player-x (world-player w)))
               (* ROW-HEIGHT (player-y (world-player w)))
               (draw-vehicles (world-vehicles w)
                              (draw-turtles (world-turtles w)
                                            (draw-planks (world-planks w)
                                                         GAME-BACKGROUND)))))
(define test-world4
  (make-world (make-player 10 22 DOWN)
              (list (make-vehicle 5 12 RIGHT))
              (list (make-turtle 1 7 LEFT))
              (list (make-plank 5 8 RIGHT))))

(define expected-scene4
  (place-image (rotate 180 PLAYER-SPRITE)
               (* COLUMN-WIDTH 10)
               (* ROW-HEIGHT 22)
               (place-image VEHICLE-SPRITE
                            (* COLUMN-WIDTH 5)
                            (* ROW-HEIGHT 12)
                            (place-image TURTLE-SPRITE
                                         (* COLUMN-WIDTH 1)
                                         (* ROW-HEIGHT 7)
                                         (place-image PLANK-SPRITE
                                                      (* COLUMN-WIDTH 5)
                                                      (* ROW-HEIGHT 8)
                                                      GAME-BACKGROUND)))))

(check-expect (draw-world test-world4) expected-scene4)
(check-expect (draw-world initial-world)
              (place-image PLAYER-SPRITE
                           (* COLUMN-WIDTH (player-x (world-player initial-world)))
                           (* ROW-HEIGHT (player-y (world-player initial-world)))
                           (draw-vehicles
                            (world-vehicles initial-world)
                            (draw-turtles
                             (world-turtles initial-world)
                             (draw-planks
                              (world-planks initial-world)
                              GAME-BACKGROUND)))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Key Inputs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; key-response : World String -> World
; update world based on key event
(define (key-response w key)
  (make-world (move-player (world-player w) key)
              (world-vehicles w)
              (world-turtles w)
              (world-planks w)))

(check-expect (key-response initial-world "left")
              (make-world (move-player initial-player "left")
                          initial-vehicles
                          initial-turtles
                          initial-planks))

(check-expect (key-response initial-world "right")
              (make-world (move-player initial-player "right")
                          initial-vehicles
                          initial-turtles
                          initial-planks))

(check-expect (key-response initial-world "down")
              (make-world (move-player initial-player "down")
                          initial-vehicles
                          initial-turtles
                          initial-planks))

; move-player : Player String -> Player
; moves the player a unit towards the direction that is given in the string
(define (move-player p key)
  (cond
    [(string=? key "left") (make-player (max -1 (- (player-x p) 1))
                                        (player-y p)
                                        LEFT)]
    [(string=? key "right") (make-player (min 20 (+ (player-x p) 1))
                                         (player-y p)
                                         RIGHT)]
    [(string=? key "up") (make-player (player-x p)
                                      (max -1 (- (player-y p) 1))
                                      UP)]
    [(string=? key "down") (make-player (player-x p)
                                        (min 23 (+ (player-y p) 1))
                                        DOWN)]
    [else p]))

; move-player tests
(check-expect (move-player initial-player "left") (make-player 9 22 LEFT))
(check-expect (move-player initial-player "right") (make-player 11 22 RIGHT))
(check-expect (move-player initial-player "up") (make-player 10 21 UP))

;;;;;;;;;;;;;;;;;;;;;;; Game Over ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; in-river-section? : World -> Boolean
; Check if the player is in river
(define (in-river-section? w)
  (and (<= (player-y (world-player w)) 10)
       (>= (player-y (world-player w)) 7)))

; in-river-section? tests
(check-expect (in-river-section? (make-world (make-player 1 7 "up")
                                             initial-vehicles
                                             initial-turtles
                                             initial-planks)) #t)
(check-expect (in-river-section? (make-world (make-player 1 6 "up")
                                             initial-vehicles
                                             initial-turtles
                                             initial-planks)) #f)

; game-over? : World -> Boolean
; Check if the game should end
(define (game-over? w)
  (or (any-collision? (world-player w) (world-vehicles w))
      (and (in-river-section? w)
           (not (on-turtle-or-plank?
                 (world-player w)
                 (world-turtles w)
                 (world-planks w))))
      (< (player-x (world-player w)) 0)
      (> (player-x (world-player w)) 19)))

; game-over? tests
(check-expect (game-over? (make-world
                           initial-player
                           initial-vehicles
                           initial-turtles
                           initial-planks)) #false)
(check-expect (game-over? (make-world
                           (make-player 10 8 UP)
                           initial-vehicles
                           initial-turtles
                           initial-planks)) #true)

; draw-game-over : World -> Scene
; Draw game over scene
(define (draw-game-over w)
  (place-image (text "GAME OVER" 50 "red")
               (/ SCREEN-WIDTH 2)
               (/ SCREEN-HEIGHT 2)
               (draw-world w)))

(check-expect (draw-game-over initial-world)
              (place-image (text "GAME OVER" 50 "red")
                           (/ SCREEN-WIDTH 2)
                           (/ SCREEN-HEIGHT 2)
                           (draw-world initial-world)))

;;;;;;;;;;;;;;;;;;;;;;; World Updates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; update-world : World -> World
;; Update world each tick (move vehicles, turtles, planks, check for collisions)
(define (update-world w)
  (make-world (if (in-river-section? w)
                  (cond
                    [(ormap (λ (t) (on-turtle? (world-player w) t)) (world-turtles w))
                     (move-player (world-player w) LEFT)]
                    [(ormap (λ (p) (on-plank? (world-player w) p)) (world-planks w))
                     (move-player (world-player w) RIGHT)])
                  (world-player w))
              (move-all-vehicles (world-vehicles w))
              (move-all-turtles (world-turtles w))
              (move-all-planks (world-planks w))))

; update-world tests
(check-expect (update-world initial-world)
              (make-world initial-player
                          (move-all-vehicles initial-vehicles)
                          (move-all-turtles initial-turtles)
                          (move-all-planks initial-planks)))

;;;;;;;;;;;;;;;;;;;;;;;;;; Big Bang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(big-bang initial-world
  [to-draw draw-world]
  [on-key key-response]
  [on-tick update-world 0.75]
  [stop-when game-over? draw-game-over])