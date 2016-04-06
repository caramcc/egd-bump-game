;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bump_alpha) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)

#| ABOUT/ TODO
personal space optimization as measured by the distance between the player
and the other npcs in the scene

every 5 seconds a new score snapshot is taken, every 10-15 a new enemy is added,
and they shuffle in and out and adjust and move around

the player grows redder as an indication of how badly they're doing. 

|#


;; TODO
;; make a tesselated ever-shifting background of random pretty triangles

;;;;; UTILITIES ;;;;;
(define (random-element list)
  (list-ref list (random (length list))))

(define (make-grid x y spacing)
  (foldl (lambda (l base)
           (place-image (line 0 (* 2 y) 'black) l l 
                        (place-image (line (* 2 x) 0 'blue) l l base)))
         (empty-scene x y)
         (range 0 (max x y) spacing)))

(define (gen-enemies n)
  (if (= n 0)
      null
      (cons (make-enemy (make-posn (random-element (range 0 500 25))
                                   (random-element (range 0 500 25)))
                        (random-element (range 0 7 1)))
            (gen-enemies (- n 1)))))

;; TODO
; bump detect w/ list of bounds ranges for walls 

;;;;; STRUCTS ;;;;;
; a player is a Posn Number Number
(define-struct player (posn score bump-count))

; an enemy is a Posn Number where the number is the number of stops left
(define-struct enemy (posn stops-left))

; a world is a Player and (Listof Enemy)
(define-struct world (player enemies))



;;;;; CONSTANTS ;;;;;
(define MMMM (rs-read "mmm.wav"))
(define ENEMY-IMG (star 25/2 'solid 'black))
(define PLAYER-IMG (star 25/2 'solid 'blue))
(define SCN (make-grid 500 500 25))
(define MOV-CONST 25)
(define ENEMY-STOPS-CONST 5)
(define FRONT-DOOR (make-posn 50 0))
(define BACK-DOOR (make-posn 650 0))



(define TEST-ENEMIES (gen-enemies 100))
  ;(list (make-enemy (make-posn 50 75) 3)
   ;                        (make-enemy (make-posn 300 100) 4)
    ;                       (make-enemy (make-posn 75 75) 1)
     ;                      (make-enemy (make-posn 400 400)  5)))
(define TEST-PLAYER (make-player (make-posn 25 25) 0 0))

;;;;; FUNCTIONS ;;;;;

(define (draw-enemy enemy scene)
  (place-image ENEMY-IMG
               (posn-x (enemy-posn enemy))
               (posn-y (enemy-posn enemy))
               scene))

(define (draw-world w)
  (place-image (text (number->string (calc-score w)) 12 'red)
               250 25
               (place-image PLAYER-IMG
                            (posn-x (player-posn (world-player w)))
                            (posn-y (player-posn (world-player w)))
                            (foldl draw-enemy SCN (world-enemies w)))))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (intersects-enemies? p w dir)
  (let ([moved-p (move-player-posn p dir)])
    (ormap (lambda (e) (posn=? (player-posn moved-p) (enemy-posn e))) (world-enemies w))))


; move player p in the specified direction
(define (move-player-posn p dir)
  (cond [(symbol=? 'up dir) (make-player (make-posn (posn-x (player-posn p))
                                                     (max 0 (- (posn-y (player-posn p)) MOV-CONST)))
                                          (player-score p)
                                          (player-bump-count p))]
        [(symbol=? 'down dir) (make-player (make-posn (posn-x (player-posn p))
                                                     (min (image-height SCN) (+ (posn-y (player-posn p)) MOV-CONST)))
                                          (player-score p)
                                          (player-bump-count p))]
        [(symbol=? 'left dir) (make-player (make-posn (max 0 (- (posn-x (player-posn p)) MOV-CONST))
                                             (posn-y (player-posn p)))
                                          (player-score p)
                                          (player-bump-count p))]
        [(symbol=? 'right dir) (make-player (make-posn (min (image-width SCN) (+ (posn-x (player-posn p)) MOV-CONST))
                                             (posn-y (player-posn p)))
                                          (player-score p)
                                          (player-bump-count p))]))

(define (move-player p w dir)
  (if (intersects-enemies? p w dir)
    (begin (play MMMM)
           (make-player (player-posn p)
                        (player-score p)
                        (+ 1 (player-bump-count p))))
    (move-player-posn p dir)))

; distance helper
(define (distance-between p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

; calculate the player's score as a function of distance between them and all enemies
; calc-score: World -> Number
(define (calc-score w)
  (floor (/ (foldl (lambda (enemy i)
           (+ i (distance-between (player-posn (world-player w)) (enemy-posn enemy))))
           0
           (world-enemies w)) (length (world-enemies w)))))

; find the centroid between enemies
;(define (centroid w)
;  (foldl



; moves the list of enemies
; move-enemies : (listof enemy) -> (listof enemy)
(define (move-enemies loe)
  (map (lambda (e)
         (let ([dir (random-element '(up down left right))])
           (move-enemy e dir)))
       loe))

; moves an enemy one movement unit in a random direction
(define (move-enemy e dir)
  (if (> (enemy-stops-left e) 0)
      (make-enemy (enemy-posn e) (- (enemy-stops-left e) 1))
      (cond [(symbol=? 'up dir) (make-enemy (make-posn (posn-x (enemy-posn e))
                                                       (max 0 (- (posn-y (enemy-posn e)) MOV-CONST)))
                                            (random ENEMY-STOPS-CONST))]
            [(symbol=? 'down dir) (make-enemy (make-posn (posn-x (enemy-posn e))
                                                         (min (image-height SCN) (+ (posn-y (enemy-posn e)) MOV-CONST)))
                                              (random ENEMY-STOPS-CONST))]
            [(symbol=? 'left dir) (make-enemy (make-posn (max 0 (- (posn-x (enemy-posn e)) MOV-CONST))
                                                         (posn-y (enemy-posn e)))
                                              (random ENEMY-STOPS-CONST))]
            [(symbol=? 'right dir) (make-enemy (make-posn (min (image-width SCN) (+ (posn-x (enemy-posn e)) MOV-CONST))
                                                          (posn-y (enemy-posn e)))
                                               (random ENEMY-STOPS-CONST))])))
  

#|
; enemy exit moves an enemy out of the train
; enemy-exit: Enemy Boolean
(define (enemy-exit enemy front-door?)
  (let ([door (if front-door? FRONT-DOOR BACK-DOOR)]
      (cond [(not (= (posn-x door) (posn-x (enemy-posn enemy))))
             ; determine if x should be incremented or decremented, inc/dec
             ]
            [(not (= (posn-y door) (posn-y (enemy-posn enemy))))
|#

; world-tick should introduce new enemies and stuff and count the player's score
(define (world-tick w)
  (make-world (world-player w) (move-enemies (world-enemies w))))

; key-handler: world key -> world
(define (key-handler w k)
  (cond [(or (key=? "up" k) 
             (key=? "down" k) 
             (key=? "left" k) 
             (key=? "right" k))
         (make-world (move-player (world-player w) w (string->symbol k))
                     (world-enemies w))]
        [(key=? "w" k) (make-world (move-player (world-player w) w 'up)
                     (world-enemies w))]
        [(key=? "a" k) (make-world (move-player (world-player w) w 'left)
                     (world-enemies w))]
        [(key=? "s" k) (make-world (move-player (world-player w) w 'down)
                     (world-enemies w))]
        [(key=? "d" k) (make-world (move-player (world-player w) w 'right)
                     (world-enemies w))]
        [else w]))

(define world0 (make-world TEST-PLAYER TEST-ENEMIES))

(big-bang world0
          (on-tick world-tick 0.1)
          (to-draw draw-world)
          (on-key key-handler)
          ;(stop-when game-over?)
          )