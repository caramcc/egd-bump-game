;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bump_alpha) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require rsound)
(require racket/runtime-path)

#| ABOUT/ TODO
personal space optimization as measured by the distance between the player
and the other npcs in the scene

every 5 seconds a new score snapshot is taken, every 10-15 a new enemy is added,
and they shuffle in and out and adjust and move around

the player grows redder as an indication of how badly they're doing. 

|#

;;;;; STATE
(define GAME-STARTED #f)

;;;;; PALETTE
(define BG-YELLOW (make-color 235 227 170))
(define MELON (make-color 202 215 178))
(define AQUA (make-color 168 202 186))
(define SLATE (make-color 131 134 137))
(define BURGUNDY (make-color 93 65 87))

;; TODO
;; make a tesselated ever-shifting background of random pretty triangles

;;;;; UTILITIES ;;;;;
(define (random-element list)
  (list-ref list (random (length list))))

(define (make-grid x y spacing)
  (foldl (lambda (l base)
           (place-image (line 0 (* 2 y) MELON) l l 
                        (place-image (line (* 2 x) 0 MELON) l l base)))
         (rectangle x y "solid" BG-YELLOW)
         (range 0 (max x y) spacing)))

(define (gen-enemies n)
  (if (= n 0)
      null
      (cons (make-enemy (make-posn (random-element (range 0 500 25))
                                   (random-element (range 0 500 25)))
                        (random-element (range 0 7 1)))
            (gen-enemies (- n 1)))))



(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (intersects-player? e p)
  (posn=? (player-posn p) (enemy-posn e)))

(define (intersects-enemies? p w dir)
  (let ([moved-p (move-player-posn p dir)])
    (ormap (lambda (e) (intersects-player? e moved-p)) (world-enemies w))))


;;;;; STRUCTS ;;;;;
; a player is a Posn Number Number
(define-struct player (posn score bump-count))

; an enemy is a Posn Number where the number is the number of stops left
(define-struct enemy (posn stops-left))

; a world is a Player and (Listof Enemy)
(define-struct world (player enemies level))

; a level is a Number Posn
(define-struct level (number goal)) 


;;;;; CONSTANTS ;;;;;
(define-runtime-path MMMM-PATH "./mmm.wav")
(define MMMM (rs-read MMMM-PATH))
(define ENEMY-IMG (star 25/2 'solid 'black))
(define PLAYER-START (make-posn 25 25))
(define PLAYER-IMG (overlay (star 25/2 'solid BURGUNDY) (circle 25/2 'solid AQUA)))

(define GOAL-IMG (overlay (star 25/2 'solid BG-YELLOW) (circle 25/2 'solid SLATE)))
(define SCN (make-grid 500 500 25))
(define MOV-CONST 25)
(define ENEMY-STOPS-CONST 5)
(define FRONT-DOOR (make-posn 50 0))
(define BACK-DOOR (make-posn 650 0))

(define RED-SCN (square 500 'solid 'red))

(define TEST-ENEMIES (gen-enemies 10))
  ;(list (make-enemy (make-posn 50 75) 3)
   ;                        (make-enemy (make-posn 300 100) 4)
    ;                       (make-enemy (make-posn 75 75) 1)
     ;                      (make-enemy (make-posn 400 400)  5)))
(define TEST-PLAYER (make-player PLAYER-START 0 0))

(define (random-goal n)
  (make-posn (random-element (range (* 50 n) (* 100 n) 25))
             (random-element (range (* 50 n) (* 100 n) 25))))

(define TEST-LEVEL (make-level 1 (random-goal 1)))

;;;;; FUNCTIONS ;;;;;

(define (draw-enemy enemy scene)
  (place-image ENEMY-IMG
               (posn-x (enemy-posn enemy))
               (posn-y (enemy-posn enemy))
               scene))

(define (draw-world w)
  (let ([p (world-player w)]
        [l (world-level w)])        
    (place-image (text (string-append "Bumps: " (number->string (player-bump-count p))) 12 BURGUNDY)
                 100 25
                 (place-image PLAYER-IMG
                              (posn-x (player-posn p))
                              (posn-y (player-posn p))
                              (place-image GOAL-IMG
                                           (posn-x (level-goal l))
                                           (posn-y (level-goal l))
                                           (foldl draw-enemy
                                                  (make-grid (* 100 (level-number (world-level w)))
                                                             (* 100 (level-number (world-level w)))
                                                             25)
                                                  (world-enemies w)))))))



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

(define (bump p)
  (begin (play MMMM)
         (make-player (player-posn p)
               (player-score p)
               (+ 1 (player-bump-count p)))))

(define (move-player p w dir)
  (if (intersects-enemies? p w dir)
      (bump p)
      (move-player-posn p dir)))


; add-bumps : player number -> player
; bumps 
(define (add-bumps p count)
  (if (= 0 count)
      p
      (add-bumps (bump p) (sub1 count))))


; moves the list of enemies, increasing bump count every bump
; move-enemies : world->world
(define (move-enemies w)
  (letrec ([loe (world-enemies w)]
        [p   (world-player w)]
        [bump-count 0]
        [local-loe
             (map (lambda (e)
                    (letrec ([dir (random-element '(up down left right))]
                           [moved-e (move-enemy e dir)])
                      (if (intersects-player? moved-e p)
                          (begin (set! bump-count (add1 bump-count)) e)
                          moved-e)))
                  loe)])
      (make-world (add-bumps p bump-count) local-loe (world-level w))))

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

; next-level : world -> world
(define (next-level w)
  (let ([p (world-player w)]
        [e (world-enemies w)]
        [ln (level-number (world-level w))])
  (make-world (make-player PLAYER-START
                           (+ 10 (player-score p))
                           (player-bump-count p))
              (append e (gen-enemies 10))
              (make-level (min (+ 1 ln) 5)
                          (random-goal ln)))))
                           

; player-at-goal? : World -> Boolean
(define (player-at-goal? w)
  (posn=? (player-posn (world-player w))
          (level-goal (world-level w))))

; world-tick should introduce new enemies and stuff and count the player's score
(define (world-tick w)
  (if GAME-STARTED
      (if (player-at-goal? w)
          (next-level w)
          (move-enemies w))
      w))

; key-handler: world key -> world
(define (key-handler w k)
  (begin (set! GAME-STARTED #t)
  (cond [(or (key=? "up" k) (key=? "w" k))
         (make-world (move-player (world-player w) w 'up) (world-enemies w) (world-level w))]
        [(or (key=? "left" k) (key=? "a" k))
         (make-world (move-player (world-player w) w 'left) (world-enemies w) (world-level w))]
        [(or (key=? "down" k) (key=? "s" k))
         (make-world (move-player (world-player w) w 'down) (world-enemies w) (world-level w))]
        [(or (key=? "right" k) (key=? "d" k))
         (make-world (move-player (world-player w) w 'right)(world-enemies w) (world-level w))]
        [(key=? " " k) (make-world (world-player w)
                                   (append (gen-enemies 350) (world-enemies w))
                                   (world-level w))]
        [else w])))

; game over when enemies > 350 (theoretical max. 400)
(define (game-over? w)
  (>= (length (world-enemies w)) 350))

(define (draw-world-wrapper w)
  (if GAME-STARTED
      (draw-world w)
      (draw-welcome-screen)))

(define (draw-welcome-screen)
  (place-image (text "Navigate through the crowd using WASD or arrow keys to each goal."
                 14
                 BURGUNDY)
               250 225
           (place-image
            (text "Press spacebar to give up."
                 14
                 BURGUNDY)
            250 250
            (place-image (text "Press any other key to start."
                 14
                 BURGUNDY)
                         250 275
            (rectangle 500 500 "solid" BG-YELLOW)))))

(define (draw-game-over w)
  (place-image (text (string-append "You were bumped: "
                                    (number->string (player-bump-count (world-player w)))
                                    " times.")
                     24
                     'black)
               250
               250
               (place-image (text (string-append "The biggest crowd you made it through was "
                                                 (number->string (player-score (world-player w)))
                                                 " people.")
                                  12
                                  BURGUNDY)
                            250
                            (+ 250 24)
                            (rectangle 500 500 "solid" BG-YELLOW))))
  
(define world0 (make-world TEST-PLAYER TEST-ENEMIES TEST-LEVEL))

(big-bang world0
          (on-tick world-tick 0.1)
          (to-draw draw-world-wrapper 500 500)
          (on-key key-handler)
          (stop-when game-over? draw-game-over)
          ;(display-mode 'fullscreen)
          )