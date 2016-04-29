#lang racket

;; ===================================================================================================
;; represent the possible choices an external player may return when asked to pick the next feeding

(require (only-in "basics.rkt" maybe/c natural? natural+?) "cards.rkt")

(define EXTINCTION-CARDS 2)

(define jsexpr/c any/c)

;; Action4 = [List N GP* GB* BT* RT*]  ; the actions each player wishes to perform
;; type GP*     = [Listof GP]       ; the growth-of-population actions
;; type GB*     = [Listof GB]       ; the growth-of-body actions
;; type BT*     = [Listof BT]       ; the board-acquisition actions
;; type RT*     = [Listof RT]       ; the trait-replacement actions
;; type GP      = [List N N]        ; [list b c] trades card c for growth of population b
;; type GB      = [List N N]        ; [list b c] trades card c for growth of population b
;; type BT      = [List N N^0..3]   ; [cons pc t] pays card pc for new species with traits t
;; type RT      = [List N N N]      ; [list b cO cN] replaces trait cO at b for trait card cN
(define action4/c any/c)

(define internal-player/c
  (object/c
   ;; external communications 
   [start     (->m natural?    [maybe/c void?])]
   [feed-next (->m any/c any/c [maybe/c (recursive-contract next/c)])]
   [choose    (->m any/c       [maybe/c action4/c])]
   [apply-card-action
    ;; determine the card that this player wishes to 'donate' to the watering hole
    ;; EFFECT apply the desired actions on cards to this player 
    (->m action4/c card?)]))

(define players/c [listof internal-player/c])

(define dealer-next/c
  (object/c
   [feed-a-player-s-species
    ;; (feed-a-player-s-species ip s) this dealer executes a complete feeding act for ip's species s
    (->m internal-player/c natural? any/c)]
   [feed-scavengers
    ;; (feed-scavengers ip) this dealer feeds all of its scavengers, starting from ip in turn order
    (->m internal-player/c any/c)]
   [give-cards-to
    ;; (give-cards-to ip c) this dealer hands the give cards c to ip
    (->m internal-player/c natural+? any/c)]
   [store-fat
    ;; (store-fat ip s n) this dealer places n fat foods on ip's species s
    (->m internal-player/c natural? natural? any/c)]))

(define next/c
  (object/c
   [interpret (->m dealer-next/c players/c players/c players/c)]
   [to-json   (->m jsexpr/c)]))

(provide
 EXTINCTION-CARDS
 
 dealer-next/c
 internal-player/c
 next/c
 
 (contract-out
  [one-of (-> any/c [listof next/c] boolean?)]
  
  [feed-none           (-> next/c)]
  [feed-vegetarian     (-> natural? next/c)]
  [store-fat-on-tissue (-> natural? natural+? next/c)]
  [feed-carnivore      (-> natural? natural? natural? next/c)]))

;; ===================================================================================================

(require (except-in "basics.rkt" maybe/c natural? natural+?))

;; for debugging
(require  "common.rkt")

(module+ test
  (require rackunit))

;; ===================================================================================================

#| ---------------------------------------------------------------------------------------------------
   interpreter pattern for responses from feed-next

					    +---------------------+
					    |       next%         |
					    +---------------------+
					    | _public_            |
					    | interpret           |
					    | acceptable?         |
					    | self-attack?        |
					    | to-json             |
					    | equal               |
					    +---------------------+
						      |
						      ^
						      |
           +------------------------+------------------------------+-------------------+
           |			    |				   |		       |
+----------------------+   +---------------------+   +---------------------+  +---------------------+
| store-fat-on-tissue% |   |     feed-none%      |   |   feed-vegetarian%  |  |   feed-carnivore%   |
+----------------------+   +---------------------+   +---------------------+  +---------------------+

--------------------------------------------------------------------------------------------------  |#

(define (feed-none) (new feed-none%))
(define (feed-vegetarian s) (new feed-vegetarian% [s s]))
(define (store-fat-on-tissue s n) (new store-fat-on-tissue% [s s][n n]))
(define (feed-carnivore attacker p0 attackee)
  (new feed-carnivore% [attacker attacker][p0  p0] [attackee attackee]))

(define next%
  (class object% 
    (super-new)
    
    ;; this is basically nonsense 
    (define/public (equal-hash-code-of hash-code) 99)
    (define/public (equal-secondary-hash-code-of hash-code) 999)
    
    (define/public (acceptable? other) #false)
    (define/public (self-attack? p) #false)
    
    (abstract interpret to-json)))

;; ---------------------------------------------------------------------------------------------------
(define feed-none%
  (class* next% (equal<%>)
    (super-new)
    
    (define/override (to-json) #false)
    
    (define/public (equal-to? other r)
      (and (is-a? other feed-none%)))
    
    (define/override (interpret dealer feedable-players _)
      (define current-player (first feedable-players))
      (rest feedable-players))))

;; ---------------------------------------------------------------------------------------------------
(define feed-vegetarian%
  (class* next% (equal<%>)
    (init-field s)
    (super-new)
    
    (define/override (to-json) s)
    
    (define/public (equal-to? other r)
      (and (is-a? other feed-vegetarian%)
           (= (get-field s other) s)))
    
    (define/override (interpret dealer players* _)
      (define current-player (first players*))
      (send dealer feed-a-player-s-species current-player s)
      (cyclic-rotate players*))))

;; ---------------------------------------------------------------------------------------------------
(define store-fat-on-tissue%
  (class* next% (equal<%>)
    (init-field s n)
    (super-new)
    
    (define/override (to-json) (list s n))
    
    (define/public (equal-to? other r)
      (and (is-a? other store-fat-on-tissue%)
           (= (get-field s other) s)
           (= (get-field n other) n)))
    
    (define/override (acceptable? other)
      (and (is-a? other store-fat-on-tissue%)
           (= (get-field s other) s)
           (<= n (get-field n other))))
    
    (define/override (interpret dealer feedable-players _)
      (define current-player (first feedable-players))
      (send dealer store-fat current-player s n)
      (cyclic-rotate feedable-players))))

;; ---------------------------------------------------------------------------------------------------
(define feed-carnivore%
  (class* next% (equal<%>)
    (init-field attacker p0 attackee)
    (super-new)
    
    (define/override (to-json) (list attacker p0 attackee))
    
    (define/public (equal-to? other r)
      (and (is-a? other feed-carnivore%)
           (= (get-field attacker other) attacker)
           (= (get-field p0 other) p0)
           (= (get-field attackee other) attackee)))
    
    (define/override (interpret dealer feedable-players attackable-players)
      (cond
        [(= (length attackable-players) p0) #false]
        [else 
         (define current-player (first feedable-players))
         (define owner (list-ref attackable-players p0))
         (define-values (defend-with-horn? d-going-extinct?) (send owner attack! attackee))
         (when d-going-extinct?
           (send dealer give-cards-to owner EXTINCTION-CARDS))
         (cond
           [defend-with-horn?
             (define a-going-extinct? (send current-player kill1 attacker))
             (cond
               [a-going-extinct?
                ;; p is asking a to commit suicide to get some cards:
                (send dealer give-cards-to current-player EXTINCTION-CARDS)]
               [else 
                (send dealer feed-a-player-s-species current-player attacker)
                (send dealer feed-scavengers current-player)])]
           [else
            (send dealer feed-a-player-s-species current-player attacker)
            (send dealer feed-scavengers current-player)])
         (cyclic-rotate feedable-players)]))))

;; ===================================================================================================
(module+ test
  (define fat-0-2 (store-fat-on-tissue 0 2))
  (define none  (feed-none))
  (check-true (one-of (store-fat-on-tissue 0 1) (list fat-0-2)))
  (check-true (one-of none (list none (feed-vegetarian 0) (feed-vegetarian 1))))
  (check-true (one-of fat-0-2 (list (feed-vegetarian 0) fat-0-2)))
  
  (check-false (one-of none (list fat-0-2)))
  (check-false (one-of #f (list fat-0-2))))

(define (one-of next possible-feedings)
  (and (is-a? next next%)
       (for/or ((a-feeding possible-feedings))
         (or (equal? a-feeding next) (send next acceptable? a-feeding)))))

