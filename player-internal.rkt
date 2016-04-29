#lang racket

;; ===================================================================================================
;; represent a base player and an internal from the perspective of the dealer

(require (only-in "board.rkt" species/c)
         (only-in "cards.rkt" valid-card?)
         (only-in "basics.rkt" natural? natural+?)
         (only-in "next.rkt" internal-player/c next/c))

;; constants
(define CARDS-PER-BOARD 1)
(define CARD-PER-PLAYER 3)

(define internal-communication/c
  (and/c
   internal-player/c
   (object/c
    (field
     [id any/c])
    
    [reduce-population-to-food
     ;; (reduce-population-to-food gc) reduces the population of all species boards
     ;; to the amount of food they acquired; gc is a call-back to get cards if it goes
     ;; extinct 
     (->m (-> any/c) any/c)]
    
    [attack!
     ;; (attack! s) executes an attack on this player's board s
     ;;   signal whether the species has horns
     ;;   signal whether the species goes extinct due to attack
     (->dm ([s natural?]) (values [horned? boolean?][gone-extinct? boolean?]))]
    
    [feed1
     ;; (feed1 s) add one token of food to this player's board s
     ;;   signal whether this board is foraging
     ;;   signal whether this board is feeding a specific neighbor s+1 or none
     (->dm ([s natural?])
           (values [foraging? boolean?]
                   [neigbor-of-s [or/c #false (and/c natural? (=/c (+ s 1)))]]))]
    
    [population+1          any/c]
    [kill1          	   any/c]
    [find-all          	   any/c]
    [score                 any/c]
    [move-food-to-bag      any/c]
    [take-cards            any/c]
    [how-many-cards-needed any/c]
    [add-board-if-needed   any/c]
    [all-fed          	   any/c]
    [move-fat          	   any/c]
    [store-fat             any/c]
    [choose          	   any/c]
    [feed-next             any/c]
    [start          	   any/c]
    [can-attack+           any/c]
    [all-attackables 	   any/c]
    [can-attack            any/c]
    [with-fat-tissue 	   any/c]
    [separate-hungries 	   any/c]
    [to-json          	   any/c]
    [equal-secondary-hash-code-of any/c]
    [equal-hash-code-of           any/c]
    [equal-to? 		 	  any/c])))

(provide
 CARDS-PER-BOARD
 CARD-PER-PLAYER
 
 internal-player/c
 
 [contract-out
  [create-player (-> any/c #;==name? any/c #;==external? internal-communication/c)]])

(module+ test
  (define species-not-extinct/c
    (listof (and/c species/c (lambda (s) (> (get-field population s) 0)))))
  
  (provide
   (contract-out
    [player
     (->i ([name any/c #;==natural+?])
          (#:bag      [bag natural-number/c]
           #:cards    [cards (listof valid-card?)]
           #:species  [boards (name) (or/c species-not-extinct/c (-> species-not-extinct/c))]
           #:external [e (-> any)])
          [r internal-player/c])])))

;; ===================================================================================================
(require "player-base.rkt" "internal-external.rkt"
         (except-in "cards.rkt" valid-card?)
         (except-in "next.rkt" internal-player/c)
         (except-in "board.rkt" species/c) 
         (except-in "basics.rkt" natural? natural+?)
         2htdp/image)

;; for debugging
(require "../common.rkt")

(module+ test
  (require rackunit))

;; ===================================================================================================
(define (player? x)
  (is-a? x player%))

(define (create-player name external)
  (define p (new player% [id name]))
  (set-field! external p external)
  p)

(module+ test
  (define (player name
                  #:bag (bag 0)
                  #:cards (cards '())
                  #:species (boards '())
                  #:external (external (lambda () #f)))
    
    (define-syntax-rule
      (set s food)
      (when s (set-field! food s (if (procedure? food) (food) food))))
    
    (define ex (external))
    (set ex boards)
    (set ex bag)
    (set ex cards)
    
    (define s (new player% [id name][external ex]))
    (set s boards)
    (set s bag)
    (set s cards)
    s))

;; ---------------------------------------------------------------------------------------------------
;; internal representation of a player from the dealer's perspective
;; also serves as the communication instrumemt between dealer and external player

(define-local-member-name confirm-choice)

(define player%
  (class* base-player% (equal<%>)
    (init-field
     id
     [external #false])
    
    (super-new)
    
    (inherit with-fat-tissue separate-hungries all-attackables)
    (inherit-field boards bag cards)
    
    ;; -----------------------------------------------------------------------------------------------
    ;; equality 
    
    (define/override (equal-to? other r)
      (and (equal? (get-field id other) id)
           (super equal-to? other r)))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; serialization
    
    (define/override (to-json)
      (cons `("id" ,id) (super to-json)))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (store-fat s n)
      (send (list-ref boards s) store-fat n))
    
    (define/public (move-fat s)
      (send (list-ref boards s) move-fat))
    
    (define/public (apply-card-action per-player)
      (match-define `(,fc ,gp* ,gb* ,bt* ,rt*) per-player)
      (add-board* bt*)
      (modify-board* rt*)
      (grow-population* gp*)
      (grow-body* gb*)
      (begin0
        (card fc)
        ;; careful: removing the cards during the above actions would affect the ordering of the cards
        (remove-cards fc gp* gb* bt* rt*)))
    
    (define/private (add-board* bc*)
      (for ((bc bc*))
        (match-define `(,_payment ,trait-cards ...) bc)
        (define t (map card-trait (map (lambda (i) (card i)) trait-cards)))
        (set! boards (append boards (list (species #:population 1 #:traits t))))))
    
    (define/private (modify-board* tr*)
      (for ((tr tr*))
        (match-define `(,b ,i ,j) tr)
        (define the-board (list-ref boards b))
        (define the-trait (card-trait (list-ref cards j)))
        (send the-board replace-trait i the-trait)))
    
    (define/private (grow-population* gp*)
      (for ((gp gp*))
        (match-define `(,b ,_) gp)
        (define board (list-ref boards b))
        (send board population+1)))
    
    (define/private (grow-body* gb*)
      (for ((gp gb*))
        (match-define `(,b ,_) gp)
        (define board (list-ref boards b))
        (send board body+1)))
    
    (define/private (feedable players*)
      (define fatties (map (lambda (x) (apply store-fat-on-tissue x)) (with-fat-tissue)))
      (define-values (veggies0 carnivores) (separate-hungries))
      (define veggies (map feed-vegetarian veggies0))
      (define possible-attacks
        (if (empty? carnivores)
            '()
            (map (lambda (x) (apply feed-carnivore (rest x)))
                 (for/fold ((attackables '())) ((c carnivores))
                   (append attackables (all-attackables c (strip players*)))))))
      (append veggies fatties possible-attacks))
    
    ;; [Listof Player] -> [Listof Board]
    (define/private (strip others)
      (for/list ((o others)) (get-field boards o)))
    
    (define/public (all-fed s)
      (send (list-ref boards s) all-fed?))
    
    (define/public (remove-cards fc gp gb bt tr)
      (define idx (cons fc (append (map second gp) (map second gb) (apply append bt) (map third tr))))
      (set! cards (remove-by-index idx cards)))
    
    (define/private (card c)
      (list-ref cards c))
    
    (define/public (add-board-if-needed)
      (when (empty? boards)
        (set! boards (list (species #:population 1)))))
    
    (define/public (how-many-cards-needed)
      (+ (* (length boards) CARDS-PER-BOARD) CARD-PER-PLAYER))
    
    (define/public (take-cards c)
      (set! cards (append c cards)))
    
    (define/public (move-food-to-bag)
      (define all-food
        (for/sum ((b boards))
          (send b move-food-to-bag)))
      (set! bag (+ all-food bag)))
    
    (define/public (score)
      (+ bag
         (length boards)
         (for/sum ((b boards)) (send b score))))
    
    (define/public (find-all p? action)
      (for ((s boards) (i (in-naturals)) #:when (send s has p?))
        (action this i)))
    
    (define/public (feed1 s)
      (define-values (foraging? cooperating?) (send (list-ref boards s) feed1))
      (values foraging? (and cooperating? (list-ref/c boards (+ s 1)) (+ s 1))))
    
    (define/public (next watering-hole in-attackable-order)
      (cond
        [(= watering-hole 0) (feed-none)]
        [else 
         (define possible-feedings (feedable in-attackable-order))
         (cond
           [(empty? possible-feedings) (feed-none)]
           [(one? possible-feedings) (first possible-feedings)]
           [else
            ;; NOTE this function computes all feeding possibilities and
            ;; checks whether the player's response is one of them
            ;; if this becomes a bottleneck determine only whether there is more
            ;; than one possibility, and then check validity of response
            (define next (feed-next watering-hole in-attackable-order))
            (and (one-of next possible-feedings) next)])]))
    
    (define/public (attack! i)
      (define s (list-ref boards i))
      (define-values (has-horns? extinct?) (send s attack!))
      (values has-horns? (kill-off-species i extinct?)))
    
    (define/public (kill1 i)
      (define a (list-ref boards i))
      (define extinct? (send a kill1))
      (kill-off-species i extinct?))
    
    (define/public (reduce-population-to-food get-cards)
      (for/fold ((position-of-b 0)) ((b boards))
        (define extinct? (send b reduce-population-to-food))
        (cond
          [(not extinct?) (+ position-of-b 1)]
          [else (kill-off-species position-of-b #true)
                (get-cards)
                position-of-b])))
    
    (define/public (population+1 i)
      (send (list-ref boards i) population+1))
    
    ;; N Boolean -> Boolean 
    (define/private (kill-off-species i extinct?)
      (when extinct? (set! boards (remove-i-th boards i)))
      extinct?)
    
    ;; -----------------------------------------------------------------------------------------------
    ;; interactions with external player
    
    (define-syntax-rule (okay> x) (let ([y x]) (if (client-error? y) #false y))) 
    
    (define/override (start watering-hole)
      (okay> (send-to-external id external start watering-hole bag boards cards)))
    
    (define/override (feed-next food-in-watering-hole others)
      (define others-boards (strip others))
      (okay>
       (send-to-external id external feed-next bag boards cards food-in-watering-hole others-boards)))
    
    (define/override (choose players)
      (define players-state (strip players))
      (define-values (before after) (split-at players-state (my-position players)))
      (confirm-choice (okay> (send-to-external id external choose before after))))
    
    ;; Any -> (Maybe Action4)
    (define/public (confirm-choice x)
      (match x
        [(? client-error? x) x]
        [`(,(? natural? fc)
           [,(? (NxN? 1 1) gp*) ...]
           [,(? (NxN? 1 1) gb*) ...]
           [,(? (NxN? 0 MAX-TRAITS) bt*) ...]
           [,(? (NxN? 2 2) rt*) ...])
         (if (validate? fc gp* gb* bt* rt*) x (cheating 1 x))]
        [else (cheating 2 x)]))
    
    ;; [Listof Player] -> N
    ;; find my position in the sequence of players 
    (define/private (my-position players)
      (for/first ((p players) (i (in-naturals)) #:when (equal? (get-field id p) id)) i))
    
    ;; N Any -> False
    ;; EFFECT log a problem report 
    (define/private (cheating n x)
      (when x
        (log-info "internal says: player ~a produced ill-formed result (~a): ~e" id n x))
      #false)
    
    ;; FC GP* GB* BT* RT* -> Boolean
    (define/private (validate? fc gp* gb* bt* rt*)
      (and
       (valid-action-cards fc gp* gb* bt* rt*)
       (valid-action-on-boards gp* gb* bt* rt*)
       (validate-growth gp* population/c (lambda (i) (get-field population i)))
       (validate-growth gb* body/c (lambda (i) (get-field body i)))
       (creates-valid-boards bt*)
       (valid-trait-replacements rt* bt*)))
    
    ;; FC GP* GB* BT* RT* -> Boolean
    ;; the choosen action card indexes are in possesion of player and they form a set
    (define/private (valid-action-cards fc gp* gb* bt* rt*)
      (define cards-in (cons fc (apply append (map second gp*) (map second gb*) (map third rt*) bt*)))
      (and (to-set cards-in) (valid-indexes (length cards) cards-in)))
    
    ;; [Listof [List N N]] -> Boolean
    ;; the select group growth must be within bounds
    (define/private (validate-growth growth check select)
      (define board# (length boards))
      (for/and ([g (group-by first growth)])
        (define brd# (first (first g)))
        (define plus (length g))
        (check
         (cond
           [(< brd# board#) (+ (select (list-ref boards brd#)) plus)]
           [else (+ plus 1)]))))
    
    ;; GP* GB* BT* RT* -> Boolean
    ;; the actions on boards (growth of pop. and body & trait replacement) apply to existing boards
    (define/private (valid-action-on-boards gp* gb* bt* rt*)
      (define boards-to-be-modified (append (map first gp*) (map first gb*) (map first rt*)))
      (valid-indexes (+ (length boards) (length bt*)) boards-to-be-modified))
    
    ;; BT* -> Boolean
    ;; the boards created must consist of a __set__ of traits
    (define/private (creates-valid-boards bt*)
      (for/and ((new-board bt*)) (index-to-traits (rest new-board))))
    
    ;; N [Listof N] -> Boolean
    (define/private (valid-indexes n idx)
      (for/and ((i idx)) (< i n)))
    
    ;; RT* BT* -> Boolean
    ;; the desired trait replacements are within the bounds of the specified boards
    ;; the boards created need to remain __sets__ of traits
    (define/private (valid-trait-replacements rt* bt*)
      (define boards# (length boards))
      (for/and ((rt rt*))
        (match-define `(,b ,i ,j) rt)
        (cond
          [(< b boards#)
           (define the-board  (list-ref boards b))
           (define its-traits (get-field traits the-board))
           (and (< i (length its-traits))
                (to-set (cons (card-trait (list-ref cards j)) (remove-i-th its-traits i))))]
          [else
           (define the-board  (list-ref bt* (- b boards#)))
           (define its-traits (rest the-board))
           (and (< i (length its-traits))
                (index-to-traits (cons j (remove-i-th its-traits i))))])))
    
    ;; [Listof N] -> [Maybe [Setof Trait]]
    (define/private (index-to-traits idx)
      (to-set (for/list ((new-card idx)) (card-trait (list-ref cards new-card)))))))

;; ===================================================================================================
(module+ test
  (require rackunit)
  
  (define (player-nox name #:cards (cards '()) #:species (boards0 #f))
    (define boards (or boards0 (list (species))))
    (define-syntax-rule
      (set s food)
      (when s (set-field! food s (if (procedure? food) (food) food))))
    
    (define s (new player% [id name]))
    (set s boards)
    (set s cards)
    s)
  
  (check-false (send (player-nox 1) confirm-choice '()))
  (check-false (send (player-nox 1) confirm-choice 43))
  
  (define min-action4 '[0 [] [] [] []])
  (check-false (send (player-nox 1) confirm-choice min-action4))
  
  (define p-with-4cards (player-nox 2 #:cards (take all-cards 4)))
  (check-equal? (send p-with-4cards confirm-choice min-action4) min-action4)
  
  (define food+bt-action4 '[0 [] [] [(1 2)] []])
  (check-equal? (send p-with-4cards confirm-choice food+bt-action4) food+bt-action4)
  
  ;; -------------------------------------------------------------------------------------------------
  ;; a player gets 8 cards, the first two have the same trait & the first differs from last
  
  (define 8cards (take all-cards 8))
  (define p-with-8cards (player-nox 2 #:cards 8cards))
  
  ;; --- good card choices 
  (define food+bt+gp '[0 [(1 4)] [] [(1 2)] []])
  (check-equal? (send p-with-8cards confirm-choice food+bt+gp) food+bt+gp)
  
  (define food+bt+gp+gb '[0 [(1 4)] [(1 5)] [(1 2)] []])
  (check-equal? (send p-with-8cards confirm-choice food+bt+gp+gb) food+bt+gp+gb)
  
  (define food+bt+gp+gb+2rt '[0 [(1 4)] [(1 5)] [(1 2)] [(1 0 6) (1 0 7)]])
  (check-equal? (send p-with-8cards confirm-choice food+bt+gp+gb+2rt) food+bt+gp+gb+2rt)
  
  ;; --- bad card choices
  (define card1 (first 8cards))  (define card1# 0)
  (define card2 (second 8cards)) (define card2# 1)
  
  (check-equal? (card-trait card1) (card-trait card2))
  (define bt-double-trait `[2 [] [] [(3 ,card1# ,card2#)] []])
  (check-false (send p-with-8cards confirm-choice bt-double-trait) 
               "new boards don't get double traits (failed during tournament)")
  
  (define card8 (last 8cards))   (define card8# 7)
  
  (check-equal? (card-trait card1) (card-trait card2))
  (check-not-equal? (card-trait card1) (card-trait card8))
  (define rt-double-trait `[2 [] [] [(3 ,card1# ,card8#)] [(1 1 ,card2#)]])
  (check-false (send p-with-8cards confirm-choice rt-double-trait)
               "replacements don't double traits on new boards (failed during tournament)")
  
  (check-equal? (card-trait card1) (card-trait card2))
  (check-not-equal? (card-trait card8) (card-trait card1))
  (define species-with-2traits     (list (species #:traits (map card-trait (list card8 card1)))))
  (define p-with-8cards-and-trait1 (player-nox 2 #:cards 8cards #:species species-with-2traits))
  (define rt-double-existing-trait `[2 [] [] [] [(0 0 ,card2#)]])
  (check-false (send p-with-8cards-and-trait1 confirm-choice rt-double-existing-trait)
               "replacements don't double existing traits (failed during tournament)")
  
  (define cards# (+ MAX-POPULATION 2))
  (define more-cards (take all-cards cards#))
  (define p-simple  (player-nox 2 #:cards more-cards))
  
  (define (gp-existing-board feed nu)
    `[0 ,(build-list MAX-POPULATION (lambda (i) `(,feed ,(+ i 1)))) [] ,nu []])
  (check-false (send p-simple confirm-choice (gp-existing-board 0 '[]))
               "increase population of the default board by too much")
  (check-false (send p-simple confirm-choice (gp-existing-board 1 `[,(- cards# 1)]))
               "increase population of a new board by too much")

  (define (gb-existing-board feed nu)
    `[0 [] ,(build-list (+ MAX-BODY 2) (lambda (i) `(,feed ,(+ i 1)))) ,nu []])
  (check-false (send p-simple confirm-choice (gb-existing-board 0 '[]))
               "increase body of the default board by too much")
  (check-false (send p-simple confirm-choice (gb-existing-board 1 `[,(- cards# 1)]))
               "increase body of a new board by too much"))
