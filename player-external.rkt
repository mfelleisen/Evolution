#lang racket

;; ===================================================================================================
;; the silly player -- one possible implementation of the external player

;; EXTERNAL SERVICES

(require (only-in "next.rkt" external-player/c action4/c pre-choose)
         (only-in "cards.rkt" card?)
         (only-in "basics.rkt" natural? natural+?))

(provide
 (contract-out
  [create-external (-> (external-player/c pre-choose))])
 
 ;; [Natural Boards Cards Natural Players -> Any] -> ExternalPlayer
 ;; creating players that act badly during feed-next
 create-bad-feed
 
 ;; Natural -> [Natural Boards Cards Natural Players -> Any]
 ;; ways to act badly during feed-next after a while 
 feed-inf
 feed-bad 
 
 ;; [Cards Boards -> Any] -> ExternalPlayer
 ;; creating players that act badly durinh choose
 create-bad-choose
 
 ;; Natural -> [Cards Boards -> Any]
 ;; ways to act badly during choose after a while 
 no-fc
 over-growth
 choice-inf)

;; ===================================================================================================
;; DEPENDENCIES
(require "player-base.rkt" "traits.rkt"
         (except-in "next.rkt" external-player/c action4/c pre-choose)
         (except-in "board.rkt" species/c)
         (except-in "cards.rkt" card?))

;; for debugging:
(require "common.rkt")

(module+ test
  (require rackunit (submod ".."))
  (require (submod "common.rkt" test) "common.rkt"))

;; ===================================================================================================
;; IMPLEMENTATION

(define (create-external)
  (new external%))

(define (create-bad-feed bad-feed)
  (new external% [bad-feed-next bad-feed]))

(define (feed-inf n)
  (lambda (_bag _boards _cards _food-in-watering-hole _others)
    (cond
      [(<= n 0) (let loop () (loop))]
      [else
       (set! n (- n 1))
       #false])))

(define (feed-bad n)
  (lambda (_bag _boards _cards _food-in-watering-hole _others)
    (cond
      [(<= n 0) (feed-vegetarian (* (length all-cards) 2))]
      [else
       (set! n (- n 1))
       #false])))

(define (create-bad-choose bad-choose)
  (new external% [bad-choose bad-choose]))

(define (no-fc n)
  (lambda (_cards _board)
    (cond
      [(<= n 0) '()]
      [else
       (set! n (- n 1))
       #false])))

(define (over-growth n)
  (lambda (_cards boards)
    (cond
      [(<= n 0) `[0 ((,(+ (length boards) 2) 1)) () () ()]]
      [else
       (set! n (- n 1))
       #false])))

(define (choice-inf n)
  (lambda (_cards boards)
    (cond
      [(<= n 0) (let loop () (loop))]
      [else
       (set! n (- n 1))
       #false])))

;; ---------------------------------------------------------------------------------------------------
;; an external player with a specific strategy
(define external%
  (class* base-player% (equal<%>)
    
    (init-field
     [bad-choose
      ;; called on every call to choose; if it produces non-#f, that value is returned from choose
      (lambda (_cards _boards) #false)]
     [bad-feed-next
      ;; called on every call to feed-next; if it produces non-#f, that value is returned from choose
      (lambda (_bag _boards _cards _food-in-watering-hole _others) #false)])
    
    (super-new)
    
    (inherit-field boards bag cards)
    (inherit with-fat-tissue separate-hungries can-attack can-attack+)
    
    ;; -----------------------------------------------------------------------------------------------
    ;; EFFECT just accept what given 
    (define/override (start _watering-hole bag-turn boards-turn cards-turn)
      (set!-values (bag boards cards) (values bag-turn boards-turn cards-turn)))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Deploy cards in <-card order. Use first card for food card & ask for a species with one trait.
    ;; If cards left: (1) grow the board that was added and (2) replace trait on new species board.
    ;; ASSUME the player has at least one board and at least four cards (beginning of turn) 
    ;; EFFECT drop cards used for action
    
    (define/override (choose _before _after)
      (define bad? (bad-choose cards boards))
      (cond
        [bad? bad?]
        [else 
         (define index-of second)
         ;; X [Listof [List Card N]] Bookean ->* [Listof [List N N]] [Listof Card]
         (define (if-possible _label c [add '()])
           (if (empty? c)
               (values '() c)
               (values `[(,(length boards) ,@add ,(index-of (first c)))] (rest c))))
         (define indexed-cards (for/list ((c cards) (i (in-naturals))) (list c i)))
         (let*-values ([(c)     (sort indexed-cards <-card #:key first)]
                       [(fc  c) (values (index-of (first c)) (rest c))]
                       [(bt* c) (values `[(,(index-of (first c)) ,(index-of (second c)))] (cddr c))]
                       [(gp* c) (if-possible "grow population" c)]
                       [(gb* c) (if-possible "grow body" c)]
                       [(rt* c) (if-possible "replace trait card on new board" c '(0))])
           (set! cards (map first (sort c < #:key second)))
           (list fc gp* gb* bt* rt*))]))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; The player feeds the species according to the following strategy:
    ;; (0) It feeds species with fat-tissues that haven't stored up as much as possible. 
    ;; 
    ;; (1) It feeds vegetarians according to the ordering imposed on the species. 
    ;;
    ;; (2) If there are no hungry vegetarians,
    ;; the player feeds the carnivores that can attack some other species from the other players.
    ;; Their species are arranged in one long sequence according to the order given. 
    ;; If more than one species can be attacked, the 'first' species of all players' species
    ;; is attacked.
    ;;
    ;; (3) The player will not use one of its carnivores to attack any of its own species.
    
    (define-syntax-rule
      (send/i target method args ...)
      (send (list-ref boards target) method args ...))
    
    (define/override (feed-next bag boards cards food-in-watering-hole others)
      (start food-in-watering-hole bag boards cards)
      (define bad? (bad-feed-next bag boards cards food-in-watering-hole others))
      (cond
        [bad? bad?]
        [else 
         (define me (length others))
         (define fatties (with-fat-tissue))
         (cond
           [(cons? fatties)
            (apply store-fat-on-tissue (argmax second (sort/i fatties #:key first)))]
           [else 
            (define-values (veggies carnivores) (separate-hungries))
            (cond
              [(cons? veggies) (feed-vegetarian (first (sort/i veggies)))]
              [(cons? carnivores)
               (define sorted (sort/i carnivores))
               (define attackable-other-species
                 (for*/first ([c sorted][a (in-value (can-attack c others >-species))] #:when a) a))
               (define attackable-my-species
                 (for*/first ([c sorted] [a (in-value (can-attack+ c me boards))] #:when (cons? a))
                   (map rest a)))
               (cond
                 [attackable-other-species (apply feed-carnivore attackable-other-species)]
                 [(cons? attackable-my-species) (feed-none)]
                 [else (define/contract species/post no-attackable-species/c boards) species/post])]
              [else
               (define/contract species/post exists-hungry/c #false) species/post])])]))
    
    ;; [Listof N] -> [Listof N]
    ;; sort the list of indexes according to their interpretation by numbers 
    (define/private (sort/i s* #:species [boards boards] #:key [k values])
      (sort s* (lambda (s1 s2) (>-species (list-ref boards s1) (list-ref boards s2))) #:key k))))

(define (False x) #f)

(define exists-hungry/c
  (flat-named-contract "there are no hungry species or species with the fat-tissue trait" False))

(define no-attackable-species/c
  (flat-named-contract "there is a hungry carnivore but nothing to attack, not even itself" False))

;; ---------------------------------------------------------------------------------------------------
;; Species Species -> Boolean
;; comapres two species in lexicographic order: population, food, body size

(module+ test
  (define sp1 (species #:body 0 #:food 1 #:population 1))
  (define sp2 (species #:body 1 #:food 0 #:population 1))
  (define sp3 (species #:body 1 #:food 2 #:population 2))
  
  (check-true (>-species sp1 sp2))
  (check-true (>-species sp3 sp1))
  (check-true (>-species sp3 sp2))
  
  (check-false (>-species sp3 sp3))
  (check-false (>-species sp1 sp3))
  (check-false (>-species sp2 sp3)))

(define (>-species s1 s2)
  (or (> (get-field population s1) (get-field population s2))
      (and (= (get-field population s1) (get-field population s2))
           (> (get-field food s1) (get-field food s2)))
      (and (= (get-field population s1) (get-field population s2))
           (= (get-field food s1) (get-field food s2))
           (> (get-field body s1) (get-field body s2)))))

;; ===================================================================================================
(module+ test
  (testing (lambda (x y z) (send x feed-next y z)))
  
  ;; an extension that simulates the calling behavior for the test suite via an internal player
  (define external-stub%
    (class external%
      (super-new)
      (inherit-field bag boards cards)
      (define/override (feed-next watering-hole others)
        (define other-boards (for/list ((o others)) (get-field boards o)))
        (super feed-next bag boards cards watering-hole other-boards))))
  
  (define (player/x name #:bag (b 0) #:cards (c '()) #:species (s '()))
    (define ex (new external-stub%))
    (set-field! bag ex b)
    (set-field! boards ex s)
    (set-field! cards ex c)
    ex)
  
  (define p0 (player/x 100))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; feed-next tests
  
  (define s1 (species #:body 3 #:food 0 #:population 1 #:traits '()))
  (define s2 (species #:body 3 #:food 1 #:population 1 #:traits '()))
  (define s3 (species #:body 3 #:food 0 #:population 1 #:traits `(,carnivore)))
  (define s4 (species #:body 3 #:food 0 #:population 1 #:traits `(,climbing)))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; feed the one hungry vegetarian of a player 
  (define p1 (player/x 1 #:species (list s1 s2 s3)))
  (define p1.1 (player/x 55 #:species (list s1 s4)))
  (define p1.2 (player/x 66 #:species (list s2)))
  
  (run-testing p1 1 '() (feed-vegetarian 0) "feed the one hungry vegetarian")
  (run-testing p1.1 1 '() (feed-vegetarian 0) "feed the hungrier vegetarian in order")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; tell dealer that the player will not attack its own species 
  (define p2 (player/x 2 #:species (list s3 s2)))
  
  (run-testing p2 1 '() (feed-none) "will not attack my own species")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; pick the 'first' attackable species from a player that has three of them,
  ;; after skipping over a player that has none and another one that has no attackable one
  (define p3 (player/x 3 #:species (list s4)))
  
  (run-testing p2 1 (list p0 p3 p1) (feed-carnivore 0 2 1) "three players")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; seveeral players' species can be attacked 
  
  (define p4 (player/x 4 #:species (list s2)))
  (define p5 (player/x 5 #:species (list s2)))
  (define p6 (player/x 6 #:species (list s3)))
  (run-testing p2 1 (list p0 p3 p4 p5 p6) (feed-carnivore 0 2 0) "five players")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; add a fat-tissue card
  
  (define s-ft (species #:body 2 #:food 0 #:population 3 #:traits `(,fat-tissue)))
  (define p-ft (player/x 7 #:species (list s1 s-ft s2 s3)))
  
  (run-testing p-ft 2 (list p0) (store-fat-on-tissue 1 2) "fat tissue card")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; add two fat-tissue cards to two different species, one with more needs than the other 
  (define s-f2 (species #:body 5 #:food 0 #:population 3 #:traits `(,fat-tissue)))
  (define p-f2 (player/x 8 #:species (list s-ft s-f2)))
  
  (run-testing p-f2 2 (list p0) (store-fat-on-tissue 1 5) "2 fat tissue cards, 1 more need")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; add two fat-tissue cards to two different species, both with the same needs, sortable 
  (define s-f3 (species #:body 3 #:food 0 #:population 2 #:traits `(,fat-tissue)))
  (define s-f4 (species #:body 3 #:food 0 #:population 3 #:traits `(,fat-tissue)))
  (define p-f3 (player/x 9 #:species (list s-f3 s-f4)))
  
  (run-testing p-f3 2 '() (store-fat-on-tissue 1 3) "fat, same needs, sortable")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; add two fat-tissue cards to two different species, both with the same needs, not sortable
  (define s-f5 (species #:body 3 #:food 0 #:population 3 #:traits `(,fat-tissue ,climbing)))
  (define p-f4 (player/x 10 #:species (list s-f5 s-f4)))
  
  (run-testing p-f4 3 '() (store-fat-on-tissue 0 3) "fat, same needs, not sortable")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; add fat-tissue card that can store additional food 
  (define (s-f6 ff)
    (species #:body 3 #:fat-food ff #:food 3 #:population 3 #:traits `(,fat-tissue ,climbing)))
  (define (p-f5 ff) (player/x 11 #:species (list (s-f6 ff))))
  
  (run-testing (p-f5 2) 3 '() (store-fat-on-tissue 0 1) "fat with some food, needs more (1)")
  (run-testing (p-f5 1) 2 '() (store-fat-on-tissue 0 2) "fat with some food, needs more(2)"))

;; these are the tests from #5 turned into feed-next tests 
(module+ test
  
  ;; -------------------------------------------------------------------------------------------------
  (define (attacker1 2traits)
    (species #:body 3 #:food 2 #:population 4 #:traits `(,carnivore ,@2traits)))

  ;; -------------------------------------------------------------------------------------------------
  (define att-plain (attacker1 '()))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; this exn test covers a case I overlooked in #6
  
  (define def-wc+hard
    (species #:body 2 #:food 3 #:population 3 #:traits `(,hard-shell ,warning-call)))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; the standard result 
  (define zero (feed-carnivore 0 0 0))
  (define none (feed-none))
  
  (define def-plain (species #:body 1 #:food 3 #:population 4))
  (define p501 (player/x 501 #:species (list att-plain)))
  (define p502 (player/x 502 #:species (list def-plain)))
  (run-testing p501 9 (list p502) zero "plain attack")
  
  (define (def-burrow f p) (species #:body 1 #:food f #:population p #:traits `(,burrowing)))
  (define p503 (player/x 503 #:species (list (def-burrow 1 1))))
  (define p504 (player/x 504 #:species (list (def-burrow 3 4))))
  
  (run-testing p501 1 (list p503) none "defend with burrowing")
  (run-testing p501 1 (list p504) zero "overcome burrowing")
  
  (define def-climbing (species #:body 1 #:food 3 #:population 4 #:traits `(,climbing)))
  (define att-climbing (attacker1 `(,climbing)))
  (define p511 (player/x 511 #:species (list att-climbing)))
  (define p512 (player/x 512 #:species (list def-climbing)))
  
  (run-testing p501 1 (list p512) none "defend with climbing")
  (run-testing p511 1 (list p512) zero "overcome climbing")
  
  (define def-hard (species #:body 2 #:food 2 #:population 3 #:traits `(,hard-shell)))
  (define p521 (player/x 521 #:species (list def-hard)))
  
  (run-testing p501 1 (list p521) none "defend with hard shell")
  
  (define att-big (species #:body 7 #:food 2 #:population 3 #:traits `(,carnivore)))
  (define att-pack (species #:body 3 #:food 2 #:population 4 #:traits `(,carnivore ,pack-hunting)))
  (define p531 (player/x 531 #:species (list att-big)))
  (define p532 (player/x 532 #:species (list att-pack)))
  
  (run-testing p531 1 (list p521) zero "overcome hard shell with large size")
  (run-testing p532 1 (list p521) zero "overcome hard shell with pack hunting")
  
  (define def-wc (species #:body 2 #:food 2 #:population 3 #:traits `(, climbing ,warning-call)))
  (define att-ambush (attacker1 `(,ambush)))
  (define p541 (player/x 541 #:species (list att-ambush)))
  (define p542 (player/x 542 #:species (list def-wc def-plain)))
  (define p543 (player/x 543 #:species (list def-plain def-wc)))
  (define p544 (player/x 543 #:species (list def-wc def-plain def-wc)))
  
  ;; exception:
  
  (run-testing p501 1 (list p542) none "defend with warning call left")
  (run-testing p501 1 (list p543) none "defend with warning call right")
  (run-testing p501 1 (list p544) none "defend with warning call both")
  (run-testing p541 1 (list p543) zero "overcome with warning call ambush")
  
  (define (def with) (species #:body 2 #:food 2 #:population 2 #:traits `(,hard-shell ,@with)))
  (define att-ambush-pack (attacker1 `(,ambush ,pack-hunting)))
  (define p551 (player/x 551 #:species (list att-ambush-pack)))
  (define p552 (player/x 552 #:species (list def-wc (def '()))))
  (define p553 (player/x 553 #:species (list def-wc (def `(,climbing)))))
  
  (run-testing p551 1 (list p552) (feed-carnivore 0 0 1) "overcome hards w/ pack")
  (run-testing p551 1 (list p553) none "defend mix w/ climbing"))

(module+ test
  ;; testing choose for external (silly) player
  (testing (lambda (x y z) (send x choose y z)))
  
  (define (player-with-cards c)
    (define ex (create-external))
    (set-field! cards ex c)
    (set-field! boards ex (list (species #:population 1 #:traits `())))
    ex)
  
  (define before (list '() '()))
  (define after (list '()))
  
  (run-testing
   (player-with-cards (take all-cards 4)) before after
   '(0 [(1 3)] [] [(1 2)] [])
   "minimal # of cards, yields one extra board and one population growth")
  
  (run-testing
   (player-with-cards (take all-cards 5)) before after
   '(0 [(1 3)] [(1 4)] [(1 2)] [])
   "add 1 to min # of cards, yields one extra board, one population/body growth")
  
  (run-testing
   (player-with-cards (take all-cards 6)) before after
   '(0 [(1 3)] [(1 4)] [(1 2)] [(1 0 5)])
   "add 2 to min # of cards, yields: extra board, 1 population, 1 body growth, 1 rt")
  
  (run-testing
   (player-with-cards (take all-cards 7)) before after
   '(0 [(1 3)] [(1 4)] [(1 2)] [(1 0 5)])
   "add 3 to min # of cards, yields: extra board, 1 population, 1 body growth, 1 rt")
  )
