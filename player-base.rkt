#lang racket

;; ===================================================================================================
;; basic functionality for both internal and external players (to keep up-to-date, if they wish)

(require "board.rkt" (only-in "cards.rkt" card?) (only-in "basics.rkt" natural? natural+?))

(define species-index-list/c [listof natural?])
(define species-list/c [listof species/c])
(define species-attack/c [list/c species/c natural? natural? natural?])

(define base-player/c
  (class/c
   (field [boards species-list/c]
          [bag natural?]
          [cards [listof card?]])
   [separate-hungries (->m (values species-index-list/c species-index-list/c))]
   [with-fat-tissue   (->m [listof [list/c natural? natural+?]])]
   [all-attackables   (->m natural? species-list/c [listof species-attack/c])]
   [can-attack        (->m natural? species-list/c (or/c #false [list/c natural? natural? natural?]))]
   [can-attack+       (->m natural? natural? species-list/c [listof species-attack/c])]))

(provide
 (contract-out
  [base-player%  base-player/c]))

;; ===================================================================================================
(require "traits.rkt" (except-in "cards.rkt" card?) (except-in "basics.rkt" natural? natural+?)
         2htdp/image)

;; for debugging
(require  "common.rkt")

;; ===================================================================================================
;; the base class for players: every player must have these fields and methods 
(define base-player%
  (class* object% (equal<%>)
    (super-new)
    
    (field
     [boards
      ;; [Listof Species]
      ;; the species that the player currently owns 
      '()]
     [bag
      ;; the food bag counts the number of points accumulated from prior turns 
      0]
     [cards
      ;; the cards that the player owns
      '()])
    
    ;; -----------------------------------------------------------------------------------------------
    ;; equality
    
    (define/public (equal-to? other r)
      (and (r (get-field boards other) boards)
           (= (get-field bag other) bag)
           (r (get-field cards other) cards)))
    
    ;; this is basically nonsense 
    (define/public (equal-hash-code-of hash-code)
      (hash-code boards))
    
    ;; this is basically nonsense 
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code cards))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; serialization
    
    (define/public (to-json)
      `(("species" ,(for/list ((s boards)) (send s to-json)))
        ("bag" ,bag)
        ,@(if (empty? cards)
              `()
              `(("cards" ,(map card->json cards))))))

    ;; -----------------------------------------------------------------------------------------------
    ;; methods for communicating between external player and intenal representation
    
    (abstract start feed-next choose)
    
    ;; -----------------------------------------------------------------------------------------------
    ;; methods for managing the bag, the species boards, and the cards 
    
    (define/public (separate-hungries)
      (define-values (vegetarians carnivores)
        (for/fold ([veg '()][car '()]) ([s boards][i (in-naturals)]#:when (not (send s all-fed?)))
          (if (send s has carnivore?) (values veg (cons i car)) (values (cons i veg) car))))
      (values (reverse vegetarians) (reverse carnivores)))
    
    ;; -> [Listof [List N N]]
    (define/public (with-fat-tissue)
      (for/list ([s boards]
                 [i (in-naturals)]
                 #:when (and (send s has fat-tissue?) (> (send s fat-food-needed) 0)))
        (list i (send s fat-food-needed))))
    
    ;; N [Listof Species] -> [Maybe [List N N N]]
    ;; can the given carnviore board attack any of the other players' board? 
    (define/public (can-attack carnivore-species others (>-species (lambda (x y) #t)))
      (define attackables (all-attackables carnivore-species others))
      (rest/c (first/c (sort attackables >-species #:key first))))
    
    ;; N [Listof Species] -> [Listof [List board N N N]]
    (define/public (all-attackables carnivore-species others)
      (for/fold ((attackables '())) ([other-species others][index-for-other (in-naturals)])
        (define a (can-attack+ carnivore-species index-for-other other-species))
        (append attackables a)))
    
    ;; N N [Listof Board] -> [Listof [List board N N N]]
    (define/public (can-attack+ carnivore-index other-player other-player-s-species)
      (define v (list->vector other-player-s-species))
      (for/fold ((attacks '())) ([attackee-index (in-range (vector-length v))])
        (define left (vector-ref/c v (- attackee-index 1)))
        (define attackee (vector-ref v attackee-index))
        (define right (vector-ref/c v (+ attackee-index 1)))
        (define attackable? (send attackee attackable? (list-ref boards carnivore-index) left right))
        (cond
          [(not attackable?) attacks]
          [else
           (define an-attack (list attackee carnivore-index other-player attackee-index))
           (cons an-attack attacks)])))))
