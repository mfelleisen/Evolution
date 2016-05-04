#lang racket

;; ===================================================================================================
;; representing individual cards and the set of all cards as specified in the rules of Evolution

;; EXTERNAL SERVICES

(provide
 ;; type Card = (card FoodPoints Trait)
 (struct-out card)
 
 ;; Card -> Boolean 
 valid-card? 
 
 ;; Card Card -> Boolean
 ;; lexicographic comparison, traits first according to alphabetical
 ;; spelling, followed by food points on the card 
 <-card
 
 ;; [Listof Card]
 ;; (sorted by <-card)
 all-cards
 
 ;; [Setof Card] -> Boolean 
 subset-of-all-cards?)

;; ===================================================================================================
;; DEPENDENCIES

(require "traits.rkt")

(module+ test
  (require rackunit))

;; ===================================================================================================
;; IMPLEMENTATION

(struct card (food-points trait) #:transparent)

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (check-true (<-card (card -1 climbing) (card 2 herding)))
  (check-true (<-card (card -1 climbing) (card 2 climbing)))
  (check-true (<-card (card -1 climbing) (card 0 climbing)))
  (check-false (<-card (card -1 climbing) (card -1 climbing)))
  (check-false (<-card (card 2 herding) (card -1 climbing))))

(define (<-card c1 c2)
  (match-define (card f1 t1) c1)
  (match-define (card f2 t2) c2)
  (or (<-trait t1 t2)
      (and (equal? t1 t2) (< f1 f2))))

;; ---------------------------------------------------------------------------------------------------
(define MIN-FOOD-CARNIVORE -8)
(define MAX-FOOD-CARNIVORE +8)

(define MIN-FOOD -3)
(define MAX-FOOD +3)

(define L
  (+ (- MAX-FOOD-CARNIVORE MIN-FOOD-CARNIVORE -1)
     (* (length all-but-carnivore) (- MAX-FOOD MIN-FOOD -1))))

(module+ test
  (check-equal? (length all-cards) L))

(define all-cards
  (sort 
   (append
    (for/list ((i (range MIN-FOOD-CARNIVORE (+ MAX-FOOD-CARNIVORE 1)))) (card i carnivore))
    (for*/list ((trait all-but-carnivore) (i (range MIN-FOOD (+ MAX-FOOD 1)))) (card i trait)))
   <-card))

(define all-cards-set (apply set all-cards))
(define (subset-of-all-cards? s)
  (subset? s all-cards-set))

(define (valid-card? c)
  (cons? (member c all-cards)))
