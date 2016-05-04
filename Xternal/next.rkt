#lang racket

;; ===================================================================================================
;; the ontology between dealer and external player for
;; -- start,
;; -- choose, and
;; -- feed-next
;; interactions

;; EXTERNAL SERVICES

(require "../basics.rkt")

(define next+json/c (and/c next/c (object/c [to-json any/c])))

(provide
  (all-from-out "../next.rkt")
 (contract-out
  [feed-none           (-> next+json/c)]
  [feed-vegetarian     (-> natural? next+json/c)]
  [store-fat-on-tissue (-> natural? natural+? next+json/c)]
  [feed-carnivore      (-> natural? natural? natural? next+json/c)]))

;; ===================================================================================================
;; DEPENDENCIES

(require (except-in "../next.rkt" feed-none feed-vegetarian store-fat-on-tissue feed-carnivore)
         (except-in "../basics.rkt" natural? natural+?))

;; for debugging
(require  "common.rkt")

(module+ test
  (require rackunit))

;; ===================================================================================================
;; IMPLEMENTATION

(define (feed-none)
  (new feed-none/j%))
(define (feed-vegetarian s)
  (new feed-vegetarian/j% [s s]))
(define (store-fat-on-tissue s n)
  (new store-fat-on-tissue/j% [s s][n n]))
(define (feed-carnivore attacker p0 attackee)
  (new feed-carnivore/j% [attacker attacker][p0  p0] [attackee attackee]))

;; ---------------------------------------------------------------------------------------------------
(define feed-none/j%
  (class feed-none%
    (super-new)
    
    (define/public (to-json) #false)))

;; ---------------------------------------------------------------------------------------------------
(define feed-vegetarian/j%
  (class feed-vegetarian%
    (super-new)
    (inherit-field s)
    
    (define/public (to-json) s)))

;; ---------------------------------------------------------------------------------------------------
(define store-fat-on-tissue/j%
  (class store-fat-on-tissue%
    (super-new)
    (inherit-field s n)
    
    (define/public (to-json) (list s n))))

;; ---------------------------------------------------------------------------------------------------
(define feed-carnivore/j%
  (class feed-carnivore%
    (super-new)
    (inherit-field attacker p0 attackee)
    
    (define/public (to-json) (list attacker p0 attackee))))
