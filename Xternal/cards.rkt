#lang racket

;; ===================================================================================================
;; representing individual cards and the set of all cards as specified in the rules of Evolution

;; EXTERNAL SERVICES

(require "../cards.rkt")

(provide
  (all-from-out "../cards.rkt")
 
 ;; JSexpr -> Card
 json->card
 
 ;; Card -> JSexpr
 card->json)

;; ===================================================================================================
;; DEPENDENCIES

(require "../traits.rkt")

(module+ test
  (require rackunit))

;; ===================================================================================================
;; IMPLEMENTATION

(define (card->json c)
  `[,(card-food-points c) ,(trait->string (card-trait c))])

(define (json->card j)
  (match j
    [`(,(? integer? food) ,trait)
     (define candidate (card food (string->trait trait)))
     (cond
       [(member candidate all-cards) => first]
       [else (error 'json->card "~e does not specify an Evolution card")])]))

;; ===================================================================================================
(module+ test
  (define a-card (list-ref all-cards (random (length all-cards))))
  (check-equal? (json->card (card->json a-card)) a-card "json->card is left inverse to card->json"))
