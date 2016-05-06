#lang racket

;; ===================================================================================================
;; equip species% and creators from ../ with serialization for GUI and dist. impl.

;; SERVICES

(require "json.rkt"
         (only-in "../board.rkt" species/c body/c fat-food/c food/c population/c traits/c)
         (only-in "../traits.rkt" trait?))

(define species/json/c (and/c species/c json/c))

(provide
 (all-from-out "../board.rkt")
 
 (contract-out
  [species
   (->i ()
        (#:body       [b      body/c]
         #:fat-food   [ff (b) (fat-food/c b)]
         #:food       [f  (p) (food/c p)]
         #:population [p      population/c]
         #:traits     [t      (traits/c trait?)])
        [r species/json/c])])
 
 ;; -> Species 
 (rename-out [create-species/json create-species])
 
 ;; JSexpr -> Species 
 json->species)

;; ===================================================================================================
;; DEPENDENCIES

(require (only-in "../board.rkt" MAX-POPULATION species%)
         (except-in "../traits.rkt" trait?)
         "../basics.rkt")

;; for debugging
(require  "common.rkt")

(module+ test
  (require rackunit (submod "..") (submod "common.rkt" test)))

;; ===================================================================================================
;; IMPLEMENTATION

(define (create-species/json)
  (new species/json%))

(define (species #:body       (body 0)
                 #:fat-food   (fat-food 0)
                 #:food       (food 0)
                 #:population (population 1)
                 #:traits     (traits '()))
  (define s (new species/json%))
  (set-fields! s food body population traits fat-food))

(define (json->species j)
  (match j
    [`(("food" ,(? (food/c MAX-POPULATION) f))
       ("body" ,(? body/c b))
       ("population" ,(? population/c p))
       ("traits" ,(? (traits/c string->trait) t))
       . ,stuff)
     (when (> f p)
       ;; a trick to raise a halfway appropriate exception when the food is larger than the population
       (match `("food" ,f) ['("population" 1) #true]))
     (define ff 
       (match stuff
         [`(("fat-food" ,(? (fat-food/c b) ff))) ff]
         ['() 0]))
     (species #:body b #:fat-food ff #:food f #:population p #:traits (map string->trait t))]))

(define species/json%
  (class species%
    (super-new)
    (inherit has)
    (inherit-field food fat-food body population traits)
    
    (define/public (to-json)
      `(("food" ,food)
        ("body" ,body)
        ("population" ,population)
        ("traits" ,(map trait->string traits))
        ,@(if (and (has fat-tissue?) (> fat-food 0)) `(("fat-food" ,fat-food)) '())))))

;; ===================================================================================================
(module+ test
  
  (define (attacker1 2traits)
    (define s (create-species/json))
    (set-field! body s 3)
    (set-field! food s 2)
    (set-field! population s 4)
    (set-field! traits s `(,carnivore ,@2traits))
    s)
  
  (check-equal? (json->species (send (attacker1 '()) to-json)) (attacker1 '())
                "json->species is left-inverse for to-json")
  
  (define s0
    (let* ([s (create-species/json)])
      (set-field! body s 2)
      (set-field! fat-food s 2)
      (set-field! traits s `(,fat-tissue))
      s))
  
  (check-equal? (json->species (send s0 to-json)) s0
                "json->species is left-inverse for to-json, with fat food"))
