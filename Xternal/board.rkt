#lang racket

;; ===================================================================================================
;; equip species% and creators from ../ with serialization for GUI and dist. impl.

;; EXTERNAL SERVICES

(require "json.rkt")

(define species/json/c (and/c species/c json/c))

(provide
  (except-out (all-from-out "../board.rkt") create-species)

 species

 ;; -> Species 
 (rename-out [create-species/json create-species])

 ;; JSexpr -> Species
 json->species)

;; ===================================================================================================
;; DEPENDENCIES

(require "../board.rkt" (except-in "../traits.rkt" trait?) "../basics.rkt")

;; for debugging
(require  "common.rkt")

(module+ test
  (require rackunit (submod "..") (submod "common.rkt" test)))

;; ===================================================================================================
;; IMPLEMENTATION

;; ---------------------------------------------------------------------------------------------------

(define (create-species/json)
  (new species/json%))

(define (species #:body (body 0)
                 #:fat-food (fat-food 0)
                 #:food (food 0)
                 #:population (population 1)
                 #:traits (traits '()))
  (define-syntax-rule (set food) (set-field! food s food))
  (define s (new species/json%))
  (set food)
  (set body)
  (set population)
  (set traits)
  (set fat-food)
  s)

(define (json->species j)
  (match j
    [`(("food" ,(? nat? f)) ("body" ,(? nat? b)) ("population" ,(? nat? p)) ("traits" ,(? lot? t)) . ,stuff)
     (define ff 
       (match stuff
	 [`(("fat-food" ,(? nat? ff))) ff]
	 ['() 0]))
     (define s (create-species/json))
     (set-field! body s b)
     (set-field! food s f)
     (set-field! population s p)
     (set-field! traits s (map string->trait t))
     (set-field! fat-food s ff)
     s]))

(define (nat? x)
  (and (natural? x) (<= 0 x 7)))

;; Any -> Boolean 
(define (lot? l)
  (and (list? l) (<= (length l) SPECIES-TRAITS)))

(define SPECIES-TRAITS 3)

;; ---------------------------------------------------------------------------------------------------
;; species% : Species

(define species/json%
  (class species%
    (super-new)
    (inherit has)
    (inherit-field food fat-food body population traits)

    ;; -----------------------------------------------------------------------------
    (define/public (to-json)
      `(("food" ,food)
        ("body" ,body)
        ("population" ,population)
        ("traits" ,(map trait->string traits))
        ,@(if (and (has fat-tissue?) (> fat-food 0)) `(("fat-food" ,fat-food)) '())))))

;; ===================================================================================================
(module+ test
  ;; -------------------------------------------------------------------------------------------------
  (define (attacker1 2traits)
    (define s (create-species/json))
    (set-field! body s 3)
    (set-field! food s 2)
    (set-field! population s 4)
    (set-field! traits s `(,carnivore ,@2traits))
    s)
  
  ;; -------------------------------------------------------------------------------------------------
  (check-equal? (json->species (send (attacker1 '()) to-json)) (attacker1 '())
                "json->species is left-inverse for to-json")
  
  (define s0
    (let* ([s (create-species/json)])
      (set-field! fat-food s 2)
      (set-field! traits s `(,fat-tissue))
      s))

  (check-equal? (json->species (send s0 to-json)) s0
                "json->species is left-inverse for to-json, with fat food"))
