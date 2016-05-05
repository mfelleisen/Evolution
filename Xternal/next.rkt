#lang racket

;; ===================================================================================================
;; equips concrete feed-* classes from ../ with serialization for dist. impl. 

;; EXTERNAL SERVICES

(require "json.rkt" "../basics.rkt")

(define next/j/c (and/c next/c json/c))

(provide
  (all-from-out "../next.rkt")
 (contract-out
  [feed-none           (-> next/j/c)]
  [feed-vegetarian     (-> natural? next/j/c)]
  [store-fat-on-tissue (-> natural? natural+? next/j/c)]
  [feed-carnivore      (-> natural? natural? natural? next/j/c)]))

;; ===================================================================================================
;; DEPENDENCIES

(require (except-in "../next.rkt" feed-none feed-vegetarian store-fat-on-tissue feed-carnivore)
         (except-in "../basics.rkt" natural? natural+?)
	 (for-syntax racket/syntax))

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

(define feed-vegetarian/j%
  (class feed-vegetarian%
    (super-new)
    (inherit-field s)
    (define/public (to-json) s)))

(define store-fat-on-tissue/j%
  (class store-fat-on-tissue%
    (super-new)
    (inherit-field s n)
    (define/public (to-json) (list s n))))

(define feed-carnivore/j%
  (class feed-carnivore%
    (super-new)
    (inherit-field attacker p0 attackee)
    (define/public (to-json) (list attacker p0 attackee))))

;; -----------------------------------------------------------------------------
;; abstract with syntax 
(define-syntax (define/json/c stx)
  (syntax-case stx ()
    [(_ feed-carnivore% (fields ...) json)
     (let ((feed-carnivore/j% (add-/j #'feed-carnviore%)))
       #`(define #,feed-carnivore/j%
	   (class feed-carnivore%
	     (super-new)
	     (inherit-field attacker p0 attackee)
	     (define/public (to-json) (list attacker p0 attackee)))))]))

(define-for-syntax (add-/j syntax-id)
  (define sy (syntax-e syntax-id))
  (define st (string->symbol sy))
  (define -% (substring st 0 (- (string-length st) 2)))
  (define /j (format-id "~a/j%" -%))
  (datum->syntax syntax-id /j))
