#lang racket

;; ===================================================================================================
;; equips concrete feed-* classes from ../ with serialization for dist. impl. 

;; EXTERNAL SERVICES

(require "json.rkt" "../basics.rkt" (only-in "../next.rkt" next/c))

(define next/j/c (and/c next/c json/c))

(provide
 (all-from-out "../next.rkt")
 (contract-out
  [feed-none           (-> next/j/c)]
  [feed-vegetarian     (-> natural? next/j/c)]
  [store-fat-on-tissue (-> natural? natural+? next/j/c)]
  [feed-carnivore      (-> natural? natural? natural? next/j/c)]))

#| ---------------------------------------------------------------------------------------------------
   uniform extension of each superclass
                                                     ...
                                                      |
           +------------------------+------------------------------+-------------------+
           |                        |                              |                   |
+----------------------+   +---------------------+   +---------------------+  +---------------------+
| store-fat-on-tissue% |   |     feed-none%      |   |   feed-vegetarian%  |  |   feed-carnivore%   |
+----------------------+   +---------------------+   +---------------------+  +---------------------+
           |                        |                              |                   |
+----------------------+   +---------------------+   +---------------------+  +---------------------+
|store-fat-on-tissue/j%|   |     feed-none/j%    |   | feed-vegetarian/j%  |  |   feed-carnivore/j% |
+----------------------+   +---------------------+   +---------------------+  +---------------------+

--------------------------------------------------------------------------------------------------  |#

;; ===================================================================================================
;; DEPENDENCIES

(require (except-in "../next.rkt" next/c feed-none feed-vegetarian store-fat-on-tissue feed-carnivore)
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
;; SYNTAX (define/json/c cls% (fields ...) json)
;; creates superclass with fields ... and to-json method that delivers json
(define-syntax (define/json/c stx)
  (syntax-case stx ()
    [(_ cls% (fields ...) json)
     #`(define #,(add-/j #'cls%)
         (class cls%
           (super-new)
           (inherit-field fields ...)
           (define/public (to-json) json)))]))

(define-for-syntax (add-/j syntax-id)
  (define st (symbol->string (syntax-e syntax-id)))
  (format-id syntax-id "~a/j%" (substring st 0 (- (string-length st) 1))))

(define/json/c feed-none% () #false)
(define/json/c feed-vegetarian% (s) s)
(define/json/c store-fat-on-tissue% (s n) (list s n))
(define/json/c feed-carnivore% (attacker p0 attackee) (list attacker p0 attackee))

