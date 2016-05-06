#lang racket

;; ===================================================================================================
;; overrides player-internal's create-species from ../ with a serializable version

;; EXTERNAL SERVICES

(require  (only-in "../player-internal.rkt" internal-communication/c))

(provide
 [contract-out
  [create-player (-> any/c #;==name? any/c #;==external? internal-communication/c)]])

;; ===================================================================================================
;; DEPENDENCIES
(require "board.rkt" (only-in "../player-internal.rkt" player%) "../basics.rkt")

;; ===================================================================================================
;; IMPLEMENTATION

(define (create-player name external)
  (define p (new player/j% [id name]))
  (set-fields! p external))

(define player/j%
  (class player%
    (super-new
     [create-species species])))
