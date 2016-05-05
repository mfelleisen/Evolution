#lang racket

;; ===================================================================================================
;; overrides player-internal's create-species from ../ with a serializable version

;; EXTERNAL SERVICES

(require "next.rkt")

(provide
  create-player
  #;
 [contract-out
  [create-player (-> any/c #;==name? any/c #;==external? internal-communication/c)]])

;; ===================================================================================================
;; DEPENDENCIES
(require "../player-internal.rkt" "board.rkt")

;; ===================================================================================================
;; IMPLEMENTATION

(define (create-player name external)
  (define p (new player/j% [id name]))
  (set-field! external p external)
  p)

(define player/j%
  (class player%
    (super-new
      [create-species species])))
