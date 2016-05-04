#lang racket

;; ===================================================================================================
;; represent a base player and an internal from the perspective of the dealer

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
      [create-species species]
      [create-feed-none feed-none]
      [create-feed-vegetarian feed-vegetarian]
      [create-store-fat-on-tissue store-fat-on-tissue]
      [create-feed-carnivore feed-carnivore])))
