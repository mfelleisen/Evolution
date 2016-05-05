#lang racket

;; ===================================================================================================
;; overrides player-external's create-feed-* from ../ with a serializable versions of constructors 

;; EXTERNAL SERVICES

(provide
  (all-from-out "../player-external.rkt")
    
 (contract-out
  [create-external (-> (external-player/c pre-choose))])
 
 ;; [Natural Boards Cards Natural Players -> Any] -> ExternalPlayer
 ;; creating players that act badly during feed-next
 create-bad-feed
 
 ;; [Cards Boards -> Any] -> ExternalPlayer
 ;; creating players that act badly durinh choose
 create-bad-choose)

;; ===================================================================================================
;; DEPENDENCIES
(require "../player-external.rkt" "next.rkt")

;; for debugging:
(require "common.rkt")

;; ===================================================================================================
;; IMPLEMENTATION

(define (create-external)
  (new external/j%))

(define (create-bad-feed bad-feed)
  (new external/j% [bad-feed-next bad-feed]))

(define (create-bad-choose bad-choose)
  (new external/j% [bad-choose bad-choose]))

;; ---------------------------------------------------------------------------------------------------
;; an external player with a specific strategy
(define external/j%
  (class external%
    (super-new
      [create-feed-none feed-none]
      [create-feed-vegetarian feed-vegetarian]
      [create-store-fat-on-tissue store-fat-on-tissue]
      [create-feed-carnivore feed-carnivore])))
