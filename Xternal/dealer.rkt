#lang racket

;; ===================================================================================================
;; this class extends the dealer from ../ with hooks for observations and JSON serialization, 
;; both for playing Evolution with a GUI 

(require "json.rkt"
         "observer.rkt"
         (only-in "../dealer.rkt" dealer/c create-dealer/c MAX-PLAYERS MIN-PLAYERS))

;; the interface of the dealer for external uses (xmain, xserver, etc) 
(define dealer/observer/c (and/c dealer/c json/c with-observer/c))

(provide
 ;; re-exports
 MAX-PLAYERS
 MIN-PLAYERS
 
 (contract-out
  (create-dealer (create-dealer/c dealer/observer/c))))

;; ===================================================================================================
(require "player-internal.rkt"  "cards.rkt" (only-in "../dealer.rkt" dealer%))

(define (create-dealer players (cards all-cards))
  (new xdealer% [externals players][cards cards]))

(define xdealer%
  (class dealer%
    (super-new [internal-player create-player])
    
    (inherit-field players watering-hole cards)
    
    ;; -----------------------------------------------------------------------------------------------
    ;; serialization
    
    (define/public (to-json)
      `(,(map (lambda (p) (send p to-json)) players) ,watering-hole ,(map card->json cards)))
    
    ;; -----------------------------------------------------------------------------------------------
    (define *observers '[])
    
    (define/public (register-observer o)
      (set! *observers (cons o *observers)))
    
    (define/private (call-observers)
      (for ((o *observers))
        (define j (to-json))
        (send o display j)
        (sleep 10)))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/augment-final (complete-turn)
      (displayln `(calling observers))
      (call-observers))))






