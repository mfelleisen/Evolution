#lang racket

;; ===================================================================================================
;; this class extends the dealer from ../ for the distributed version

;; the interface of the dealer for external uses (xmain, xserver, etc) 
(define dealer/observer/c (and/c dealer/c (object/c [register-observer (->m observer/c any)])))

(provide
 ;; re-exports
 MAX-PLAYERS
 MIN-PLAYERS
 
 (contract-out
  (create-dealer (create-dealer/c dealer/observer/c))))

;; ===================================================================================================
(require "../dealer.rkt" "player-internal.rkt" "observer.rkt" "cards.rkt")

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
      
    
    
    
    
    
