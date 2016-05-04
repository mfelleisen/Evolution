#lang racket

(module+ test 
  (require (submod "proxy-dealer.rkt" json-from)
           (submod "proxy-player.rkt" json-to)
           "board.rkt"
           "../traits.rkt"
           "cards.rkt"
           rackunit)
  
  (define start1 `[12 1 () ,(take all-cards 4)])
  (check-equal? (json->start (apply start->json start1)) start1)
  
  (define s (species #:traits `(,ambush)))
  (define choose1 `[((,s)) ((,s))])
  (check-equal? (json->choose (apply choose->json choose1)) choose1)
  
  (define choose2
    '(()
      (((("food" 0) ("body" 0) ("population" 1) ("traits" ())))
       ((("food" 0) ("body" 0) ("population" 1) ("traits" ())))
       ((("food" 0) ("body" 0) ("population" 1) ("traits" ()))))))
  (check-equal? (apply choose->json (json->choose choose2)) choose2))


