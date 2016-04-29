#lang racket

;; ===================================================================================================
;; a proxy player 

(require (only-in "player-external.rkt" external-player/c)
         (only-in "basics.rkt" natural? natural+?))

(provide
 (contract-out
  [create-proxy-player (-> input-port? output-port? (external-player/c (lambda (ex) #true)))]))

(module+ json-to ;; for external testing 
  (provide
   start->json
   choose->json
   state->json))

;; ===================================================================================================
(require "next.rkt" "internal-external.rkt"
         (except-in "cards.rkt" card?)
         "basics.rkt"
         "../common.rkt" json)

(module+ test
  (require "board.rkt" "traits.rkt" (submod "..") (submod "../common.rkt" test) rackunit))

;; ===================================================================================================

(define (create-proxy-player in out)
  (new proxy-player% [in in][out out]))

;; Q: can one generate proxies from interface plus translation functions? 

(define proxy-player%
  (class object%
    (init-field in out)
    (super-new)
    
    ;; no return expected    
    (define/public (start wh0 bag0 boards0 cards0)
      (parameterize ([current-input-port in] [current-output-port out])
        (send-message (start->json wh0 bag0 boards0 cards0))
        (void)))
    
    ;; EFFECT if the json is incorrect, throw exception 
    (define/public (choose pre-boards -postboards)
      (parameterize ([current-input-port in] [current-output-port out])
        (send-message (choose->json pre-boards -postboards))
        (receive-external json->action4)))
    
    ;; EFFECT if the json is incorrect, throw exception 
    (define/public (feed-next bag0 boards0 cards0 watering-hole other-boards)
      (parameterize ([current-input-port in] [current-output-port out])
        (send-message (state->json bag0 boards0 cards0 watering-hole other-boards))
        (receive-external json->next)))))

;; Natural [Listof Species] [Listof Card] -> JSexpr 
(define (start->json wh bag boards cards)
  `[,wh ,bag ,(for/list ((b boards)) (send b to-json)) ,(map card->json cards)])

;; [Listof [Listof Species]] [Listof [Listof Species]] -> JSexpr 
(define (choose->json pre post)
  `[,(boards*->json pre) ,(boards*->json post)])

;; [Listof [Listof Species]] -> JSexpr
(define (boards*->json los)
  (map (lambda (other) (map (lambda (s) (send s to-json)) other)) los))

;; Natural [Listof Species] [Listof Card] Natural [Listof Species] -> JSexpr 
(define (state->json bag boards cards food other-boards)
  `[,bag
    ,(map (lambda (s) (send s to-json)) boards)
    ,(map card->json cards)
    ,food
    ,(boards*->json other-boards)])

;; JSexpr -> Action4
(define (json->action4 j)
  (match j
    [`[,(? natural? fc) [,gp ...] [,gb ...] [,bt ...] [,rt ...]]
     `(,fc ,(map json->g gp) ,(map json->g gb) ,(map json->bt bt) ,(map json->rt rt))]))

;; JSexpr -> Growth 
(define [json->g  g]
  (match g
    [`(,(? natural? i) ,(? natural? j)) `(,i ,j)]))

;; JSexpr -> BT
;; board addition: [pay-with, optional-trait, ..., optional-trait] (between 0 and 3 optional-traits)
(define (json->bt j)
  (match j
    [`[,(? natural? pay)] `(,pay)]
    [`[,(? natural? pay) ,(? natural? t1)] `(,pay ,t1)]
    [`[,(? natural? pay) ,(? natural? t1) ,(? natural? t2)] `(,pay ,t1 ,t2)]
    [`[,(? natural? pay) ,(? natural? t1) ,(? natural? t2) ,(? natural? t3)] `(,pay ,t1 ,t2 ,t3)]))

;; JSexpr -> RT
;; trait replacement: [board-index, species-position, new-trait]
(define (json->rt j)
  (match j
    [`[,(? natural? b) ,(? natural? i) ,(? natural? j)] `(,b ,i ,j)]))

;; JSexpr -> Next 
(define (json->next nxt)
  (match nxt
    [#f (feed-none)]
    [(? natural? s) (feed-vegetarian s)]
    [`(,(? natural? s) ,(? natural+? n)) (store-fat-on-tissue s n)]
    [`(,(? natural? c) ,(? natural? p) ,(? natural? s)) (feed-carnivore c p s)]))

;; ===================================================================================================
(module+ test
  
  ;; I want to test two things: (1) that the proxy writes the proper JSON string to OUT
  ;; and (2) that the return value is the received message
  
  ;; It's really important to test the json converters extensively. 
  
  (define-syntax-rule
    (check-method m:id args:id args-json response)
    (let* ([*out (gensym)]
           [output 
            (with-output-to-string
             (lambda ()
               (define j (if (void? response) "" (to-json* response)))
               (set! *out 
                     (with-input-from-string
                      (make-test j)
                      (lambda ()
                        [define p (create-proxy-player (current-input-port) (current-output-port))]
                        (send p m:id . args:id))))))])
      (check-equal? (with-input-from-string output read-json) args-json "output test")
      (unless (void? response)
        (check-equal? *out response "result test"))))
  
  ;; -------------------------------------------------------------------------------------------------
  (define s (species #:traits `(,ambush)))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; testing start, no response expected, which is what (void) indicates 
  (define start1 `[12 1 () ,(take all-cards 4)])
  (check-method start start1 (apply start->json start1) (void))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; testing choose, tests json->action4
  (define choose1 `[((,s)) ((,s))])
  
  (check-method choose choose1 (apply choose->json choose1) '[0 [[0 3]] [] [[1 2]] []])
  (check-method choose choose1 (apply choose->json choose1) '[0 [[0 3]] [[0 4]] [[1 2]] []])
  (check-method choose choose1 (apply choose->json choose1) '[0 [[0 3]] [[0 4]] [[1 2]] [[0 0 5]]])
  (check-method choose choose1 (apply choose->json choose1)
                '[0 [[0 3]] [[0 4]] [[1 2][6 7 8 9]] [[0 0 5]]])
  (check-method choose choose1 (apply choose->json choose1)
                '[0 [[0 3]] [[0 4]] [[1 2][6 7 8 9]] [[0 0 5] [0 0 9]]])
  
  ;; -------------------------------------------------------------------------------------------------
  ;; testing feed, tests
  (define next1 `[99 (,s ,s ,s) ,(take all-cards 4) 9 ((,s) (,s ,s))])
  
  (check-method feed-next next1 (apply state->json next1) (feed-none))
  (check-method feed-next next1 (apply state->json next1) (feed-vegetarian 1))
  (check-method feed-next next1 (apply state->json next1) (feed-carnivore 1 2 3))
  (check-method feed-next next1 (apply state->json next1) (store-fat-on-tissue 1 2))
  
  )
