#lang racket

;; ===================================================================================================
;; a proxy dealer

;; EXTERNAL SERVICES

(require "../next.rkt")

(define dealer-external/c
  (object/c
   [run-game (->m any/c)]))

(provide
 (contract-out
  ;; the optional strings are sign-up strings 
  [create-proxy-dealer
   (->* (input-port? output-port? external-player/c) (string? #;jsexpr) dealer-external/c)]
  (create-proxy-dealer-infsu
   ;; go into infinite loop during sign up
   (->* (input-port? output-port? external-player/c) (string? #;any-string) dealer-external/c))
  (create-proxy-dealer-bad-sign-up
   ;; send bad kind of strings for sign-up
   (->* (input-port? output-port? external-player/c) (string? #;any-string) dealer-external/c))))

;; ===================================================================================================
;; DEPENDENCIES

(require "messaging.rkt" "../player-external.rkt" "../board.rkt" "../cards.rkt"  "../basics.rkt" json)

(require "../common.rkt")

(module+ test
  (require (submod "..") "../traits.rkt" (submod "../common.rkt" test) json rackunit))

;; ===================================================================================================
;; IMPLEMENTATION

(define SIGN-UP `"my name is matthias")

(define (create-proxy-dealer in out player [sign-up SIGN-UP])
  (new proxy-dealer% [in in][out out][player player][sign-up sign-up]))

(define (create-proxy-dealer-bad-sign-up in out player (sign-up "must pass"))
  (new proxy-dealer-bad-sign-up% [in in][out out][player player][sign-up sign-up]))

(define (create-proxy-dealer-infsu in out player (sign-up "must pass"))
  (new proxy-dealer-infsu% [in in][out out][player player][sign-up sign-up]))

;; ===================================================================================================
(define proxy-dealer-infsu%
  (class object%
    (init-field in out player sign-up)
    (super-new)
    (define/public (run-game)
      (run-game))))

(define proxy-dealer-bad-sign-up%
  (class object%
    (init-field in out player sign-up)
    (super-new)
    (define/public (run-game)
      (parameterize ([current-output-port out])
        (display sign-up)
        (flush-output)))))

(define proxy-dealer%
  (class object%
    (init-field player in out sign-up)
    (super-new)
    
    (define-syntax-rule
      (define/fsm start (_ x y ...) ...)
      (letrec ((x (state: x y ...)) ...) (start)))
    
    (define-syntax (state: stx)
      (syntax-case stx (--> ||)
        [(_ from json->in out->json --> to)
         #'(state: from json->in out->json to (err 'from))]
        [(_ from json->in out->json --> to || alt)
         #'(state: from json->in out->json to alt)]
        [(_ from json->in out->json to alt)
         #'[lambda ([input* #f])
             (define input (or input* (read-json))) ;; <-- do not impose time-out with read-message
             (log-info "raw input ~a" input)
             (cond
               [(eof-object? input)
                (close-input-port in)
                (close-output-port out)]
               [else
                (with-handlers ((exn:misc:match? (lambda (xn) (alt input))))
                  (define parsed (json->in input))
                  (out->json (send player from . parsed))
                  (to))])]]))
    
    (define/private ((err from) input)
      (error 'proxy-dealer "bad message in state: ~a: ~e" from input))
    
    (define/public (run-game)
      (parameterize ([current-input-port in] [current-output-port out])
        (send-message sign-up)
        (define ok (read-json in)) ;; <-- do not impose time-out with read-message
        (define/fsm start
          (state: start     json->start  void                          --> choose)
          (state: choose    json->choose (compose send-message values) --> feed-next)
          (state: feed-next json->state  next->json                    --> feed-next || start))))))

;; Next -> JSexpr
(define (next->json n)
  (send-message (send n to-json)))

(module+ json-from
  (provide
   json->start
   json->choose
   json->state))

;; JSexpr -> [List N [Listof Board] [Listof Card]]
(define (json->start x)
  (match x
    [`(,(? natural? watering-hole) ,(? natural? bag) ,(? list? boards) ,(? list? cards))
     (list watering-hole bag (map json->species boards) (map json->card cards))]))

;; JSexpr -> [List [Listof Board] [Listof Board]]
(define (json->choose j)
  (match j
    [`(,(? list? before) ,(? list? after))
     `(,(json->boards before)
       ,(json->boards after))]))

;; JSexpr -> [Listof [Listof Board]]
(define (json->boards before)
  (map (lambda (b) (map json->species b)) before))

;; JSexpr -> [Listf N [Listof Board] [Listof Card] N [Listof Board]]
(define (json->state j)
  (match j
    [`(,(? natural? bag) ,(? list? boards) ,(? list? cards) ,(? natural? food) ,(? list? others))
     (list bag
           (map json->species boards)
           (map json->card cards)
           food
           (json->boards others))]))

;; ===================================================================================================
(module+ test
  
  ;; -------------------------------------------------------------------------------------------------
  (define (nth f n) (lambda () (let nth ([n n]) (if (= n 0) '() (cons (f) (nth (- n 1)))))))
  
  (testing
   (lambda ()
     (define pd (create-proxy-dealer (current-input-port) (current-output-port) (create-external)))
     (send pd run-game)))
  
  (define (s f) (species #:food f #:population 1 #:traits `(,ambush ,long-neck)))
  (define (start1 f) (list 12 4 (list (send (s f) to-json)) (map card->json (take all-cards 5))))
  (define OK (make-test "ok"))
  
  ;; -------------------------------------------------------------------------------------------------
  (define in0
    (string-append
     OK
     ;; start
     (make-test (start1 0))
     ;; choose (arguments ignored anyway)
     (make-test (list '() '()))
     ;; and start again:
     (make-test (start1 0))))
  
  (define out0
    (string-append
     (make-test SIGN-UP)
     ;; no response from start
     (make-test '(0 ((1 3)) ((1 4)) ((1 2)) ()))
     ;; no response from second start
     ))
  
  (check-equal? (pipe in0) out0 "checking I/O: for start, choose, start")
  
  ;; -------------------------------------------------------------------------------------------------
  (define in1
    (string-append
     OK
     ;; start
     (make-test (start1 0))
     ;; choose (arguments ignored anyway)
     (make-test (list '() '()))
     ;; feed-next
     (make-test (append (rest (start1 0)) '(1 ())))
     ;; feed-next
     (make-test (append (rest (start1 0)) '(1 ())))
     ;; and start again:
     (make-test (start1 1))))
  
  (define out1
    (string-append
     (make-test SIGN-UP)
     ;; no response from start
     (make-test '(0 ((1 3)) ((1 4)) ((1 2)) ()))
     (make-test 0)
     (make-test 0)
     ;; no response from second start
     ))
  
  (check-equal? (pipe in1) out1 "checking I/O: for start, choose, feed-next, feed-next, start")
  
  ;; -------------------------------------------------------------------------------------------------
  (define in2
    (string-append
     OK
     ;; start
     (make-test (start1 0))
     ;; feed-next
     (make-test (append (start1 0) '(1 ())))))
  
  (check-exn exn:fail? (lambda () (pipe in2)) "checking exn behavior for start -> feed-next"))
