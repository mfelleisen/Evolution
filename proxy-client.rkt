#! /bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#
#lang typed/racket

;; ===================================================================================================
;; a one-player client for an Evolution player

(define-type Client (->* (Natural) (String Natural) Void))

(provide:
 (main
  ;; EFFECT connects to an evolution game server on first optional argument
  ;; or LOCALHOST at the second optional argument or REMOTE-PORT 
  Client)

 (infsu
  ;; EFFECT go into infinite loop after establishing tcp connection
  Client)

 (ill-formed
  ;; EFFECT send ill-formed JSON
  Client)

 (huge
  ;; EFFECT send huge input string
  Client)

 (deep
  ;; EFFECT send deeply nested but well-formed JSON array
  Client)
 
 (bad-no-fc
  ;; (bad-no-fc n) a client like main whose choose method produces inappropriate JSON after n calls
  (-> Natural Client))

 (bad-over-growth
  ;; (bad-over-growth n) a client like main whose choose method produces cheating choice after n calls
  (-> Natural Client))

 (bad-choice-inf
  ;; (bad-choice-inf n) a client like main whose choose method diverges loop after n rounds
  (-> Natural Client))

 (bad-feed-bad
  ;; (bad-no-fc n) a client like main whose feed method produces a bad response after n rounds
  (-> Natural Client))

 (bad-feed-inf
  ;; (bad-feed-inf n) a client like main whose feed method goes diverges after n rounds
  (-> Natural Client)))

;; ===================================================================================================
(define-type Dealer (->* (Input-Port Output-Port Any) (String) (Object [run-game (-> Void)])))

(require/typed
 "proxy-dealer.rkt"
 [create-proxy-dealer Dealer]
 [create-proxy-dealer-infsu Dealer]
 [create-proxy-dealer-bad-sign-up Dealer])

(define-type Bad-Choice (-> Natural (-> Any Any Any)))
(define-type Bad-Feed   (-> Natural (-> Any Any Any Any Any Any)))

(require/typed
 "player-external.rkt"
 [create-bad-choose (-> (-> Any Any Any) Any)]
 [create-bad-feed   (-> (-> Any Any Any Any Any Any) Any)]
 [no-fc             Bad-Choice]
 [over-growth       Bad-Choice]
 [choice-inf        Bad-Choice]
 [feed-inf          Bad-Feed]
 [feed-bad          Bad-Feed]
 [create-external   (-> Any)])

(require/typed
 "../common.rkt"
 [LOCALHOST String]
 [REMOTE-PORT Natural]
 [debug (-> Any Any)])

;; ===================================================================================================

(define TRIES 3)
(define WAIT-FOR 2)

(: make-make-main (-> Dealer (-> [-> Any] String Client))) ;; work-around for provide: bug
(define [([make-make-main create] create-external sign-up) id [server@ LOCALHOST][port REMOTE-PORT]]
  (define-values (in out) 
    (let loop : (Values Input-Port Output-Port) ([n : Natural TRIES])
      (with-handlers ((exn:fail:network?
                       (lambda ({x : exn})
                         (log-info "~a is waiting ~a tries left" id n)
                         (sleep WAIT-FOR)
                         (if (<= n 0) (raise x) (loop (- n 1))))))
        (tcp-connect server@ port))))
  (send (create in out (create-external) sign-up) run-game))

(define infsu [(make-make-main create-proxy-dealer-infsu) create-external "infinite loop"])

(define make-bad (make-make-main create-proxy-dealer-bad-sign-up))

(define ill-formed (make-bad create-external "[[[[[[[[["))
(define huge (make-bad create-external (make-string 100 #\a)))
(define deep (make-bad create-external (string-append (make-string 200 #\[) (make-string 200 #\]))))

(define make-main (make-make-main create-proxy-dealer))

(: main Client) 
(define main (make-main create-external "good guy"))

(: make-make-bad (All (α) (-> (-> α Any) (-> (-> Natural α) String (-> Natural Client)))))
(define (((make-make-bad create-bad-choose) bad-choice fmt) n)
  (: external-bad-chooser (-> Natural (-> Any)))
  (define (external-bad-chooser n)
    (lambda () (create-bad-choose (bad-choice n))))
  (make-main (external-bad-chooser n) (format fmt n)))

; (: make-bad-choose (-> Bad-Choice String (-> Natural Client)))
(define make-bad-choose (make-make-bad create-bad-choose))

(: bad-no-fc (-> Natural Client))
(define bad-no-fc (make-bad-choose no-fc "no fc ~a"))

(: bad-over-growth (-> Natural Client))
(define bad-over-growth (make-bad-choose over-growth "bad pop growth ~a"))

(: bad-choice-inf (-> Natural Client))
(define bad-choice-inf (make-bad-choose choice-inf "bad choice inf ~a"))

; (: make-bad-feed (-> Bad-Feed String (-> Natural Client)))
(define make-bad-feed (make-make-bad create-bad-feed))

(: bad-feed-bad (-> Natural Client))
(define bad-feed-bad (make-bad-feed feed-bad "bad feed ~a"))

(: bad-feed-inf (-> Natural Client))
(define bad-feed-inf (make-bad-feed feed-inf "bad feed inf ~a"))