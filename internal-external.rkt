#lang racket

;; =============================================================================
;; a library that protects calls from exceptions and overly slow clients 

(define TIMEOUT 1)

(provide
 ;; SYNTAX
 ;; (interact-with-external Name:Natural+ p:expr m:id a:expr ...)
 ;;   : (or/c client-error? any/c)
 ;; calls method  m on player p with arguments a ...
 ;; a normal return value within TIMEOUT seconds is passed on,
 ;; if the call raises an exception or takes too long, the result
 ;; is a unique value that satisfies client-error?
 send-to-external

 ;; [JSexpr -> X] -> X
 ;; EFFECT: raise CLIENT-ERROR
 receive-external

 ;; Any -> Boolean 
 client-error?)

;; =============================================================================
(require "basics.rkt" "../common.rkt")

(module+ test
  (require rackunit))

;; =============================================================================

(define-syntax-rule
  (send-to-external id p m a ...)
  (let ()
    (define c (make-custodian))
    (struct ok (value))
    (parameterize ((current-custodian c))
      (define ch (make-channel))
      (thread
       (lambda ()
         (with-handlers ((client-error? (curry channel-put ch))
                         (void (handler id ok ch)))
           (channel-put ch (ok (send p m a ...))))))
      (define result (sync/timeout TIMEOUT ch))
      (custodian-shutdown-all c)
      (cond
        [(ok? result) (ok-value result)]
        [(client-error? result) result]
        [(boolean? result) (time-out-handler id)]
        [else (error 'send-to-external "something went horribly wrong")]))))

;; Channel -> Any -> Void 
(define ((handler id ok ch) x)
  (log-info "the client player ~a raised an exception: ~a" id (exn-message x))
  (channel-put ch (ok CLIENT-ERROR)))

;; Name -> client-error?
(define (time-out-handler id)
  (log-info "the client player ~a timed out" id)
  CLIENT-ERROR)

(struct client-error ())
(define CLIENT-ERROR (client-error))

(define (receive-external json->value)
  (define in (read-message))
  (match-define `(,ok? ,msg) (from-json json->value in))
  (cond
    [ok? msg]
    [else
     (log-info "proxy player says: client sent the wrong kind of JSON: ~a" (exn-message msg))
     (raise CLIENT-ERROR)]))

;; ===================================================================================================
(module+ test

  (define GOOD 5)
  (define test%
    (class object%
      (super-new)
      (define/public (good) GOOD)
      (define/public (better) #false)
      (define/public (diverge) (let loop () (loop)))
      (define/public (raise-exn) (/ 1 0))))
  (define test (new test%))
  
  (check-equal? (send-to-external 1 test good) GOOD)
  (check-equal? (send-to-external 1 test better) #false)
  (check-true (client-error? (send-to-external 2 test raise-exn)))
  (check-true (client-error? (send-to-external 3 test diverge)))
  
  (define (json->false f) (match f [#f #f]))
  (check-exn client-error?
             (lambda ()
               (with-input-from-string "" (lambda () (receive-external json->false))))))
