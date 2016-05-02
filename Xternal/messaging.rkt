#lang racket

(require (only-in json jsexpr?) racket/exn)

(define LOCALHOST "127.0.0.1")
(define REMOTE-PORT 45678)
(define ACCEPT-TIMEOUT 5) ;; seconds. See (server).
(define TIMEOUT 5) ;; seconds. See read-json-safely/timeout.

(provide
 
 LOCALHOST
 REMOTE-PORT
 ACCEPT-TIMEOUT
 
 unset-time-out
 TIMEOUT
 
 (contract-out
  [send-message
   ;; JSexpr -> Void
   ;; EFFECT send Jsexpr as bytes 
   (-> jsexpr? #t)]
  [read-message
   ;; -> JSexpr
   ;; Read a blob of JSON, treating any network error as EOF, and only waiting for TIMEOUT seconds.
   ;; (Because tcp-read gets RST from linux servers from time to time.)
   ;; EFFECT a timed reader for JSON messages
   ;; EFFECT log errors and exceptions and time-outs 
   (-> (or/c jsexpr? eof-object? 'timeout-1 'timeout-2 'error))]))

;; ===================================================================================================
;; DEPENDENCIES

(require (except-in json jsexpr?) racket/exn)

;; ===================================================================================================
;; IMPLEMENTATION

(define (unset-time-out)
  (set! TIMEOUT 1000000000))

(define (send-message i)
  (with-handlers ([exn:fail:network? (lambda (e) #f)])
    (define output-bytes (jsexpr->bytes i #:encode 'control))
    (define output-length (bytes-length output-bytes))
    (write-bytes output-bytes)
    (write-byte 32)
    (flush-output)
    #t))

(define (read-message)
  (with-handlers ([exn:fail:network? (lambda (_exn) eof)])
    (read-json/timeout TIMEOUT TIMEOUT)))

;; Read a blob of JSON with a timeout for the first byte of input to appear
;; and a second timeout by which the entirety of the blob should have appeared.
(define (read-json/timeout start-timeout-sec response-duration-timeout-sec)
  (define control-ch (make-channel))
  (define reply-ch (make-channel))
  (define read-thread
    (thread
     (lambda ()
       (cond
         [(sync/timeout start-timeout-sec (current-input-port))
          (channel-put control-ch 'response-started)
          (with-handlers [(values (lambda (e) (channel-put reply-ch (list 'exn e))))]
            (channel-put reply-ch (list 'ok (read-json))))]
         [else (channel-put control-ch 'response-not-started)]))))
  (match (channel-get control-ch)
    ['response-not-started
     (log-info "Timed out waiting for reading to start.")
     'timeout-1]
    ['response-started
     (match (sync/timeout response-duration-timeout-sec reply-ch)
       [(list 'ok blob) blob]
       [(list 'exn (? exn:fail:network?)) eof]
       [(list 'exn e)
        (log-info "Error reading message:\n~a" (exn->string e))
        'error]
       [#f
        (log-info "Timed out waiting for reading to complete.")
        'timeout-2])]))