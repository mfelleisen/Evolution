#lang racket 

;; ===================================================================================================
;; this file provides a debugging routine and -- from (modul test ..) functions/syntax for testing,
;; including json testing in Xternal/

;; SERVICES
(provide
 debug)

(module+ test
  (provide
   ;; parameter: the function to be tested
   testing
   
   ;; SYNTAX (run-testing x ... w msg) runs (testing)
   ;; on x ..., compares its output with w; reports msg for failures
   run-testing))

;; ===================================================================================================
;; IMPLEMENTATION

;; (define *debug (open-output-file "/dev/tty" #:exists 'append))
(define *debug (current-error-port))

(define (debug x)
  (displayln x *debug)
  x)

(module+ test
  (require rackunit)
  
  (define testing
    (make-parameter
     ;; String -> String
     (lambda x (error 'testing "nothing to test"))))
  
  (define-syntax-rule
    (run-testing x ... expected msg)
    ;; --- the ordering matters because x ... might be a mutable object
    (check-equal? ((testing) x ...) expected msg)))
