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
   
   ;; type ToJson
   ;; ToJson ToJson ToJson ToJson String -> Void
   ;; (run-testing x y z w  msg) runs (testing)
   ;; on x y z w, compares its output with expected; report msg
   ;; effect: write to files if (write-out-tests)
   run-testing
   
   ;; String JSexpr JSexpr #:result Any -> Void
   ;; runs (testing) on a JSON version of inputs and checks that it is jsexpr-equal? to out0
   ;; if result is not *special, the function also tests equality on the result
   run-json-testing
   
   ;; SYNTAX: (run-json-exn-testing msg:string inputs:jsexpr exn?:predicate)
   ;; runs (testing) on a JSON version of inputs to ensure it raises exn?; msg is the label 
   run-json-exn-testing))

;; ===================================================================================================
;; IMPLEMENTATION

;; (define *debug (open-output-file "/dev/tty" #:exists 'append))
(define *debug (current-error-port))

(define (debug x)
  (displayln x *debug)
  x)

(module+ test
  (require rackunit json)
  
  (define testing
    (make-parameter
     ;; String -> String
     (lambda x (error 'testing "nothing to test"))))
  
  (define-syntax-rule
    (run-testing x ... expected msg)
    ;; --- the ordering matters because x ... might be a mutable object
    (check-equal? ((testing) x ...) expected msg))
  
  (define *special (gensym))
  (define (run-json-testing msg inputs out0 #:result (result *special))
    (define in0 (apply string-append (map make-test inputs)))
    (cond
      [(eq? *special result)
       (check-equal? (to-jsexpr (let-values ([(output result) (pipe in0)]) output)) out0 msg)]
      [else
       (define *result (gensym))
       (check-equal? (to-jsexpr
                      (let-values ([(output result) (pipe in0)])
                        (set! *result result)
                        output))
                     out0
                     (string-append msg "---output"))
       (check-equal? *result result (string-append msg "---result"))]))
  
  (define-syntax-rule
    (run-json-exn-testing msg inputs exn?)
    (let* ([in0 (apply string-append (map make-test inputs))])
      (check-exn exn? (lambda () (define-values (output result) (pipe in0)) output) msg)))
  
  ;; String ->* String Any
  ;; run testing so that it reads from in1, return both its output to current-output-port & the result
  (define (pipe in1)
    (define to-be-tested (testing))
    (define *result (gensym))
    (define output
      (with-output-to-string
       (lambda ()
         (set! *result (with-input-from-string in1 to-be-tested)))))
    (values output *result))
  
  ;; JSexpr -> String 
  (define (make-test x)
    (with-output-to-string
     (lambda ()
       (write-json x)
       (newline)
       (flush-output))))
  
  ;; String -> [Listof JSexpr]
  (define (to-jsexpr s)
    ;; -> [Listof JSexpr]
    (define (loop)
      (define nxt (read-json))
      (if (eof-object? nxt) '() (cons nxt (loop))))
    (with-input-from-string s loop))
  (check-equal? (to-jsexpr "[1,2]\n[3,4]") (list (list 1 2) (list 3 4))))
