#lang racket 

(require json)

;; -----------------------------------------------------------------------------
(module+ test
  
  ;; ---------------------------------------------------------------------------
  (provide
   #;
   (case->
    (-> Boolean Void)
    (-> Boolean))
   ;; parameter: should tests be writte out?
   write-out-tests
   
   #;
   (case->
    (-> (-> String String) Void)
    (-> (-> String String)))
   ;; parameter: the function to be tested
   testing
   
   ;; String String String -> Void
   ;; (run-write-json-test in out msg) pipes in into (testing)
   ;; and compares its output with out; report msg
   ;; effect: write to files if (write-out-tests)
   run-write-json-test
   
   ;; type ToJson
   #;
   [Maybe (Class (to-json (-> Any JSexpr)))]
   ;; ToJson ToJson ToJson ToJson String -> Void
   ;; (run-write-json-test2 x y z w  msg) runs (testing)
   ;; on x y z w, compares its output with expected; report msg
   ;; effect: write to files if (write-out-tests)
   run-write-json-test2
   
   run-write-json-test3
   
   run-write-json-test2/error
   
   ;; String -> String
   ;; run (testing) on the given string and produce output as string 
   pipe
   
   ;; JSexpr ... -> String 
   ;; turn the series of JSON expressions into a single string
   make-test
   
   write-test-case
   add-test-input
   
   to-json*)
  
  ;; ---------------------------------------------------------------------------
  (require rackunit)
  
  (define write-out-tests (make-parameter #f))
  
  (define testing
    (make-parameter
     ;; String -> String
     (lambda x (error 'testing "nothing to test"))))
  
  (define (run-write-json-test in1 out1 msg)
    (check-equal? (to-jsexpr (pipe in1)) (to-jsexpr out1) msg)
    (write-test-case in1 out1))
  
  (define-syntax-rule
    (run-write-json-test2 x ... expected msg)
    (begin 
      (define in1 (make-test (map to-json* (list x ...))))
      ;; --- the ordering matters because x ... might be a mutable object
      (check-equal? ((testing) x ...) expected msg)
      ;; --- now we get the output from the possibly mutated object 
      (define out1 (make-test (to-json* expected)))
      (write-test-case in1 out1)))
  
  (define-syntax-rule
    (run-write-json-test3 x ... expected expected->json msg)
    (begin 
      (define in1 (make-test (map to-json* (list x ...))))
      ;; --- the ordering matters because x ... might be a mutable object
      (check-equal? ((testing) x ...) expected msg)
      ;; --- now we get the output from the possibly mutated object 
      (define out1 (make-test (expected->json expected)))
      (write-test-case in1 out1)))
  
  (define-syntax-rule
    (run-write-json-test2/error x ... expected msg)
    (begin
      (check-exn expected (lambda () ((testing) x ...)) msg)
      (define in1 (make-test (map to-json* (list x ...))))
      (define out1 (make-test))
      (write-test-case in1 out1)))
  
  ;; Any -> JSexpr
  (define (to-json* an-x)
    (cond
      [(object? an-x) (send an-x to-json)]
      [(list? an-x) (map to-json* an-x)]
      [else an-x]))
  
  (define (make-test . x)
    (with-output-to-string
     (lambda ()
       (for/list ((x x))
         (write-json x)
         (newline)
         (flush-output)))))
  
  (define (pipe in1)
    (define to-be-tested (testing))
    (with-output-to-string (lambda () (with-input-from-string in1 to-be-tested))))
  
  ;; effect: write the test, consisting of input and expected output,
  ;;   to two files: <n>-in.json and <n>-out.json for some n in N
  (define *test-case 0)
  (define (write-test-case in0 out0)
    (when (write-out-tests)
      (define in (if (string? in0) in0 (make-test in0)))
      (define out (if (string? out0) out0 (make-test out0)))
      (set! *test-case (+ *test-case 1))
      (write-test-part "~a-in.json" in)
      (write-test-part "~a-out.json" out)))
  
  ;; effect 
  (define (add-test-input in1)
    (when (write-out-tests)
      (define fname (format "~a-in.json" *test-case))
      (define input (with-input-from-file fname read-json))
      (with-output-to-file fname #:exists 'replace (lambda () (write-json `(,input ,in1))))))
  
  ;; String -> [Listof JSexpr]
  (define (to-jsexpr s)
    ;; -> [Listof JSexpr]
    (define (loop)
      (define nxt (read-json))
      (if (eof-object? nxt) '() (cons nxt (loop))))
    (with-input-from-string s loop))
  
  (check-equal? (to-jsexpr "[1,2]\n[3,4]") (list (list 1 2) (list 3 4)))
  
  ;; FormatString String -> Void
  ;; effect: write in or out part
  (define (write-test-part fmt txt)
    (define fname (format fmt *test-case))
    (with-output-to-file fname #:exists 'replace (lambda () (displayln txt)))))

;; -----------------------------------------------------------------------------
(provide
 debug)

;; (define *debug (open-output-file "/dev/tty" #:exists 'append))
(define *debug (current-error-port))

(define (debug x)
  (displayln x *debug)
  x)
