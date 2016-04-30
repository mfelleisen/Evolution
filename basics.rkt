#lang racket

(module+ test
  (require rackunit))

;; =============================================================================
;; CONTRACTS 

(provide
 ;; N N -> [ [Listof X] -> Boolean ]
 between
 
 ;; [Listof X] -> Boolean 
 unique/c
 
 ;; some basic predicates
 natural+?
 natural?
 
 ;; [N N -> [ Any -> Boolean ]]
 ;; is x an n-tuple of natural numbers with at least min+1 and at most max+1 parts?
 NxN?
 
 ;; Any -> Boolean
 one?)

(define (between min max)
  (define msg (format "list of ~a to ~a items" min max))
  (flat-named-contract msg (lambda (l) (<= min (length l) max))))

(define (unique/c l)
  (cond
    [(empty? l) #t]
    [else (and (not (member (first l) (rest l))) (unique/c (rest l)))]))

(define (natural+? x)
  (and (integer? x) (<= 1 x)))

(define natural? natural-number/c)

(define ((NxN? min max) x)
  (match x
    [`(,(? natural? i) ,(? natural? j*) ...) (<= min (length j*) max)]
    [else #f]))

(define (one? inputs)
  (and (cons? inputs) (empty? (rest inputs))))

;; =============================================================================
;; JSON

(provide
 
 ;; [JSexpr -> X] JSexpr -> [List Boolean (U X Exn)]
 ;; really: returns (values #f Exn) or (values #t X)
 ;; 
 ;; [list #t (convert x)] if the latter succeeds
 ;; [list #f exn] if (convert x) fails with exn:misc:match
 from-json)

;; -----------------------------------------------------------------------------
(define (from-json convert x)
  (with-handlers ((exn:misc:match? (lambda (xn) (list #f xn))))
    (list #t (convert x))))

;; =============================================================================
;; CONDITIONAL ACCESS AND SEND 

(provide
 ;; [List X] -> [Maybe X]
 first/c
 
 ;; [Maybe [NEListof Any]] -> [Maybe [Listof Any]]
 rest/c
 
 ;; [Vectorof X] Index -> [Maybe X]
 vector-ref/c
 
 ;; [Listof X] Index -> [Maybe X]
 list-ref/c
 
 ;; SYNTAX
 ;; (send/c t m o ...) is (send t m o ...) unless t is #false 
 send/c)

(define (first/c l)
  (if (empty? l) #f (first l)))

(define (rest/c l)
  (if (cons? l) (rest l) #f))

(define (vector-ref/c v i)
  (if (<= 0 i (- (vector-length v) 1)) (vector-ref v i) #f))

(define (list-ref/c l i)
  (if (<= 0 i (- (length l) 1)) (list-ref l i) #f))

(define-syntax-rule
  (send/c t m o ...)
  (if (boolean? t) #f (send t m o ...)))

;; =============================================================================
;; some general basic functions 

(provide
 (contract-out
  ;; [Listof X] N -> [Listof X]
  ;; the given list has at least one X 
  [remove-i-th (-> cons? natural-number/c any)])
 
 ;; [Listof N] [Listof X] -> [Listof X]
 ;; (remove-by-index (list i ... j) L) eliminates the i-th ... j-th item from L
 remove-by-index
 
 ;; N X [Listof X] -> [Listof X]
 ;; (replace-by-index i x lox) replaces the i-th item of lox with x
 ;; no change of i is out of range 
 replace-by-index

 ;; [Listof X] -> [Maybe [Setof X]]
 ;; ensure that the given list represents a set and convert it to a set
 to-set
 
 ;; [NEListof X] -> [NEListof X]
 cyclic-rotate)

(module+ test
  (check-equal? (remove-i-th '(x) 0) '())
  (check-equal? (remove-i-th '(x y) 0) '(y))
  (check-equal? (remove-i-th '(x y) 1) '(x))
  (check-equal? (remove-i-th '(x y z) 1) '(x z)))

(define (remove-i-th lox i)
  (if (= i 0)
      ;; for efficiency only: 
      (rest lox)
      (append (take lox i) (drop lox (+ i 1)))))

(module+ test
  (check-equal? (remove-by-index '() '(a b c)) '(a b c))
  (check-equal? (remove-by-index '(0) '(a b c)) '(b c))
  (check-equal? (remove-by-index '(1) '(a b c)) '(a c))
  (check-equal? (remove-by-index '(1 2) '(a b c)) '(a))
  (check-equal? (remove-by-index '(0 1 2) '(a b c)) '()))

(define (remove-by-index index* lox)
  (for/list ((x lox) (i (in-naturals)) #:unless (member i index*))
    x))

(module+ test
  (check-equal? (replace-by-index 0 'x '(a b c)) '(x b c))
  (check-equal? (replace-by-index 1 'x '(a b c)) '(a x c))
  (check-equal? (replace-by-index 2 'x '(a b c)) '(a b x)))

(define (replace-by-index idx nu lox)
  (for/list ((x lox) (i (in-naturals)))
    (if (= i idx) nu x)))

(define (to-set cards-in-play)
  (define cards-as-set (apply set cards-in-play))
  (and (= (length cards-in-play) (set-count cards-as-set))
       cards-as-set))

(define (cyclic-rotate players*)
  (append (rest players*) (list (first players*))))
