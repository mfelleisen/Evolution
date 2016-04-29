#lang racket

(module+ test
  (require rackunit))

;; =============================================================================
;; basic constants for rendering text 
(provide
 TEXT-SIZE
 blank
 
 boxed
 
 label->img
 label->box 
 
 attr->img)

(require 2htdp/image)

(define blank (square 3 'solid 'white))

(define TEXT-SIZE 16)

(define attr-color 'black)
(define label-color 'black)

(define (attr->img a)
  (text (number->string a) TEXT-SIZE attr-color))

(define (label->img l)
  (text l TEXT-SIZE label-color))

(define (label->box s)
  (rectangle (+ 10 (image-width (label->img s))) TEXT-SIZE 'solid 'lime))

(define (boxed label image (align 'center))
  (define bx (rectangle (image-width image) TEXT-SIZE 'solid 'lime))
  (define tx (if (string? label) (text label TEXT-SIZE 'black) label))
  (overlay/align align 'center tx bx))

;; =============================================================================
(provide
 ;; String [string->number?] -> Boolean : valid port number
 valid-port/c

 maybe/c
 ;; [Listof X] -> Boolean 
 unique/c
 ;; some basic predicates
 nat?
 natural+?
 natural?
 
 ;; [N N -> [ Any -> Boolean ]]
 ;; is x an n-tuple of natural numbers with at least min+1 and at most max+1 parts?
 NxN?)

;; -----------------------------------------------------------------------------

(define (valid-port/c x0)
  (define x (string->number x0))
  (and (natural-number/c x) (<= x (expt 2 16))))

(define (maybe/c c)
  (or/c #false c))

(define (unique/c l)
  (cond
    [(empty? l) #t]
    [else (and (not (member (first l) (rest l))) (unique/c (rest l)))]))

(define (nat? x)
  (and (integer? x) (<= 0 x 7)))

(define (natural+? x)
  (and (integer? x) (<= 1 x)))

(define (natural? x)
  (and (integer? x) (<= 0 x)))

(define ((NxN? min max) x)
  (match x
    [`(,(? natural? i) ,(? natural? j*) ...) (<= min (length j*) max)]
    [else #f]))

;; =============================================================================
;; anticipate module split here 

;; dealing with json

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
;; anticipate module split here

(provide
 ;; [List X] -> [Maybe X]
 first/c
 
 ;; [Maybe [NEListof Any]] -> [Maybe [Listof Any]]
 rest/c
 
 ;; [NEList X] -> [Maybe X]
 second/c
 
 ;; [Vectorof X] Index -> [Maybe X]
 vector-ref/c
 
 ;; [Listof X] Index -> [Maybe X]
 list-ref/c
 
 ;; [NEListof X] -> [NEListof X]
 cyclic-rotate
 
 ;; Any -> Boolean
 one?
 two?)

;; -----------------------------------------------------------------------------
(define (first/c l)
  (if (empty? l) #f (first l)))

(define (rest/c l)
  (if (cons? l) (rest l) #f))

(define (second/c l)
  (if (empty? (rest l)) #f (second l)))

(define (vector-ref/c v i)
  (if (<= 0 i (- (vector-length v) 1)) (vector-ref v i) #f))

(define (list-ref/c l i)
  (if (<= 0 i (- (length l) 1)) (list-ref l i) #f))

(define (one? inputs)
  (and (cons? inputs) (empty? (rest inputs))))

(define (two? inputs)
  (and (cons? inputs) (cons? (rest inputs)) (empty? (cddr inputs))))

(define (cyclic-rotate players*)
  (append (rest players*) (list (first players*))))

;; =============================================================================
(provide
 ;; [Listof X] N -> [Listof X]
 (contract-out
  ;; the given list has at least one X 
  [remove-i-th (-> cons? natural-number/c any)])
 
 ;; [Listof X] [Listof X] -> Boolean
 suffix=?
 
 ;; [Listof X] -> [Maybe [Setof X]]
 ;; ensure that the given list represents a set and convert it to a set
 to-set
 
 ;; [Listof N] [Listof X] -> [Listof X]
 ;; (remove-by-index (list i ... j) L) eliminates the i-th ... j-th item from L
 remove-by-index
 
 ;; N X [Listof X] -> [Listof X]
 ;; (replace-by-index i x lox) replaces the i-th item of lox with x
 ;; no change of i is out of range 
 replace-by-index
 )

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
  (check-true (suffix=? '(a b c) '(a b c)))
  (check-true (suffix=? '(b c) '(a b c)))
  (check-true (suffix=? '((b) (c)) '((a) (b) (c))))
  (check-false (suffix=? '(a b c d) '(a b c)))
  (check-false (suffix=? '(b d) '(a b c))))

(define (suffix=? a b)
  (define al (length a))
  (define bl (length b))
  (cond
    [(> al bl) #false]
    [else (equal? (drop b (- bl al)) a)]))

(define (to-set cards-in-play)
  (define cards-as-set (apply set cards-in-play))
  (and (= (length cards-in-play) (set-count cards-as-set))
       cards-as-set))

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



;; =============================================================================

(provide
 ;; SYNTAX
 ;; (send/c t m o ...) is (send t m o ...) unless t is #false 
 send/c)

(define-syntax-rule
  (send/c t m o ...)
  (if (boolean? t) #f (send t m o ...)))

;; =============================================================================

(provide
 
 ;; N N -> [ [Listof X] -> Boolean ]
 between
 
 ;; [Listof X] -> [ N -> Boolean] ]
 valid?
 
 ;; SYNTAX
 ;; (define/checked-io name i-or-o contract name/pre) ensures that 
 define/checked-io
 in
 out)

(define ((valid? cards) n)
  (and (natural? n) (< n (length cards))))

(define (between min max)
  (define msg (format "list of ~a to ~a items" min max))
  (flat-named-contract msg (lambda (l) (<= min (length l) max))))

(define-syntax-rule 
  (define/checked-io nm I/O ctc name/pre m ...)
  (define nm
    (let ((fmt (begin IO-CONTRACT m ...)))
      (with-handlers ([exn:fail:contract?
                       (lambda (x)
                         (printf fmt 'nm I/O 'ctc)
                         (newline)
                         (raise x))])
        (define/contract nm ctc name/pre)
        nm))))

(define-syntax out (syntax-id-rules () [_ 'OUT/c]))
(define-syntax in (syntax-id-rules () [_ 'IN/c]))

(define IO-CONTRACT "~a ~a does not satisfy its contract [~a]")

;; =============================================================================
;; sync-ing

(provide 
  ;; [Listof CML-Events] -> Void
  ;; wait for all 'clients' to happen
  wait-for-all)

(define (wait-for-all clients (wait-time 10))
  (when (and (> wait-time 0) (cons? clients))
    (displayln `(waiting ,wait-time))
    (define done (apply sync/timeout wait-time clients))
    (if done 
        (wait-for-all (remq done clients) wait-time)
        (wait-for-all (remq done clients) (- wait-time 2)))))
