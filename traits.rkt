#lang racket

;; ===================================================================================================
;; representing Evolution traits (mechanisms for specifying, rendering, and comparing traits)

(provide
 ;; type Trait

 ;; Any -> Boolean : Trait
 trait? 

 ;; Trait -> JSexpr
 trait->json

 ;; JSExpr -> Trait
 json->trait
 
 (contract-out
  ;; render this trait as an image 
  [trait->img (-> trait? image?)])

 ;; Image
 empty-trait

 ;; Trait
 carnivore
 attack-traits       ;; [Listof Trait]
 ambush pack-hunting
 defensive-traits    ;; [Listof Trait]
 burrowing warning-call hard-shell herding horns
 action-trait        ;; [Listof Trait]
 climbing
 common-traits       ;; [Listof Trait]
 cooperation fat-tissue fertile foraging long-neck scavenger symbiosis

 all-but-carnivore   ;; [Listof Trait]

 ;; Trait Trait -> Boolean
 <-trait

 ;; (-> Any Boolean) : Specific Trait
 carnivore?
 ambush? warning-call? burrowing? climbing? hard-shell? herding? horns? pack-hunting?
 cooperation? fat-tissue? fertile? foraging? long-neck? scavenger? symbiosis?)

;; ===================================================================================================
(require "basics.rkt" 2htdp/image)

(module+ test
  (require rackunit))

;; ===================================================================================================
;; syntax: use singleton-pattern to represent traits via structs

;; (define-traits n:id t1:id ... tn:id)
;; creates the traits t1 through tn and also collects them a list, which is then named n
;; effect: remember the new traits in *rendering 

(define *rendering '())

(define-syntax (define-traits stx)
  (syntax-case stx ()
    [(_ name n ...)
     (with-syntax ([(n? ...) (map mk-pred (syntax->list #'(n ...)))])
       #'(begin
           ;; (provide n n?) ... ;; why not here? I don't want readers to know how it is implemented
           (define-values (n n?)
             (let ()
               (struct n ())
               (define x (n))
               (set! *rendering (cons `(,x ,(symbol->string 'n)) *rendering))
               (values x n?)))
           ...
           (define name (list n ...))))]))

;; Identifier -> Identifier
;; create the predicate identifier for n; example: (mk-pred #'carnivore) == #'carnivore?
(define-for-syntax (mk-pred n)
  (define n:symbol (syntax-e n))
  (define n:string (symbol->string n:symbol))
  (define r:symbol (string->symbol (string-append n:string "?")))
  (datum->syntax n r:symbol))

;; ---------------------------------------------------------------------------------------------------
(define-traits tester* tester) ;; define a special tester trait
(module+ test
  (check-equal? (json->trait (trait->json tester)) tester))

(define (trait->json x)
  (define r (assq x *rendering))
  (if r (second r) (error 'trait->json "~e" x)))

(define (json->trait j)
  (define r (argmax (lambda (x) (if (string=? (second x) j) 1 0)) *rendering))
  (if r (first r) (error 'json->trait "~e" j)))

;; ---------------------------------------------------------------------------------------------------
;; specifying traits 

(define-traits _ carnivore)

(define-traits attack-traits
  ambush pack-hunting)

(define-traits defensive-traits
  burrowing hard-shell herding horns warning-call)

(define-traits action-trait
  climbing)

(define-traits common-traits
  cooperation fat-tissue fertile foraging long-neck scavenger symbiosis)

(define all-but-carnivore (append attack-traits defensive-traits action-trait common-traits))

(define (trait? x)
  (or (carnivore? x) (member x all-but-carnivore)))

;; ---------------------------------------------------------------------------------------------------
;; comparing traits

(module+ test
  (check-true (<-trait ambush foraging))
  (check-true (<-trait herding horns))
  (check-false (<-trait herding hard-shell)))

(define (<-trait t1 t2)
  (string<? (trait->json t1) (trait->json t2)))

;; ---------------------------------------------------------------------------------------------------
;; render traits as images 
(define trait-front-color 'black)
(define trait-back-color  'pink)

(define all-trait-images
  (local ((define all-traits-img
            (for/list ((t *rendering))
              (cons (first t) (text (second t) TEXT-SIZE trait-front-color))))
          (define trait-width (+ 10 (apply max (map (compose image-width cdr) all-traits-img))))
          (define trait-box (rectangle trait-width TEXT-SIZE 'solid trait-back-color)))
    (for/list ((t all-traits-img))
      (cons (car t) (overlay/align 'left 'center (cdr t) trait-box)))))

(define a-trait-image (cdr (first all-trait-images)))
(define empty-trait
  (rectangle (image-width a-trait-image) (image-height a-trait-image) 'solid 'white))

(define (trait->img t)
  (cdr (assq t all-trait-images)))

(module+ test
  (trait->img (first (list-ref *rendering (random (length *rendering))))))
