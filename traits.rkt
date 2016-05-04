#lang racket

;; ===================================================================================================
;; representing Evolution traits (mechanisms for specifying, rendering, and comparing traits)

;; EXTERNAL SERVICES

(provide
 ;; type Trait

 ;; Any -> Boolean : Trait
 trait? 

 ;; Trait -> String 
 trait->string 

 ;; JSExpr -> String
 string->trait
 
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
;; DEPENDENCIES

(module+ test
  (require rackunit))

;; ===================================================================================================
;; IMPLEMENTATION

;; syntax: use singleton-pattern to represent traits via structs

;; (define-traits n:id t1:id ... tn:id)
;; creates the traits t1 through tn and also collects them a list, which is then named n
;; EFFECT remember the new traits in *rendering 

(define *trait-x-string-representation '())

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
               (set! *trait-x-string-representation
                     (cons `(,x ,(symbol->string 'n)) *trait-x-string-representation))
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
  (check-equal? (string->trait (trait->string tester)) tester))

(define (trait->string x)
  (define r (assq x *trait-x-string-representation))
  (if r (second r) (error 'trait->string "~e" x)))

(define (string->trait j)
  (define r (argmax (lambda (x) (if (string=? (second x) j) 1 0)) *trait-x-string-representation))
  (if r (first r) (error 'string->trait "~e" j)))

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
  (string<? (trait->string t1) (trait->string t2)))

