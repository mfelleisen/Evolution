#lang racket

;; ===================================================================================================
;; represent a sepcies board

;; EXTERNAL SERVICES

(require (only-in "traits.rkt" trait?) (only-in "basics.rkt" natural? between unique/c))

(define MIN-TRAITS 0)
(define MAX-TRAITS 3)
(define MAX-BODY 7)
(define MIN-BODY 0)
(define MAX-POPULATION 7)
(define MIN-POPULATION 0)

(define trait/c any/c)

(define body/c (and/c natural? (between/c MIN-BODY MAX-BODY)))
(define (fat-food/c b) (and/c natural? #;(or (unsupplied-arg? b) (<=/c b))))
(define (food/c p) (and/c natural? (<=/c p)))
(define population/c (and/c natural? (between/c MIN-POPULATION MAX-POPULATION)))
(define traits/c (and/c [listof trait?] (between MIN-TRAITS MAX-TRAITS) unique/c))

(define species/c
  (object/c
   (field
    [food       (food/c MAX-POPULATION)]     ;; <-- weakness in contract system 
    [fat-food   (fat-food/c MAX-BODY)] ;; <-- weakness in contract system 
    [body       body/c]
    [population population/c]
    [traits     traits/c])
   [attackable?               any/c] ;; base
   [score                     any/c] ;; internal
   [replace-trait             any/c] ;; internal 
   [move-fat                  any/c] 
   [store-fat                 any/c]
   [all-fed?                  any/c]
   [population+1              any/c]
   [population#               any/c] ;; population count for attacks
   [reduce-population-to-food any/c] 
   [move-food-to-bag          any/c] 
   [body+1                    any/c]
   [body#                     any/c] ;; body size for attacks 
   [fat-food-needed           any/c]
   [has                       any/c]
   [kill1 (->dm () #:pre (> (get-field population this) 0) [_ any/c])]
   [attack!
    ;; remove 1 animal from this species population
    ;; -- signal whether this defends itself with horns 
    ;; -- signal whether this dies out due to the attack
    (->dm () #:pre (> (get-field population this) 0) (values [horns? boolean?][dies? boolean?]))]
   [feed1
    ;; add 1 token of food to this species
    ;; ASSUME (< food population)
    ;; -- signal whether this species is a foraging one
    ;; -- signal whether this species is cooperating 
    (->dm () #:pre (not (send this all-fed?)) (values [forage? boolean?] [coop? boolean?]))]))


(provide
 MAX-POPULATION
 MIN-POPULATION
 MAX-BODY
 MIN-BODY
 MAX-TRAITS
 MIN-TRAITS
 
 species/c
 body/c
 population/c
 
 ;; -> Species 
 create-species
 
 (contract-out
  [species
   (->i ()
        (#:body       [b      body/c]
         #:fat-food   [ff (b) (fat-food/c b)]
         #:food       [f  (p) (food/c p)]
         #:population [p      population/c]
         #:traits     [t      traits/c])
        [r species/c])]
  
  [json->species
   ;; convert a JSexpr into a Species if it meets the JSON and Species specs 
   ;; EFFECT raise exn:misc:match? otherwise 
   ;; HINT use json->opt-species instead of json->species when additional possibilities exist
   ;; (-> JSexpr Species)
   any/c])
 
 ;; syntax
 ;; (json->opt-species JSexpr MatchClause ...) : [Maybe Species]
 json->opt-species)

;; ===================================================================================================
;; DEPENDENCIES

(require (except-in "traits.rkt" trait?) (except-in "basics.rkt" natural? between) 2htdp/image)

;; for debugging
(require  "common.rkt")

(module+ test
  (require rackunit (submod ".."))
  
  (require (submod "common.rkt" test))
  (write-out-tests #f)
  (testing (lambda (x y z w) (send x attackable? y z w))))

;; ===================================================================================================
;; IMPLEMENTATION

(define HARD-SHELL-ATTACK 4)
(define HORN-DAMAGE -1)

;; ---------------------------------------------------------------------------------------------------

(define (create-species)
  (new species%))

(define (species #:body (body 0)
                 #:fat-food (fat-food 0)
                 #:food (food 0)
                 #:population (population 1)
                 #:traits (traits '()))
  (define-syntax-rule (set food) (set-field! food s food))
  (define s (new species%))
  (set food)
  (set body)
  (set population)
  (set traits)
  (set fat-food)
  s)

(define-syntax-rule
  (json->opt-species j false-clause ...)
  ;; ==> 
  (match j
    false-clause ...
    [`(("food" ,(? nat? f)) ("body" ,(? nat? b)) ("population" ,(? nat? p))
                            ("traits" ,(? lot? t)))
     (species #:body b #:food f #:population p #:traits (map json->trait t))]
    [`(("food" ,(? nat? f)) ("body" ,(? nat? b)) ("population" ,(? nat? p))
                            ("traits" ,(? lot? t))
                            ("fat-food" ,(? nat? ff)))
     (species #:body b #:fat-food ff #:food f #:population p #:traits (map json->trait t))]))

(define (nat? x)
  (and (natural? x) (<= 0 x 7)))

(define (json->species j)
  (json->opt-species j))

;; Any -> Boolean 
(define (lot? l)
  (and (list? l) (<= (length l) SPECIES-TRAITS)))

(define SPECIES-TRAITS 3)

;; ---------------------------------------------------------------------------------------------------
;; species% : Species

(define species%
  (class* object% (equal<%>)
    (super-new)
    
    (field
     [food 0]
     [fat-food 0]
     [body 0]
     [population 0]
     [traits '()])
    
    (when (and (> fat-food 0) (not (has fat-tissue?)))
      (error 'species% "inconsistent object (stores tokens but has no fat tissue)"))
    
    ;; -----------------------------------------------------------------------------
    (define/public (equal-to? other r)
      (and (r (get-field traits other) traits)
           (= (get-field food other) food)
           (= (get-field fat-food other) fat-food)
           (= (get-field body other) body)
           (= (get-field population other) population)))
    
    ;; this is basically nonsense 
    (define/public (equal-hash-code-of hash-code)
      (hash-code traits))
    
    ;; this is basically nonsense 
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code traits))
    
    ;; -----------------------------------------------------------------------------
    (define/public (to-json)
      `(("food" ,food)
        ("body" ,body)
        ("population" ,population)
        ("traits" ,(map trait->json traits))
        ,@(if (and (> fat-food 0) (has fat-tissue?)) `(("fat-food" ,fat-food)) '())))
    
    ;; -----------------------------------------------------------------------------
    (define/public (attackable? attacker left right)
      (cond
        [(and (or (send/c left has warning-call?) (send/c right has warning-call?))
              (not (send attacker has ambush?)))
         #false]
        [(and (has burrowing?) (all-fed?)) #false]
        [(and (has climbing?) (not (send attacker has climbing?))) #false]
        [(and (has herding?) (<= (send attacker population#) (population#))) #false]
        [(and (has hard-shell?) (<= (send attacker body#) (body#))) #false]
        [else (if (has horns?) HORN-DAMAGE #true)]))
    
    (define/public (has p?)
      (ormap p? traits))
    
    (define/public (all-fed?)
      (= food population))
    
    (define/public (fat-food-needed)
      (- body fat-food))
    
    (define/public (body#)
      (cond
        [(and (has hard-shell?) #; (not (has carnivore?)))
         (+ body HARD-SHELL-ATTACK)]
        [(and (has pack-hunting?) (has carnivore?))
         (+ body population)]
        [else body]))
    
    (define/public (body+1)
      (set! body (+ body 1)))
    
    (define/public (population#)
      (if (has pack-hunting?) (+ body population) population))
    
    (define/public (feed1)
      (set! food (+ food 1))
      (values (has foraging?) (has cooperation?)))
    
    (define/public (store-fat n)
      (set! fat-food (+ fat-food n)))
    
    (define/public (move-fat)
      (define food-movable (- population food))
      (when (> food-movable 0)
        (define n (min food-movable fat-food))
        (set! fat-food (- fat-food n))
        (set! food (+ food n))))
    
    (define/public (attack!)
      (values (has horns?) (kill1)))
    
    (define/public (population+1)
      (when (< population MAX-POPULATION)
        (set! population (+ population 1))))
    
    (define/public (kill1)
      (set! population (- population 1))
      (when (< population food)
        (set! food population))
      (= population 0))
    
    (define/public (reduce-population-to-food)
      ;; food >= 0
      (set! population food)
      (= population 0))
    
    (define/public (replace-trait i new-trait)
      (set! traits (replace-by-index i  new-trait traits)))
    
    (define/public (move-food-to-bag)
      (begin0 food
              (set! food 0)))
    
    (define/public (score)
      (length traits))))

;; ===================================================================================================
(module+ test
  ;; -------------------------------------------------------------------------------------------------
  (define (attacker1 2traits)
    (species #:body 3 #:food 2 #:population 4 #:traits `(,carnivore ,@2traits)))
  
  ;; -------------------------------------------------------------------------------------------------
  (check-equal? (json->species (send (attacker1 '()) to-json)) (attacker1 '())
                "json->species is left-inverse for to-json")
  
  (define s0 (species #:fat-food 2 #:traits `(,fat-tissue)))
  (check-equal? (json->species (send s0 to-json)) s0
                "json->species is left-inverse for to-json, with fat food")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; attackable tests
  
  (define att-plain (attacker1 '()))
  
  (define def-plain (species #:body 1 #:food 3 #:population 4))
  (run-write-json-test2 def-plain att-plain #f #f #true "plain attack")
  
  (define (def-burrow f p) (species #:body 1 #:food f #:population p #:traits `(,burrowing)))
  (run-write-json-test2 (def-burrow 1 1) att-plain #f #f #false "defend with burrowing")
  (run-write-json-test2 (def-burrow 3 4) att-plain #f #f #true  "overcome burrowing")
  
  (define def-climbing (species #:body 1 #:food 3 #:population 4 #:traits `(,climbing)))
  (define att-climbing (attacker1 `(,climbing)))
  (run-write-json-test2 def-climbing att-plain    #f #f #false "defend with climbing")
  (run-write-json-test2 def-climbing att-climbing #f #f #true  "overcome climbing")
  
  (define def-hard (species #:body 2 #:food 2 #:population 3 #:traits `(,hard-shell)))
  (run-write-json-test2 def-hard att-plain #f #f #false "defend with hard shell")
  
  (define att-big (species #:body 7 #:food 2 #:population 3 #:traits `(,carnivore)))
  (define att-pack (species #:body 3 #:food 2 #:population 4 #:traits `(,carnivore ,pack-hunting)))
  (run-write-json-test2 def-hard att-big  #f #f #true "overcome hard shell with large size")
  (run-write-json-test2 def-hard att-pack #f #f #true "overcome hard shell with pack hunting")
  
  (define def-wc (species #:body 2 #:food 2 #:population 3 #:traits `(,warning-call)))
  (define att-ambush (attacker1 `(,ambush)))
  
  (run-write-json-test2 def-plain att-plain def-wc #f     #false "defend with warning call left")
  (run-write-json-test2 def-plain att-plain #f     def-wc #false "defend with warning call right")
  (run-write-json-test2 def-plain att-plain def-wc def-wc #false "defend with warning call both")
  (run-write-json-test2 def-plain att-ambush #f    def-wc #true  "overcome with warning call ambush")
  
  (define (def with) (species #:body 2 #:food 2 #:population 2 #:traits `(,hard-shell ,@with)))
  (define att-ambush-pack (attacker1 `(,ambush ,pack-hunting)))
  
  (run-write-json-test2 (def '())          att-ambush-pack def-wc #f #true "overcome hards w/ pack")
  (run-write-json-test2 (def `(,climbing)) att-ambush-pack def-wc #f #false "defend mix w/ climbing")
  
  (run-write-json-test2 (def `(,pack-hunting)) att-plain #f #f #false
                        "defend w/ hard-shell & pack-hunting (fail the trait people)"))
