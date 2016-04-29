#lang racket

;; ===================================================================================================
;; represent a dealer and its internal representation of a state

(require (only-in "player-internal.rkt" internal-player/c)
         (only-in "player-external.rkt" external-player/c)
         (only-in "next.rkt" dealer-next/c)
         (only-in "cards.rkt" card?)
         (only-in "basics.rkt" natural? between)
         "observer.rkt"
         json)

(define MIN-PLAYERS 3)
(define MAX-PLAYERS 8)

(define dealer-proper/c
  (object/c
   [to-json
    ;; 
    (->m jsexpr?)]
   [register-observer (->m observer/c any)]
   [run-game (->m any/c)]))

(define dealer/c (and/c dealer-proper/c dealer-next/c))

(provide
 ;; constants 
 MIN-PLAYERS
 MAX-PLAYERS
 
 ;; JSexpr -> Dealer
 json->dealer
 
 (contract-out
  [create-dealer
   (->* [(and/c [listof (list/c string? external-player/c)] (between MIN-PLAYERS MAX-PLAYERS))]
        dealer/c)]))

;; ===================================================================================================
(require (only-in "player-external.rkt" json->player)
         (only-in "player-internal.rkt" create-player CARDS-PER-BOARD CARD-PER-PLAYER)
         (except-in "cards.rkt" card?) 
         (except-in "basics.rkt" natural? between)
         (except-in "next.rkt" dealer-next/c)
         "board.rkt" "traits.rkt" 2htdp/image)

;; for debugging 
(require "common.rkt")

(module+ test
  (require rackunit
           (only-in "player-external.rkt"
                    create-external action4->json create-bad-choose no-fc over-growth)
           (only-in (submod "player-internal.rkt" test) player)
           (submod ".."))
  (require (for-syntax syntax/parse))
  (require (submod "common.rkt" test)))

;; ===================================================================================================
(define TITLE "Evolution: the dealer view")

(define (create-dealer players)
  (new dealer% [externals players][cards #;shuffle all-cards]))

(define/contract (dealer #:cards [cards '()] 
                         #:players players
                         #:watering [watering-hole 0])
  (->i (#:players  [players (and/c [listof any/c #;player?] (between MIN-PLAYERS MAX-PLAYERS))])
       (#:cards    [cards [listof valid-card?]]
        #:watering [watering-hole natural-number/c])
       #:pre (cards players)
       (let* ([player-cards  (apply append (map (lambda (p) (get-field cards p)) players))]
              [dealer-cards  (if (unsupplied-arg? cards) '() cards)]
              [cards-as-set  (to-set (append dealer-cards player-cards))])
         (and cards-as-set (subset-of-all-cards? cards-as-set)))
       [r any/c #;==dealer%])
  (define-syntax-rule (set food) (set-field! food s food))
  (define s (new dealer% [cards cards]))
  (set players)
  (set watering-hole)
  s)

(define (json->dealer j)
  (match j
    [`((,player* ...) ,(? natural? watering-hole) (,card* ...))
     (define players (map json->player player*))
     (define cards (map json->card card*))
     (dealer #:cards cards #:players players #:watering watering-hole)]))

;; ---------------------------------------------------------------------------------------------------

(define dealer%
  (class object%
    (init-field
     ;; [Listof Card]
     cards
     
     [externals
      ;; [Listof [List String Player]]
      '()])
    
    (super-new)
    
    (field
     [players
      ;; [Listof Player]]
      ;; the players in the order in which they will take the next turn 
      (for/list ((e externals) (name (in-naturals)))
        (define id (format "~a#~a" (first e) (+ name 1)))
        (create-player id (second e)))]
     
     [watering-hole
      ;; N : the number of food tokens left in the watering hole 
      0])
    
    ;; -----------------------------------------------------------------------------------------------
    ;; equality
    
    (define/public (equal-to? other r)
      (and (r (get-field cards other) cards)
           (= (get-field watering-hole other) watering-hole)
           (r (get-field players other) players)))
    
    ;; this is basically nonsense 
    (define/public (equal-hash-code-of hash-code)
      (hash-code players))
    
    ;; this is basically nonsense 
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code players))
    
    ;; -----------------------------------------------------------------------------------------------
    (define *observers '[])

    (define/public (register-observer o)
      (set! *observers (cons o *observers)))

    (define/private (call-observers)
      (for ((o *observers))
        (define j (to-json))
        (send o display j)
        (sleep 10)))

    ;; -----------------------------------------------------------------------------------------------
    ;; serialization

    (define/public (to-json)
      `(,(map (lambda (p) (send p to-json)) players) ,watering-hole ,(map card->json cards)))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; running a game
    
    ;; Step4   = [Listof Action4]           ; the actions all players wish to perform 
    ;; Results = [Listof [List Name Score]]
    ;; Score   = N
    
    ;; -> Void
    ;; run turns until game is over, then determine results and pretty-print them 
    (define/public (run-game)
      (let run-turns ()
        (unless (over?)
          (call-observers)
          (boards-for-all)
          (turn)
          (when (member 'display (interface->method-names (object-interface this)))
            (send this display))
          (unless (empty? players)
            (set! players (cyclic-rotate players))
            (run-turns))))
      ;; ----------------------------------------------
      (printf "results\n------------------------\n")
      (for ((r (results)) (i (in-naturals)))
        (match-define `(,id ,s) r)
        (printf "~a player id: ~a score: ~a\n" (+ i 1) id s)))
    
    ;; -> Void
    ;; EFFECT add one board to all players that don't have one
    (define/public (boards-for-all)
      (for ((p players))
        (send p add-board-if-needed)))
    
    ;; -> Boolean
    ;; are there too few cards left to start a turn?
    (define/private (over?)
      (or (empty? players)
          (< (length cards) (for/sum ((p players)) (send p how-many-cards-needed)))))
    
    ;; -> Results 
    ;; determine the scores of the players and sort them in descending order (with id)
    (define/public (results)
      (define raw-scores
        (for/list ((p players))
          (list (get-field id p) (send p score))))
      (sort raw-scores >= #:key second))
    
    ;; -> Void
    ;; EFFECT run a complete turn
    ;; ASSUME there are enough cards to launch a turn 
    (define/public (turn)
      (unless (empty? players)
        (step1)
        (unless (empty? players)
          (define action (step2-3))
          (unless (empty? players)
            (step4 action)
            (reduce-population-to-food)
            (move-food-to-bags)))))
    
    ;; -> Void
    ;; EFFECT distribute cards to players
    ;; ASSUME called only if (not (over?)) holds and (boards-for-all) has been run 
    (define/public (step1)
      (for ((p players))
        (give-cards-to p (send p how-many-cards-needed)))
      (all-players-communicate-externally (lambda (p) (send p start watering-hole)) "start [~e]"))
    
    ;; -> Void
    ;; EFFECT move player's food on species boards to bags
    (define/public (move-food-to-bags)
      (for ((p players))
        (send p move-food-to-bag)))
    
    ;; -> Void
    ;; EFFECT reduce population to food for all of all player's species 
    (define/private (reduce-population-to-food)
      (for ((p players))
        (send p reduce-population-to-food (lambda () (give-cards-to p EXTINCTION-CARDS)))))
    
    ;; -> Step4
    ;; ask players to choose their actions
    ;; ASSUME (board-for-all) and (give-cards ...) replenished all players boards and cards 
    (define/private (step2-3)
      (all-players-communicate-externally (lambda (p) (send p choose players)) "choose ~e"))
    
    ;; Step4 -> Void 
    ;; execute step 4 of the Evolution turn
    ;; ASSUME: ;; (= (length players) (length card-actions))
    ;; the actions are specified in the same order as players
    (define/public (step4 actions*)
      (for ((p players) (per-player actions*))
        (define added-food (send p apply-card-action per-player))
        (fill-up-watering-hole added-food))
      (feeding))
    
    ;; Card -> Void 
    (define/private (fill-up-watering-hole fc)
      (set! watering-hole (max 0 (+ watering-hole (card-food-points fc)))))
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (feeding)
      (auto-birth-fertiles)
      (auto-feed-long-necks)
      (auto-move-fat-food)
      ;; players* represents a cicular queue of the players that may still need food
      (let feeding ([players* players])
        (when (and (> watering-hole 0) (cons? players*))
          (feeding (feed1 players*)))))
    
    ;; -> Void
    (define/private (auto-birth-fertiles)
      (act-on-all-from (first players) fertile? (lambda (p s) (send p population+1 s))))
    
    ;; -> Void
    (define/private (auto-feed-long-necks)
      (act-on-all-from (first players) long-neck? (lambda (p s) (feed-a-player-s-species p s))))
    
    ;; -> Void
    (define/private (auto-move-fat-food)
      (act-on-all-from (first players) fat-tissue? (lambda (p s) (send p move-fat s))))
    
    ;; {[Listof Player]} -> [Listof Player]
    ;; players* represents a cicular queue of the players that may still need food
    ;; ASSUME watering-hole is not empty, there is a feedable player
    (define/public (feed1 (players* players))
      (define current-player (first players*))
      (define in-attackable-order (players-in-order current-player #:last #true))
      (define choice (send current-player next watering-hole in-attackable-order))
      (communicate-externally choice
                              (get-field id current-player)
                              "feed-next ~e"
                              (lambda (next) (send next interpret this players* in-attackable-order))
                              (lambda () (rest players*))))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; the below is the API to NEXT for the interpret method 
    
    (define/public (give-cards-to player n0)
      (define n (min (length cards) n0))
      (send player take-cards (take cards n))
      (set! cards (drop cards n)))
    
    (define/public (store-fat current-player s n)
      (define to-be-transfered (min n watering-hole))
      (set! watering-hole (- watering-hole to-be-transfered))
      (send current-player store-fat s to-be-transfered))
    
    (define/public (feed-scavengers current-player)
      (act-on-all-from current-player scavenger? (lambda (p s) (feed-a-player-s-species p s))))
    
    ;; generative recursion: the following three methods implement the rules of feeding a species
    (define/public (feed-a-player-s-species player s)
      ;; TERMINATION
      ;; neighbor-of-s > s or (not neighbor-of-s)
      ;; so a recursive call from cooperate will be a larger species index
      ;; but there's only a finite number of species (with index) per player,
      ;; hence neighbor-of-s will eventually be #false & there will be no more recursion
      (define-values (ok? foraging? neighbor-of-s) (feed-if-possible player s))
      (when ok? 
        (when foraging?
          (define-values (ok? _1 _2) (feed-if-possible player s))
          (when ok? 
            (cooperate player neighbor-of-s)))
        (cooperate player neighbor-of-s)))
    
    ;; Player [Maybe N] -> Void
    ;; hand s the right to eat if it points to a species 
    (define/private (cooperate player s)
      (when s
        (feed-a-player-s-species player s)))
    
    ;; Player N -> (values Boolean Boolean [Maybe N])
    ;; feed species s of player from this dealer's waterhing hole
    ;; if there is food and if s points to a species 
    (define/private (feed-if-possible player s)
      (cond
        ;; scavenging, foraging, and cooperation must not overfeed a species
        [(or (= watering-hole 0) (send player all-fed s)) (values #f #f #f)]
        [else
         (set! watering-hole (- watering-hole 1))
         (define-values (foraging? neighbor-of-s) (send player feed1 s))
         (values #true foraging? neighbor-of-s)]))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; [Player -> X] FormatString[1] -> [Listof X]
    ;; apply all players to action and collect results 
    ;; EFFECT validate action's result and throw players out that cheat
    (define/private (all-players-communicate-externally action fmt)
      (reverse
       (for/fold ((result '())) ((p players))
         (define a (action p))
         (define i (get-field id p))
         (communicate-externally a i fmt (lambda (choice) (cons choice result)) (lambda () result)))))
    
    ;; [Maybe X] Player FormatString [-> Y]  [X -> Y] -> Y
    ;; run (thn choice) if choice holds, otherwise (els)
    ;; EFFECT: remove player p if (client-error? choice) holds 
    (define/private (communicate-externally choice id fmt thn els)
      (cond
        [choice (thn choice)]
        [else
         (log-info (string-append "dealer says: player ~a failed on " fmt) id choice)
         (set! players (remf (lambda (p) (equal? (get-field id p) id)) players))
         (els)]))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Player [Any -> Boolean] [Player N -> Void] -> Void
    ;; act on all player's species where species satisfies p?
    (define/private (act-on-all-from current-player p? action)
      (for ((p (players-in-order current-player)))
        (send p find-all p? action)))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Player -> [Listof Player]
    ;; find current-player id in players, take rest and append front-end
    ;; the keyword argument #:last places the current player
    (define/private (players-in-order current-player #:last (last? #false))
      (define id (get-field id current-player))
      (let loop ([players players][front-end '()])
        (define fst (first players))
        (if (equal? (get-field id fst) id)
            (if last?
                (append (rest players) (reverse front-end))
                (append players (reverse front-end)))
            (loop (rest players) (cons (first players) front-end)))))))

;; ===================================================================================================
;; the testing DSL 
(module+ test
  ;; (check-scenario ...) describes the state of a dealer before and after (testing dealer) is called
  ;; it synthezies rackunit tests that check the expected changes as well as the frame conditions 
  (define-syntax (check-scenario stx)
    (syntax-parse stx
      [(check-feed1 #:doc msg
                    ;; create players for this test case:
                    #:before
                    [p:id (~optional [#:cards card+] #:defaults ([card+ #''()])) sp ... ] ...
                    (~optional (~seq #:cards dealer-cards) #:defaults ([dealer-cards #''()]))
                    ;; the food in the watering hole before (testing dealer)
                    #:pre  pre:number
                    ;; the food after a feed1 step
                    #:post post:number 
                    ;; those players that changed, all others are tested for differences:
                    ;; _ means the species renains the same
                    ;; - means the species went extinct
                    #:after
                    [q:id (~optional [#:cards cards] #:defaults ([cards #''()]))
                          (~optional [#:bag bag] #:defaults ([bag #'0]))
                          sq ... ] ...)
       ;;
       ;; creates code to: 
       ;; -- define functions named p ... that create players from species sp ...,
       ;;    (thawed if sp is just an id)
       ;; -- creates instances of these players and runs check-plain on them
       ;; -- compares those specified in ((q sq ...) ...) to make sure they changed as desired
       ;; -- makes sure the others remain the same (frame conditions)
       ;; ------------------------------------------------------------------------------------
       (let* ([players  (syntax->list #'((p sp ...) ...))]
              [names    (syntax->list #'(p ...))]
              [ids      (for/list ([_p names][i (in-naturals)]) (+ i 1))]
              [modified (syntax->list #`((q bag cards sq ...) ...))]
              [names-for-thunked-players (generate-temporaries names)])
         (with-syntax ([(p-id ...) ids]
                       [(player-of ...) names-for-thunked-players]
                       [((tp ...) ...)
                        (for*/list ([s (syntax->list #'((sp ...) ...))])
                          (for/list ([t (syntax->list s)])
                            (id->app t)))])
           #`(let ([player-of
                    (lambda ()
                      (player p-id #:cards card+ #:external create-external #:species (lazy tp ...)))]
                   ...)
               (log-info msg)
               (let #,(for/list ([p names] [pre names-for-thunked-players])
                        #`[#,p (#,pre)])
                 (check-plain msg dealer-cards pre post p ...)
                 #,@(generate-checks players ids names-for-thunked-players modified #'msg)))))]))
  
  ;; make distinct species for external player and internal player 
  (define-syntax-rule (lazy s ...) (lambda () (list s ...)))
  
  ;; String N N Player *-> Void
  ;; (check-plain msg pre post player ... player) tests that a dealer w/o cards feeds a player
  ;; and that the feeding reduces the food tokens at the watering hole as expected
  (define (check-plain msg cards pre post . players)
    (define dealer0 (dealer #:players players #:cards cards #:watering pre))
    (define pre-dealer-json (send dealer0 to-json))
    (with-handlers ([exn:fail:contract? (lambda (x) (debug `("contract:" ,msg)) (raise x))])
      (check-equal? (begin [(testing) dealer0] (get-field watering-hole dealer0)) post msg))
    (define pst-dealer-json (send dealer0 to-json))
    ;; --- let's write them out
    (write-test-case pre-dealer-json pst-dealer-json)
    (cond
      [(set-add-test-in-hook) => add-test-input]
      [else (void)]))
  
  (define set-add-test-in-hook (make-parameter #false))
  
  ;; PlayerSpec =  #'[Id {Id | (Id Expr ...)}]
  
  ;; [Listof PlayerSpec] [Listof N] [Listof #'Id] [Listof PlayerSpec] -> Expr
  (define-for-syntax (generate-checks players ids names-of-thunks changed-players msg)
    (for/list ([p players] [id ids] [pre names-of-thunks])
      (syntax-case p ()
        [(p sp ...)
         (let* ([ekwal? (lambda (p q) (free-identifier=? p (car (syntax->list q))))]
                [the-q  (member #'p changed-players ekwal?)])
           (if (not the-q)
               #`(check-equal? p (#,pre) (format "~a (frame condition ~a)" #,msg #,id))
               (syntax-case (car the-q) ()
                 [(q bag cards sq ...)
                  (let ([sp-list (syntax->list (find #'q players))]
                        [sq-list (syntax->list #'(sq ...))])
                    #;
                    (unless (= (length sp-list) (length sq-list)) ;; doesn't work with cards ???
                      (displayln `(,sp-list ,sq-list))
                      (raise-syntax-error #f "unequal length"))
                    (define species #`(lazy #,@(expected-species sp-list sq-list)))
                    (define the-plr
                      #`(player #,id #:bag bag
                                #:external create-external
                                #:cards cards
                                #:species #,species))
                    (define chk-msg
                      #`(format "~a (difference in player ~a)\nactual: ~a\nexpect: ~a\n"
                                #,msg
                                #,id
                                (send q to-json)
                                (send #,the-plr to-json)))
                    #`(check-equal? q #,the-plr #,chk-msg))])))])))
  
  ;; [Listof Expr] [Listof (U - _ Expr)] -> [Listof Expr]
  ;; construct the expected list of species 
  (define-for-syntax (expected-species sp-list sq-list)
    (let loop ([sp-list sp-list][sq-list sq-list])
      (cond
        [(null? sq-list) '()]
        [(null? sp-list) (map id->app sq-list)]
        [else
         (syntax-parse (car sq-list) #:literals (_ -)
           [_    (cons (id->app (car sp-list)) (loop (cdr sp-list) (cdr sq-list)))]
           [-    (loop (cdr sp-list) (cdr sq-list))]
           [sq   (cons (id->app #'sq) (loop (cdr sp-list) (cdr sq-list)))])])))
  
  
  ;; #'Id [Listof PlayerSpec] -> PlayerSpec 
  (define-for-syntax (find id name+species*)
    (let/ec return
      (for/and ((name+species name+species*))
        (syntax-case name+species ()
          [(p sp ...) (if (free-identifier=? #'p id) (return #'(sp ...)) #t)]))
      (raise-syntax-error #f (string-append "not found: " (symbol->string (syntax-e id))))))
  
  ;; (U Id App) -> App 
  (define-for-syntax (id->app t)
    (if (identifier? t) #`(#,t) t)))

;; ---------------------------------------------------------------------------------------------------
;; the actual tests: 
(module+ test 
  ;; -------------------------------------------------------------------------------------------------
  ;; testing the feed1 step
  (testing (lambda (dealer0) (send dealer0 feed1)))
  (write-out-tests #f)
  
  (log-info "testing feed1")
  
  ;; -------------------------------------------------------------------------------------------------
  ;; some species and players 
  
  (define (s-vegetarian-pop=1 f) (species #:body 3 #:food f #:population 1 #:traits '()))
  (define (s-satisfied (with '()) (f 1)) (species #:body 3 #:food f #:population 1 #:traits with))
  (define (s-hungry-carnivore (f 0))
    (species #:body 3 #:food f #:population 1 #:traits `(,carnivore)))
  (define (s-climbing g) (species #:body 3 #:food g #:population 1 #:traits `(,climbing)))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; this test must fail but doesn't because the player is called even though there's no food
  #;
  (check-scenario #:doc "no food, no feeding happens"
                  #:before
                  [p1 (s-vegetarian-pop=1 0) s-satisfied s-hungry-carnivore]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 0
                  #:post 0
                  #:after)
  
  (check-scenario #:doc "cannot attack anything but myself and won't"
                  #:before [p1 s-hungry-carnivore s-satisfied] [p2 (s-climbing 0)] [p3 ]
                  #:pre  1 
                  #:post 1
                  #:after)
  
  ;; -------------------------------------------------------------------------------------------------
  (define (species-with-fat-trait f)
    (species #:fat-food f #:body 2 #:population 1 #:traits `(,fat-tissue)))
  
  (check-scenario #:doc "fat tissue with sufficient food"  
                  #:before
                  [p1 (species-with-fat-trait 0)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre  2 
                  #:post 0
                  #:after [p1 (species-with-fat-trait 2)])
  
  (check-scenario #:doc "fat tissue with insufficient food"  
                  #:before
                  [p1 (species-with-fat-trait 0)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre  1
                  #:post 0
                  #:after [p1 (species-with-fat-trait 1)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "feed the first hungry vegetarian" 
                  #:before
                  [p1 (s-vegetarian-pop=1 0) s-satisfied s-hungry-carnivore]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 1
                  #:post 0
                  #:after [p1 (s-vegetarian-pop=1 1) _ _])
  
  (check-scenario #:doc "feed the first hungry vegetarian again" 
                  #:before
                  [p1 s-satisfied (s-vegetarian-pop=1 0) s-hungry-carnivore]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 1
                  #:post 0
                  #:after [p1 _ (s-vegetarian-pop=1 1) _])
  
  (check-scenario #:doc "feed the one hungry carnivore and kill of a species" 
                  #:before
                  [p1 s-satisfied (s-vegetarian-pop=1 1) s-hungry-carnivore]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 1
                  #:post 0
                  #:after
                  [p1 _ _ (s-hungry-carnivore 1)]
                  [p3 -])
  
  (check-scenario #:doc "no hungry species, remove player & move on"
                  #:before
                  [p1 s-satisfied]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied (s-vegetarian-pop=1 1) s-hungry-carnivore]
                  #:pre 1
                  #:post 1
                  #:after)
  
  ;; -------------------------------------------------------------------------------------------------
  (define (s-food f) (species #:food f #:population 1))
  (define (s-cooperating-veg (f 0)) (species #:food f #:population 1 #:traits `(,cooperation)))
  
  (check-scenario #:doc "feed cooperating vegetarian with hungry neighbor"
                  #:before
                  [p1 s-cooperating-veg (s-food 0)]
                  [p2 s-cooperating-veg (s-food 1)]
                  [p3 s-cooperating-veg (s-food 1)]
                  #:pre 2
                  #:post 0
                  #:after
                  [p1 (s-cooperating-veg 1) (s-food 1)])
  
  ;; -------------------------------------------------------------------------------------------------
  (define (s-foraging-veg (f 0) (w '())) (species #:food f #:population 5 #:traits `(,foraging ,@w)))
  
  (check-scenario #:doc "feed vegetarian with foraging trait 1 -> 0"
                  #:before
                  [p1 (s-foraging-veg)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 1
                  #:post 0
                  #:after
                  [p1 (s-foraging-veg 1)])
  
  (check-scenario #:doc "feed vegetarian with foraging trait 2 -> 0"
                  #:before
                  [p1 (s-foraging-veg)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 2
                  #:post 0
                  #:after
                  [p1 (s-foraging-veg 2)])
  
  (define (s+cooperating (f 0)) (s-foraging-veg f `(,cooperation)))
  (define (s-vegegtarian-pop=3 (f 0)) (species #:food f #:population 3 #:traits `(,long-neck)))
  
  (check-scenario #:doc "feed cooperating and foraging vegetarian 5 -> 1"
                  #:before
                  [p1 s+cooperating s-vegegtarian-pop=3]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 5
                  #:post 1
                  #:after
                  [p1 (s+cooperating 2) (s-vegegtarian-pop=3 2)])
  
  (check-scenario #:doc "feed cooperating and foraging vegetarian 4 -> 0"
                  #:before
                  [p1 s+cooperating s-vegegtarian-pop=3]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 4
                  #:post 0
                  #:after
                  [p1 (s+cooperating 2) (s-vegegtarian-pop=3 2)])
  
  (check-scenario #:doc "feed cooperating and foraging vegetarian 3 -> 0"
                  #:before
                  [p1 s+cooperating s-vegegtarian-pop=3]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 3
                  #:post 0
                  #:after
                  [p1 (s+cooperating 2) (s-vegegtarian-pop=3 1)])
  
  ;; -------------------------------------------------------------------------------------------------
  ;; s1: veg eats one, gets another one for foraging
  ;; s2: neighbor receives 2 for cooperating s1, takes 2 for foraging
  ;; s3: s2 tries to hand 4 to s3, but it can take only 3 because capacity runs out
  
  (check-scenario #:doc "feed foraging vegetarian with cooperation chain 12 -> 3"
                  #:before
                  [p1 (s+cooperating) (s+cooperating) (s-vegegtarian-pop=3)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 12
                  #:post 3
                  #:after
                  [p1 (s+cooperating 2) (s+cooperating 4) (s-vegegtarian-pop=3 3)])
  
  (check-scenario #:doc "feed foraging vegetarian with cooperation chain 10 -> 1"
                  #:before
                  [p1 (s+cooperating) (s+cooperating) (s-vegegtarian-pop=3)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 10
                  #:post 1
                  #:after
                  [p1 (s+cooperating 2) (s+cooperating 4) (s-vegegtarian-pop=3 3)])
  
  (check-scenario #:doc "feed foraging vegetarian with cooperation chain 8 -> 0"
                  #:before
                  [p1 (s+cooperating) (s+cooperating) (s-vegegtarian-pop=3)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 8
                  #:post 0
                  #:after
                  [p1 (s+cooperating 2) (s+cooperating 4) (s-vegegtarian-pop=3 2)])
  
  ;; -------------------------------------------------------------------------------------------------
  (define 2cards (take all-cards 2))
  (define 2+cards (take (drop all-cards 2) 2))
  (define 4cards (append 2cards 2+cards))
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "attacker will attack attackee & kill the latter's only species"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-vegetarian-pop=1 0) (s-satisfied) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 1
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [attackee (#:cards 2cards) -])
  (define (s-horned) (species #:food 0 #:population 1 #:traits `(,horns)))
  
  (check-scenario #:doc "attacker will attack attackee and commit suicide, no feeding"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-horned]
                  [by-stand ]
                  #:cards 4cards
                  #:pre 1
                  #:post 1
                  #:after
                  [attacker (#:cards 2+cards) - (s-satisfied)]
                  [attackee (#:cards 2cards) -])
  
  (check-scenario #:doc "attacker will attack attackee and commit suicide, no feeding, no cards"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-horned]
                  [by-stand ]
                  #:cards 2cards
                  #:pre 1
                  #:post 1
                  #:after
                  [attacker - (s-satisfied)]
                  [attackee (#:cards 2cards) -])
  
  (define (s-2satisfied (f 2))
    (species #:food f #:population f))
  
  (check-scenario #:doc "attackee's population is reduced below the food size"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-2satisfied]
                  [by-stand ]
                  #:cards 4cards
                  #:pre 1
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) s-satisfied]
                  [attackee (s-2satisfied 1)])
  
  ;; -------------------------------------------------------------------------------------------------
  (define (s-scavenger-with p (f 0) (t '()))
    (species #:body 0 #:food f #:population p #:traits `(,scavenger ,@t)))
  
  (check-scenario #:doc "feed a scavenger"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 3
                  #:post 1
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 1) (s-hungry-carnivore)])
  
  (check-scenario #:doc "feed a scavenger 2 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 2
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 1) (s-hungry-carnivore)])
  
  (check-scenario #:doc "feed a scavenger 1 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 1
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 0) (s-hungry-carnivore)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "foraging scavenger 4 -> 1"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [by-standr s-satisfied]
                  [scavenger (s-scavenger-with 3 0 `(,foraging)) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 4
                  #:post 1
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [scavenger (s-scavenger-with 2 2 `(,foraging)) s-hungry-carnivore])
  
  (check-scenario #:doc "foraging scavenger 3 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [by-standr s-satisfied]
                  [scavenger (s-scavenger-with 3 0 `(,foraging)) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 3
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [scavenger (s-scavenger-with 2 2 `(,foraging)) s-hungry-carnivore])
  
  (check-scenario #:doc "foraging scavenger 3 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [scavenger (s-scavenger-with 1 0 `(,foraging)) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 2
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [attackee (#:cards 2cards) -]
                  [scavenger (s-scavenger-with 1 1 `(,foraging)) s-hungry-carnivore])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "foraging and cooperating scavenger 6 -> 2"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee (s-scavenger-with 4 0 `(,foraging ,cooperation)) s-hungry-carnivore]
                  [by-stand (s-vegetarian-pop=1 0) (s-satisfied) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 6
                  #:post 2
                  #:after
                  [attacker (s-hungry-carnivore 1) s-satisfied]
                  [attackee (s-scavenger-with 3 2 `(,foraging ,cooperation)) (s-hungry-carnivore 1)])
  
  (check-scenario #:doc "foraging and cooperating scavenger 5 -> 1"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee (s-scavenger-with 4 0 `(,foraging ,cooperation)) s-hungry-carnivore]
                  [by-stand (s-vegetarian-pop=1 0) (s-satisfied) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 5
                  #:post 1
                  #:after
                  [attacker (s-hungry-carnivore 1) s-satisfied]
                  [attackee (s-scavenger-with 3 2 `(,foraging ,cooperation)) (s-hungry-carnivore 1)])
  
  (check-scenario #:doc "foraging and cooperating scavenger 3 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee (s-scavenger-with 4 0 `(,foraging ,cooperation)) s-hungry-carnivore]
                  [by-stand (s-vegetarian-pop=1 0) (s-satisfied) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 3
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) s-satisfied]
                  [attackee (s-scavenger-with 3 2 `(,foraging ,cooperation)) s-hungry-carnivore])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "foraging and scavenging carnivore 6 -> 1"
                  #:before
                  [attacker (s-scavenger-with 4 0 `(,foraging ,carnivore)) s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 6
                  #:post 1
                  #:after
                  [attacker (s-scavenger-with 4 4 `(,foraging ,carnivore)) s-satisfied]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 1) (s-hungry-carnivore)])
  
  (check-scenario #:doc "foraging and scavenging carnivore 4 -> 0"
                  #:before
                  [attacker (s-scavenger-with 4 0 `(,foraging ,carnivore)) s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 4
                  #:post 0
                  #:after
                  [attacker (s-scavenger-with 4 4 `(,foraging ,carnivore)) s-satisfied]
                  [attackee (#:cards 2cards) -])
  
  (check-scenario #:doc "foraging and scavenging carnivore 6 -> 2"
                  #:before
                  [attacker (s-scavenger-with 4 0 `(,foraging ,carnivore)) s-satisfied]
                  [attackee s-satisfied]
                  [by-stand s-satisfied]
                  #:cards 2cards
                  #:pre 6
                  #:post 2
                  #:after
                  [attacker (s-scavenger-with 4 4 `(,foraging ,carnivore)) s-satisfied]
                  [attackee (#:cards 2cards) -]
                  [by-stand s-satisfied])
  
  (check-scenario #:doc "foraging, scavenging carnivore 3 0, not evough food to forage"
                  #:before
                  [attacker (s-scavenger-with 1 0 `(,foraging ,carnivore)) s-hungry-carnivore]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 3
                  #:post 0
                  #:after
                  [attacker (s-scavenger-with 1 1 `(,foraging ,carnivore)) (s-hungry-carnivore 1)]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 1) (s-hungry-carnivore)])
  
  ;; -------------------------------------------------------------------------------------------------
  
  (define (s-with (f 0) (p 1) (with '()))
    (species #:food f #:population p #:traits with))
  
  (check-scenario #:doc "foraging fails, cooperation should fail"  
                  #:before
                  [p1 (s-with 1 2 `(,cooperation ,foraging)) (s-with 0 2)]
                  [p2 s-satisfied]
                  [p3 s-satisfied]
                  #:pre  3
                  #:post 1
                  #:after
                  [p1 (s-with 2 2 `(,cooperation ,foraging)) (s-with 1 2)])
  
  ;; -------------------------------------------------------------------------------------------------
  
  (define (s-plain f) (species #:body 3 #:food f #:population 3))
  (define (s-carnivore-with p with (f 0))
    (species #:body 3 #:food f #:population p #:traits `(,carnivore ,@with)))
  
  (check-scenario #:doc "allison's test"
                  #:before
                  [attacker (s-carnivore-with 5 `(,cooperation))
                            (s-carnivore-with 3 `(,cooperation))
                            (s-carnivore-with 3 `(,cooperation))
                            (s-carnivore-with 2 '())
                            (s-carnivore-with 2 '())]
                  [attackee (s-carnivore-with 4 `(,scavenger ,cooperation))
                            (s-plain 0)
                            (s-plain 0)]
                  [by-stander (s-plain 2) (s-plain 2)]
                  #:pre 15 
                  #:post 9
                  #:after
                  [attacker
                   ;; eats because it attacks, hands right to eat to neighbor on right (below)
                   (s-carnivore-with 5 `(,cooperation) 1) 
                   ;; eats because it received the rights, hands right to eat to neighbor 
                   (s-carnivore-with 3 `(,cooperation) 1)
                   ;; eats because it received the rights, hands right to eat to neighbor 
                   (s-carnivore-with 3 `(,cooperation) 1)
                   ;; eats because it received the rights
                   (s-carnivore-with 2 '() 1)
                   _] ;; last species remains same 
                  [attackee
                   ;; eats because it scavenges, hands right to eat to neighbor on right (below)
                   (s-carnivore-with 3 `(,scavenger ,cooperation) 1)
                   ;; eats because it scavenges 
                   (s-plain 1)
                   (s-plain 0)]))

(module+ test 
  ;; -------------------------------------------------------------------------------------------------
  ;; testing the entire feeding step
  (testing (lambda (dealer0) (send dealer0 feeding)))
  
  (log-info "testing feeding")
  
  ;; ****************************************
  ;; DON'T EVER TURN THOSE ON FOR A TEST FEST
  (write-out-tests #f)
  ;; ****************************************
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF no food, no feeding happens"
                  #:before
                  [p1 (s-vegetarian-pop=1 0) s-satisfied s-hungry-carnivore]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 0
                  #:post 0
                  #:after)
  
  (check-scenario #:doc "FF cannot attack anything but myself and won't"
                  #:before [p1 s-hungry-carnivore s-satisfied] [p2 (s-climbing 0)] [p3 ]
                  #:pre  1 
                  #:post 0
                  #:after [p2 (s-climbing 1)])
  
  (define (s-fertile (p 1)) (species #:population p #:traits `(,fertile)))
  
  (check-scenario #:doc "cannot attack because fertiles ate first"
                  #:before
                  [p1 s-hungry-carnivore s-satisfied]
                  [p2 (s-climbing 0) s-fertile]
                  [p3 s-fertile]
                  #:pre  0
                  #:post 0
                  #:after
                  [p2 (s-climbing 0) (s-fertile 2)]
                  [p3 (s-fertile 2)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF fat tissue with sufficient food"  
                  #:before
                  [p1 (species-with-fat-trait 0)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre  2 
                  #:post 0
                  #:after [p1 (species-with-fat-trait 2)])
  
  (check-scenario #:doc "FF fat tissue with insufficient food"  
                  #:before
                  [p1 (species-with-fat-trait 0)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre  1
                  #:post 0
                  #:after [p1 (species-with-fat-trait 1)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF feed the first hungry vegetarian" 
                  #:before
                  [p1 (s-vegetarian-pop=1 0) s-satisfied s-hungry-carnivore]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 1
                  #:post 0
                  #:after [p1 (s-vegetarian-pop=1 1) _ _])
  
  (check-scenario #:doc "FF feed the first hungry vegetarian again" 
                  #:before
                  [p1 s-satisfied (s-vegetarian-pop=1 0) s-hungry-carnivore]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 1
                  #:post 0
                  #:after [p1 _ (s-vegetarian-pop=1 1) _])
  
  (check-scenario #:doc "FF feed the one hungry carnivore and kill of a species" 
                  #:before
                  [p1 s-satisfied (s-vegetarian-pop=1 1) s-hungry-carnivore]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 1
                  #:post 0
                  #:after
                  [p1 _ _ (s-hungry-carnivore 1)]
                  [p3 -])
  
  (check-scenario #:doc "FF no hungry species, remove player & move on"
                  #:before
                  [p1 s-satisfied]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied (s-vegetarian-pop=1 1) s-hungry-carnivore]
                  #:pre 1
                  #:post 0
                  #:after
                  [p2 (s-vegetarian-pop=1 1) (s-climbing 0)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF feed cooperating vegetarian with hungry neighbor"
                  #:before
                  [p1 s-cooperating-veg (s-food 0)]
                  [p2 s-cooperating-veg (s-food 1)]
                  [p3 s-cooperating-veg (s-food 1)]
                  #:pre 2
                  #:post 0
                  #:after
                  [p1 (s-cooperating-veg 1) (s-food 1)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF feed vegetarian with foraging trait 1 -> 0"
                  #:before
                  [p1 (s-foraging-veg)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 1
                  #:post 0
                  #:after
                  [p1 (s-foraging-veg 1)])
  
  (check-scenario #:doc "FF feed vegetarian with foraging trait 2 -> 0"
                  #:before
                  [p1 (s-foraging-veg)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 2
                  #:post 0
                  #:after
                  [p1 (s-foraging-veg 2)])
  
  (check-scenario #:doc "FF feed cooperating and foraging vegetarian 5 -> 0"
                  #:before
                  [p1 s+cooperating s-vegegtarian-pop=3]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 5
                  #:post 0
                  #:after
                  [p1 (s+cooperating 2) (s-vegegtarian-pop=3 3)])
  
  (check-scenario #:doc "FF feed cooperating and foraging vegetarian 4 -> 0"
                  #:before
                  [p1 s+cooperating s-vegegtarian-pop=3]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 4
                  #:post 0
                  #:after
                  [p1 (s+cooperating 2) (s-vegegtarian-pop=3 2)])
  
  (check-scenario #:doc "FF feed cooperating and foraging vegetarian 3 -> 0"
                  #:before
                  [p1 s+cooperating s-vegegtarian-pop=3]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 3
                  #:post 0
                  #:after
                  [p1 (s+cooperating 2) (s-vegegtarian-pop=3 1)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF feed foraging vegetarian with cooperation chain 12 -> 0"
                  #:before
                  [p1 (s+cooperating) (s+cooperating) (s-vegegtarian-pop=3)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 12
                  #:post 0
                  #:after
                  [p1 (s+cooperating 2) (s+cooperating 5) (s-vegegtarian-pop=3 3)]
                  [p2 (s-vegetarian-pop=1 1) (s-climbing 1)])
  
  (check-scenario #:doc "FF feed foraging vegetarian with cooperation chain 10 -> 1"
                  #:before
                  [p1 (s+cooperating) (s+cooperating) (s-vegegtarian-pop=3)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 10
                  #:post 0
                  #:after
                  [p1 (s+cooperating 2) (s+cooperating 4) (s-vegegtarian-pop=3 3)]
                  [p2 (s-vegetarian-pop=1 1) (s-climbing 0)])
  
  (check-scenario #:doc "FF feed foraging vegetarian with cooperation chain 8 -> 0"
                  #:before
                  [p1 (s+cooperating) (s+cooperating) (s-vegegtarian-pop=3)]
                  [p2 (s-vegetarian-pop=1 0) (s-climbing 0)]
                  [p3 s-satisfied]
                  #:pre 8
                  #:post 0
                  #:after
                  ;; this differs from the original because pop=3 includes a long-neck
                  [p1 (s+cooperating 2) (s+cooperating 3) (s-vegegtarian-pop=3 3)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF attacker will attack attackee & kill the latter's only species"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-vegetarian-pop=1 0) (s-satisfied) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 1
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [attackee (#:cards 2cards) -])
  
  (check-scenario #:doc "FF attacker will attack attackee and commit suicide, no feeding"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-horned]
                  [by-stand ]
                  #:cards 4cards
                  #:pre 1
                  #:post 1
                  #:after
                  [attacker (#:cards 2+cards) - (s-satisfied)]
                  [attackee (#:cards 2cards) -])
  
  (check-scenario #:doc "FF attacker will attack attackee and commit suicide, no feeding, no cards"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-horned]
                  [by-stand ]
                  #:cards 2cards
                  #:pre 1
                  #:post 1
                  #:after
                  [attacker - (s-satisfied)]
                  [attackee (#:cards 2cards) -])
  
  (check-scenario #:doc "FF attackee's population is reduced below the food size"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-2satisfied]
                  [by-stand ]
                  #:cards 4cards
                  #:pre 1
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) s-satisfied]
                  [attackee (s-2satisfied 1)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF feed a scavenger"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 3
                  #:post 0
                  #:after
                  [attacker [#:cards 2+cards] (s-hungry-carnivore 1)]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 1) (s-hungry-carnivore 1)])
  
  (check-scenario #:doc "FF feed a scavenger 2 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 2
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 1) (s-hungry-carnivore)])
  
  (check-scenario #:doc "FF feed a scavenger 1 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 1
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 0) (s-hungry-carnivore)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF foraging scavenger 4 -> 1"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [by-standr s-satisfied]
                  [scavenger (s-scavenger-with 3 0 `(,foraging)) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 4
                  #:post 0
                  #:after
                  [attacker  [#:cards 2cards] (s-hungry-carnivore 1) -]
                  [scavenger (s-scavenger-with 2 2 `(,foraging)) (s-hungry-carnivore 1)])
  
  (check-scenario #:doc "FF foraging scavenger 3 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [by-standr s-satisfied]
                  [scavenger (s-scavenger-with 3 0 `(,foraging)) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 3
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [scavenger (s-scavenger-with 2 2 `(,foraging)) s-hungry-carnivore])
  
  (check-scenario #:doc "FF foraging scavenger 3 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee s-satisfied]
                  [scavenger (s-scavenger-with 1 0 `(,foraging)) s-hungry-carnivore]
                  #:cards 4cards
                  #:pre 2
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) (s-satisfied)]
                  [attackee (#:cards 2cards) -]
                  [scavenger (s-scavenger-with 1 1 `(,foraging)) s-hungry-carnivore])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF foraging and cooperating scavenger 6 -> 2"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee (s-scavenger-with 4 0 `(,foraging ,cooperation)) s-hungry-carnivore]
                  [by-stand (s-vegetarian-pop=1 0) (s-satisfied) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 6
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) s-satisfied]
                  [attackee (s-scavenger-with 3 3 `(,foraging ,cooperation)) (s-hungry-carnivore 1)]
                  [by-stand (s-vegetarian-pop=1 1) (s-satisfied) s-hungry-carnivore])
  
  (check-scenario #:doc "FF foraging and cooperating scavenger 5 -> 1"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee (s-scavenger-with 4 0 `(,foraging ,cooperation)) s-hungry-carnivore]
                  [by-stand (s-vegetarian-pop=1 0) (s-satisfied) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 5
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) s-satisfied]
                  [attackee (s-scavenger-with 3 3 `(,foraging ,cooperation)) (s-hungry-carnivore 1)])
  
  (check-scenario #:doc "FF foraging and cooperating scavenger 3 -> 0"
                  #:before
                  [attacker s-hungry-carnivore s-satisfied]
                  [attackee (s-scavenger-with 4 0 `(,foraging ,cooperation)) s-hungry-carnivore]
                  [by-stand (s-vegetarian-pop=1 0) (s-satisfied) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 3
                  #:post 0
                  #:after
                  [attacker (s-hungry-carnivore 1) s-satisfied]
                  [attackee (s-scavenger-with 3 2 `(,foraging ,cooperation)) s-hungry-carnivore])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF foraging and scavenging carnivore 6 -> 1"
                  #:before
                  [attacker (s-scavenger-with 4 0 `(,foraging ,carnivore)) s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 6
                  #:post 0
                  #:after
                  [attacker (s-scavenger-with 3 3 `(,foraging ,carnivore)) s-satisfied]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 1) (s-hungry-carnivore 1)])
  
  (check-scenario #:doc "FF foraging and scavenging carnivore 4 -> 0"
                  #:before
                  [attacker (s-scavenger-with 4 0 `(,foraging ,carnivore)) s-satisfied]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 4
                  #:post 0
                  #:after
                  [attacker (s-scavenger-with 4 4 `(,foraging ,carnivore)) s-satisfied]
                  [attackee (#:cards 2cards) -])
  
  (check-scenario #:doc "FF foraging and scavenging carnivore 6 -> 2"
                  #:before
                  [attacker (s-scavenger-with 4 0 `(,foraging ,carnivore)) s-satisfied]
                  [attackee s-satisfied]
                  [by-stand s-satisfied]
                  #:cards 2cards
                  #:pre 6
                  #:post 2
                  #:after
                  [attacker (s-scavenger-with 4 4 `(,foraging ,carnivore)) s-satisfied]
                  [attackee (#:cards 2cards) -]
                  [by-stand s-satisfied])
  
  (check-scenario #:doc "FF foraging, scavenging carnivore 3 0, not evough food to forage"
                  #:before
                  [attacker (s-scavenger-with 1 0 `(,foraging ,carnivore)) s-hungry-carnivore]
                  [attackee s-satisfied]
                  [by-stand (s-scavenger-with 1 0) (s-hungry-carnivore)]
                  #:cards 2cards
                  #:pre 3
                  #:post 0
                  #:after
                  [attacker (s-scavenger-with 1 1 `(,foraging ,carnivore)) (s-hungry-carnivore 1)]
                  [attackee (#:cards 2cards) -]
                  [by-stand (s-scavenger-with 1 1) (s-hungry-carnivore)])
  
  ;; -------------------------------------------------------------------------------------------------
  (check-scenario #:doc "FF foraging fails, cooperation should fail"  
                  #:before
                  [p1 (s-with 1 2 `(,cooperation ,foraging)) (s-with 0 2)]
                  [p2 s-satisfied]
                  [p3 s-satisfied]
                  #:pre  3
                  #:post 0
                  #:after
                  [p1 (s-with 2 2 `(,cooperation ,foraging)) (s-with 2 2)])
  
  ;; -------------------------------------------------------------------------------------------------
  (define (s-plain-f f (p 3)) (species #:body 3 #:food f #:population p))
  
  (check-scenario #:doc "FF allison's test"
                  #:before
                  [attacker (s-carnivore-with 5 `(,cooperation))
                            (s-carnivore-with 3 `(,cooperation))
                            (s-carnivore-with 3 `(,cooperation))
                            (s-carnivore-with 2 '())
                            (s-carnivore-with 2 '())]
                  [attackee (s-carnivore-with 4 `(,scavenger ,cooperation))
                            (s-plain 0)
                            (s-plain 0)]
                  [by-stander (s-plain 2) (s-plain 2)]
                  #:pre 15 
                  #:post 0 
                  #:after
                  [attacker
                   ;; eats because it attacks, hands right to eat to neighbor on right (below)
                   (s-carnivore-with 5 `(,cooperation) 2) 
                   ;; eats because it received the rights, hands right to eat to neighbor 
                   (s-carnivore-with 3 `(,cooperation) 2)
                   ;; eats because it received the rights, hands right to eat to neighbor 
                   (s-carnivore-with 3 `(,cooperation) 2)
                   ;; eats because it received the rights
                   (s-carnivore-with 2 '() 2)
                   _] ;; last species remains same 
                  [attackee
                   ;; eats because it scavenges, hands right to eat to neighbor on right (below)
                   (s-carnivore-with 3 `(,scavenger ,cooperation) 2)
                   ;; eats because it scavenges 
                   (s-plain 3)
                   (s-plain 1)]
                  [by-stander (s-plain-f 2 2) (s-plain 2)])
  
  (check-scenario #:doc "FF carnivore attacks board after player drops out"
                  #:before
                  [attacker (s-carnivore-with 5 '())]
                  [attackee (s-plain 3)]
                  [by-stander]
                  #:pre 3
                  #:post 0 
                  #:after
                  [attacker (s-carnivore-with 5 '() 3)]
                  [attackee -])
  )

(module+ test
  ;; -------------------------------------------------------------------------------------------------
  ;; testing step 4: (testing ...) is set way for each scenario
  (write-out-tests #f)
  
  (log-info "testing step4")
  
  ;; -------------------------------------------------------------------------------------------------
  (define (n-cards n) (filter (lambda (c) (= (card-food-points c) n)) all-cards))
  (define 3+cards (n-cards +3))
  (define 3-cards (n-cards -3))
  (define 0-cards (n-cards 0))
  
  (define (& fc*) (map (lambda (x) '()) fc*))
  (define (step4 fc* #:gp [gp* (& fc*)] #:gb [gb* (& fc*)] #:bt [bc* (& fc*)] #:rt [tr* (& fc*)])
    (define input (map list fc* gp* gb* bc* tr*))
    (testing (lambda (dealer) (send dealer step4 input)))
    (set-add-test-in-hook (map action4->json input)))
  
  (step4 '(0 0 0))
  
  ;; 1
  (check-scenario #:doc "part 1, step 4, only food cards that 0 out"
                  #:before
                  [p1 [#:cards (take 3+cards 4)] s-satisfied]
                  [p2 [#:cards (take 3-cards 4)] s-satisfied]
                  [p3 [#:cards (take 0-cards 4)]]
                  #:pre 0
                  #:post 0
                  #:after
                  [p1 [#:cards (rest (take 3+cards 4))] s-satisfied]
                  [p2 [#:cards (rest (take 3-cards 4))] s-satisfied]
                  [p3 [#:cards (rest (take 0-cards 4))]])
  
  ;; 2
  (check-scenario #:doc "part 1, step 4, only food cards that zero out"
                  #:before
                  [p1 [#:cards (take 3+cards 4)] s-satisfied]
                  [p2 [#:cards (take 3-cards 4)] s-satisfied]
                  [p3 [#:cards (take 0-cards 4)]]
                  #:pre 1
                  #:post 1
                  #:after
                  [p1 [#:cards (rest (take 3+cards 4))] s-satisfied]
                  [p2 [#:cards (rest (take 3-cards 4))] s-satisfied]
                  [p3 [#:cards (rest (take 0-cards 4))]])
  
  
  ;; 3
  (check-scenario #:doc "part 1, step 4, only food cards that add 1"
                  #:before
                  [p1 [#:cards (take 3+cards 4)] s-satisfied]
                  [p2 [#:cards (take 3-cards 4)] s-satisfied]
                  [p3 [#:cards (take (n-cards 1) 3)]]
                  #:pre 1
                  #:post 2
                  #:after
                  [p1 [#:cards (rest (take 3+cards 4))] s-satisfied]
                  [p2 [#:cards (rest (take 3-cards 4))] s-satisfied]
                  [p3 [#:cards (rest (take (n-cards 1) 3))]])
  
  (define (s-growable-pop p f)
    (species #:food f #:population p))
  
  (step4 '[0 0 0] #:gp '[[[0 1][0 2]] [] []])
  
  ;; 4
  (check-scenario #:doc "part 1, step 4, food cards that add 1 and grows pop of 0-th board"
                  #:before
                  [step4-1 [#:cards (take 3+cards 4)] (s-growable-pop 1 1)]
                  [step4-2 [#:cards (take 3-cards 4)] s-satisfied]
                  [step4-3 [#:cards (take (n-cards 1) 3)]]
                  #:cards (n-cards -2)
                  #:pre 1
                  #:post 0
                  #:after
                  [step4-1 (#:cards (cdddr (take 3+cards 4))) (s-growable-pop 3 3)]
                  [step4-2 [#:cards (rest (take 3-cards 4))] s-satisfied]
                  [step4-3 [#:cards (rest (take (n-cards 1) 3))]])
  
  (define (s-growable-body b)
    (species #:body b #:food 1 #:population 1))
  
  (step4 '[0 0 0] #:gb '[[[0 1][0 2]] [] []])
  
  ;; 5
  (check-scenario #:doc "part 1, step 4, food cards that add 1 and grows bodies of 0-th board"
                  #:before
                  [step4-1 [#:cards (take 3+cards 4)] (s-growable-body 1)]
                  [step4-2 [#:cards (take 3-cards 4)] s-satisfied]
                  [step4-3 [#:cards (take (n-cards 1) 3)]]
                  #:cards (n-cards -2)
                  #:pre 1
                  #:post 2
                  #:after
                  [step4-1 (#:cards (cdddr (take 3+cards 4))) (s-growable-body 3)]
                  [step4-2 [#:cards (rest (take 3-cards 4))] s-satisfied]
                  [step4-3 [#:cards (rest (take (n-cards 1) 3))]])
  
  (step4 '(0 0 0) #:bt '(([1 2 3]) () ()))
  (define (s-23)
    (species #:food 1 #:population 1 #:traits (map card-trait (cddr (take 3+cards 4)))))
  
  ;; 6
  (check-scenario #:doc "part 1, step 4, only food cards that add 1 and adds hungry carnivore board"
                  #:before
                  [step4-1 [#:cards (take 3+cards 4)] s-satisfied]
                  [step4-2 [#:cards (take 3-cards 4)] s-satisfied]
                  [step4-3 [#:cards (take (n-cards 1) 3)]]
                  #:cards (n-cards -2)
                  #:pre 1
                  #:post 1
                  #:after
                  [step4-1 (#:cards '()) s-satisfied s-23]
                  [step4-2 [#:cards (append (take (n-cards -2) 2) (rest (take 3-cards 4)))] -]
                  [step4-3 [#:cards (rest (take (n-cards 1) 3))]])
  
  (step4 '(0 0 0) #:rt '(([0 0 1]) () ()))
  
  ;; 7
  (check-scenario #:doc "part 1, step 4, only food cards that add 1 and change traits"
                  #:before
                  [step4-1 [#:cards (take 3+cards 4)] (s-satisfied `(,carnivore))]
                  [step4-2 [#:cards (take 3-cards 4)] s-satisfied]
                  [step4-3 [#:cards (take (n-cards 1) 3)]]
                  #:cards (n-cards -2)
                  #:pre 1
                  #:post 2
                  #:after
                  [step4-1 [#:cards (cddr (take 3+cards 4))]
                           (s-satisfied `(,(card-trait (second (take 3+cards 4)))))]
                  [step4-2 [#:cards (rest (take 3-cards 4))] s-satisfied]
                  [step4-3 [#:cards (rest (take (n-cards 1) 3))]])
  
  (step4 '(0 0 0) #:rt '(([0 1 2]) () ()))
  
  ;; 8
  (check-scenario #:doc "part 1, step 4, only food cards that add 1 and change trait to carnivore"
                  #:before
                  [step4-1 [#:cards (take 3+cards 4)] (s-satisfied `(,ambush ,climbing) 0)]
                  [step4-2 [#:cards (take 3-cards 4)] s-satisfied]
                  [step4-3 [#:cards (take (n-cards 1) 3)]]
                  #:cards (n-cards -2)
                  #:pre 1
                  #:post 1
                  #:after
                  [step4-1 [#:cards (cons (second (take 3+cards 4)) (cdddr (take 3+cards 4)))]
                           (s-satisfied `(,ambush ,(card-trait (third (take 3+cards 4)))))]
                  [step4-2 [#:cards (append (take (n-cards -2) 2) (rest (take 3-cards 4)))] -]
                  [step4-3 [#:cards (rest (take (n-cards 1) 3))]])
  
  (step4 '(0 0 0) #:rt '(([0 1 2] [0 1 3]) [] []))
  
  ;; 9
  (check-scenario #:doc "part 1(4), food cards add 1 & 2x change second trait of s1 -> vegetarian"
                  #:before
                  [step4-1 [#:cards (take 3+cards 4)] (s-satisfied `(,ambush ,climbing) 0)]
                  [step4-2 [#:cards (take 3-cards 4)] s-satisfied]
                  [step4-3 [#:cards (take (n-cards 1) 3)]]
                  #:cards (n-cards -2)
                  #:pre 1
                  #:post 1
                  #:after
                  [step4-1 [#:cards (cons (second (take 3+cards 4)) (cddddr (take 3+cards 4)))]
                           (s-satisfied `(,ambush ,(card-trait (fourth (take 3+cards 4)))))]
                  [step4-2 [#:cards (rest (take 3-cards 4))] s-satisfied]
                  [step4-3 [#:cards (rest (take (n-cards 1) 3))]])
  )

(module+ test
  ;; -------------------------------------------------------------------------------------------------
  ;; testing boards-for-all: (testing ...) is set way for each scenario
  (write-out-tests #f)
  (testing (lambda (dealer) (send dealer boards-for-all)))
  
  (log-info "testing boards for all")
  
  (define (s-with-1pop) (species #:population 1))
  
  (check-scenario #:doc "start of turn: make sure everyone has at least one board"
                  #:before [p1 (s-with-1pop) (s-with-1pop)] [p2 ] [p3 ]
                  #:pre 0
                  #:post 0
                  #:after
                  [p1 (s-with-1pop) (s-with-1pop)] [p2 (s-with-1pop)] [p3 (s-with-1pop)]))

(module+ test
  ;; -------------------------------------------------------------------------------------------------
  ;; testing step1: (testing ...) is set way for each scenario
  (write-out-tests #f)
  (testing (lambda (dealer) (send dealer step1)))
  
  (log-info "testing step1")
  
  (define d0 all-cards)
  (define c1 (take d0 (+ CARD-PER-PLAYER (* CARDS-PER-BOARD 2))))
  (define d1 (drop d0 (+ CARD-PER-PLAYER (* CARDS-PER-BOARD 2))))
  (define c2 (take d1 (+ CARD-PER-PLAYER (* CARDS-PER-BOARD 1))))
  (define d2 (drop d1 (+ CARD-PER-PLAYER (* CARDS-PER-BOARD 1))))
  (define c3 (take d2 (+ CARD-PER-PLAYER (* CARDS-PER-BOARD 1))))
  
  (check-scenario #:doc "start of turn: hand appropriate number of cards to every player"
                  #:before
                  [p1 (s-with-1pop) (s-with-1pop)]
                  [p2 (s-with-1pop)]
                  [p3 (s-with-1pop)]
                  #:cards all-cards
                  #:pre 0
                  #:post 0
                  #:after
                  [p1 [#:cards c1] (s-with-1pop) (s-with-1pop)]
                  [p2 [#:cards c2] (s-with-1pop)]
                  [p3 [#:cards c3] (s-with-1pop)]))

(module+ test
  ;; -------------------------------------------------------------------------------------------------
  ;; testing turn: (testing ...) is set way for each scenario
  (write-out-tests #f)
  (testing (lambda (dealer) (send dealer move-food-to-bags)))
  
  (log-info "testing move-food-to-bag")
  
  (define (s-with-food f)
    (species #:food f #:population 7))
  
  (check-scenario #:doc "end of turn: move food to bags"
                  #:before
                  [p1 (s-with-food 1) (s-with-food 2)]
                  [p2 (s-with-food 3)]
                  [p3 ]
                  #:pre 0
                  #:post 0
                  #:after
                  [p1 [#:bag 3] (s-with-food 0) (s-with-food 0)]
                  [p2 [#:bag 3] (s-with-food 0)]
                  [p3 ]))

(module+ test
  ;; -------------------------------------------------------------------------------------------------
  ;; testing turn: (testing ...) is set way for each scenario
  (write-out-tests #f)
  (testing (lambda (dealer) (send dealer results)))
  
  (log-info "testing results")
  
  (define (s-traits t)
    (species #:food 3 #:population 7 #:traits t))
  
  (define p1 (player 1 #:bag 33 #:species `(,(s-traits `(,carnivore ,ambush)) ,(s-traits `()))))
  (define p2 (player 2 #:bag 55 #:species `(,(s-traits `(,carnivore)) ,(s-traits `()))))
  (define p3 (player 3 #:bag 66))
  (define final-dealer (dealer #:players (list p1 p2 p3)))
  
  (run-write-json-test2 final-dealer '([3 66][2 58][1 37]) "end of game: score players"))

(module+ test
  ;; -------------------------------------------------------------------------------------------------
  ;; testing turn: (testing ...) is set way for each scenario
  (write-out-tests #f)
  (testing (lambda (dealer) (send dealer results)))
  
  (log-info "run many games")
  
  (define (externals n)
    (build-list n (lambda (i) (list (number->string i) (create-external)))))
  
  (send (create-dealer (externals 3)) run-game)
  (send (create-dealer (externals 4)) run-game)
  (send (create-dealer (externals 5)) run-game)
  (send (create-dealer (externals 6)) run-game)
  (send (create-dealer (externals 7)) run-game)
  (send (create-dealer (externals 8)) run-game)
  
  )

(module+ test
  (define (bad-choosers n* no-fc)
    (define (external-bad-chooser n)
      (list (format "~a" n) (create-bad-choose (no-fc n))))
    (define bads (map external-bad-chooser n*))
    (define good
      (build-list (- MAX-PLAYERS (length bads))
                  (lambda (i) (list (number->string i) (create-external)))))
    (send (create-dealer (append bads good)) run-game))
  
  (bad-choosers '(0) no-fc)
  (bad-choosers '(0 1) no-fc)
  (bad-choosers '(0 1 2) no-fc)
  (bad-choosers '(0 1 2 6) no-fc)
  (bad-choosers '(0 1 2 6 7) no-fc)
  (bad-choosers '(0 1 2 6 7 8) no-fc)
  (bad-choosers '(0 1 2 6 7 8 9) no-fc)
  (bad-choosers '(0 1 2 5 6 7 8 9) no-fc)
  
  (bad-choosers '(0) over-growth)
  (bad-choosers '(0 1) over-growth)
  (bad-choosers '(0 1 2) over-growth)
  (bad-choosers '(0 1 2 6) over-growth)
  (bad-choosers '(0 1 2 6 7) over-growth)
  (bad-choosers '(0 1 2 6 7 8) over-growth)
  (bad-choosers '(0 1 2 6 7 8 9) over-growth)
  (bad-choosers '(0 1 2 5 6 7 8 9) over-growth)
  
  )
