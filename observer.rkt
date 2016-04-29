#lang racket

;; =============================================================================
;; a JSON-based observer for Dealer and GUI

(provide
 dealer0
 dealer->image

 observer/c)

;; ===================================================================================================
(require json)

(define observer/c
  (object/c
   [display (->m jsexpr? any)]))

(define TEXT-COLOR 'red)
(define BLANK (square 20 'solid 'white))
(define OFF 10)

;; dealer  = [ [iplayer_1, ..., iplayer_n], food, [card_1, ..., card_n]]
;; food    = natural 
;; iplayer = [["id", N] . bplayer]
;; bplayer = [["species", species_1, ..., species_n], ["bag", N]]
;          | [["species", species_1, ..., species_n], ["bag", N], ["cards", card_1, ..., card_n]]
;; card    = [points, trait]
;; trait   = String

(define (player name b species cards)
  `[["id" ,name] ["species" ,species] ["bag" ,b] ,@(if (cons? cards) `(["cards" ,cards]) '())])

(define food0     12)
(define cards0    (map card->json (drop all-cards 22)))
(define cards1    (map card->json (take all-cards 11)))
(define traits0   (map trait->json `[,ambush ,carnivore ,warning-call]))
(define species0  `[["food" 1] ["body" 2] ["population" 3] ["traits" ,traits0] ["fat-food" 3]])
(define boards0   `[,species0 ,species0 ,species0])
(define player0   (player "good guy plays well" 22 boards0 (map card->json (take all-cards 11))))
(define player1   (player "bad guy plays better" 37 (rest boards0) cards1))
(define player2   (player "... and Lydia and Alex will win" 13 '() cards1))
(define players0  `[,player0 ,player1 ,player2])
(define dealer0   `[ ,players0 ,food0 ,cards0])

;; ===================================================================================================
(require "cards.rkt" "traits.rkt" "basics.rkt" 2htdp/image)

(define (dealer->image j)
  (match j
    [`(,iplayers ,food ,cards)
     (beside/align 'top
                   (above/align 'left
                                (labeled "food supply" food)
                                (cards->image cards))
                   BLANK
                   (iplayers->image iplayers))]))

(define (iplayers->image j)
  (match j
    [`(,iplayer* ...)
     (for/fold ((i empty-image)) ((ip iplayer*))
       (beside/align 'top (frame* (player->image ip)) BLANK i))]))

(define (frame* img)
  (define w (image-width img))
  (define h (image-height img))
  (define f (rectangle (+ OFF w) (+ h OFF) 'outline 'black))
  (overlay f img))

(define (player->image j)
  (match j
    [`[ ["id" ,id] ["species" [,s* ...]] ["bag" ,b] ]
     (above/align 'left
                  (labeled "player" id)
                  (labeled "food points" b)
                  (boards->image s*))]
    [`[ ["id" ,id] ["species" [,s* ...]] ["bag" ,b] ["cards" [,card* ...]]]
     (above/align 'left
                  (labeled "player" id)
                  (labeled "food points" b)
                  (boards->image s*)
                  (cards->image card*))]))

(define (boards->image j)
  (match j
    [`(,species* ...)
     (for/fold ((i empty-image)) ((s species*))
       (beside/align 'top (species->image s) blank i))]))

(define (species->image j)
  (match j
    [`[("food" ,food) ("body" ,body) ("population" ,population) ("traits" ,traits)]
     (above/align 'left
                   (labeled "food" food)
                   (labeled "body" body)
                   (labeled "pop." population)
                   (traits->image traits))]
    [`[("food" ,food) ("body" ,body) ("population" ,population) ("traits" ,traits) ("fat-food" ,ff)]
     (above/align 'left
                   (labeled "food" food)
                   (labeled "body" body)
                   (labeled "pop." population)
                   (labeled "fat " ff)
                   (traits->image traits))]))

(define (cards->image j)
  (match j
    [`(,card* ...)
     (for/fold ((i empty-image)) ((c card*))
       (above/align 'left (card->image (json->card c)) i))]))

(define (traits->image j*)
  (for/fold ((i empty-image)) ((t j*))
    (above/align 'left (scale .7 (trait->img (json->trait t))) i)))


(define (labeled l j)
  (text (format "~a: ~a" l j) TEXT-SIZE TEXT-COLOR))

(module+ test
  (dealer->image dealer0))