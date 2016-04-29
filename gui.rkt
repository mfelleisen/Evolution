#lang racket/gui

;; ===================================================================================================
;; a class for displaying an image on a monitor

(require "observer.rkt")

(provide
 (contract-out
  [create-gui (-> (and/c natural-number/c (<=/c WIDTH))
                  (and/c natural-number/c (<=/c HEIGHT))
                  observer/c)]))

;; ===================================================================================================
(require 2htdp/image json)

;; ===================================================================================================
(define WIDTH 1200)
(define HEIGHT 800)
(define INSET 10)

(define (create-gui w h)
  (new display% [pre-set-width w][pre-set-height h]))

(define display%
  (class object%
    (init-field [pre-set-width WIDTH][pre-set-height HEIGHT])
    (super-new)
    
    (field
     [frame
      (new frame%
           [label "Evolution Game"]
           [alignment '(center center)]
           [width  pre-set-width]
           [height pre-set-height])]
     [paste  (new pasteboard%)]
     [editor
      (new editor-canvas%
           (parent frame) (editor paste) (horizontal-inset INSET) (vertical-inset INSET))])
    
    (sleep .1)
    (send frame show #t)
    
    ;; -----------------------------------------------------------------------------------------------
    (define/public (display j)
      (define pict (dealer->image j))
      (define width (max (image-width pict) pre-set-width))
      (define height (max (image-height pict) pre-set-height))
      (send frame resize width height)
      (send frame show #true)
      (send editor focus)
      (send editor min-client-width  (+ pre-set-width INSET INSET))
      (send editor min-client-height (+ pre-set-height INSET INSET))
      (send paste set-cursor (make-object cursor% 'arrow))
      ;; --- insert pict now 
      (send paste begin-edit-sequence)
      (send paste lock #f)
      (define snip (send paste find-first-snip))
      (when snip (send paste delete snip))
      (send paste insert #;disable-cache (send pict copy) 0 0)
      (send paste lock #t)
      (send paste end-edit-sequence))))

(module+ test 
  (define d (create-gui 800 760))
  
  (send d display dealer0)
  (send d display dealer0)
  (send d display dealer0))
