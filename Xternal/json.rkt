#lang racket

;; ===================================================================================================
;; a silly little module

;; SERVICES

(provide
  ;; contracts 
  json/c
  jsexpr/c)

;; ===================================================================================================
;; IMPLEMENTATION

(define jsexpr/c any/c)

(define json/c
  (object/c
    [to-json (->m jsexpr/c)]))
