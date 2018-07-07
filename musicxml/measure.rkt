#lang racket/base

(provide (except-out (all-defined-out) attributes/w-div?))

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "music-data.rkt"
         "attributes.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")

;; ---------------------------------------------------------

(define-tag measure
  (attrs-must/c 'number str-integer?)
  (listof music-data/c))

;; ---------------------------------------------------------

(define-syntax-class measureₑ
  #:attributes [measure-number]
  #:datum-literals [number width]
  [pattern {~measure
            ({~alt {~once [number N:str-int]}
                   {~optional [width w:str-num]}}
             ...)
            (:%music-data
             ...)}
    #:attr measure-number (@ N.number)])

;; ---------------------------------------------------------

;; Measure -> Integer
;; Returns the value in the number attribute, which is
;; probably one-indexed.
(define (measure-number m)
  (match m
    [(measure _ _)
     (string->number (attr-ref m 'number))]))

;; ---------------------------------------------------------

;; private
;; Any -> Boolean
(define (attributes/w-div? v)
  (match v
    [(attributes _ _) (attributes-has-divisions? v)]
    [_ #false]))

;; Measure -> Boolean
(define (measure-has-divisions? m)
  (match m
    [(measure _ elements)
     (ormap attributes/w-div? elements)]))

;; Measure -> PositiveInteger
(define (measure-divisions m)
  (match m
    [(measure _ elements)
     (for/first ([e (in-list elements)]
                 #:when (attributes/w-div? e))
       (attributes-divisions e))]))

;; ---------------------------------------------------------

