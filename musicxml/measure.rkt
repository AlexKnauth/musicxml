#lang racket/base

(provide (except-out (all-defined-out) attributes/w-div?))

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "music-data.rkt"
         "attributes.rkt"
         "util/tag.rkt")

;; ---------------------------------------------------------

(define-tag measure
  (attrs-must/c 'number str-integer?)
  (listof music-data/c))

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

