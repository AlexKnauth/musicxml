#lang racket/base

(provide (all-defined-out)
         key
         time
         clef)

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "time-signature.rkt"
         "clef.rkt"
         "key.rkt"
         "util/tag.rkt")

;; The divisions type is used to express values in terms of the musical
;; divisions defined by the `divisions` element. 
(define str-divisions/c (flat-named-contract 'str-divisions/c str-integer?))

(define str-positive-divisions/c
  (flat-named-contract 'str-positive-divisions/c str-positive-integer?))

;; ---------------------------------------------------------

(define attribute-element/c
  (or/c (tag/c footnote)
        (tag/c level)
        (tag/c divisions)
        (tag/c key)
        (tag/c time)
        (tag/c staves)
        (tag/c part-symbol)
        (tag/c instruments)
        (tag/c clef)
        (tag/c staff-details)
        (tag/c transpose)
        (tag/c measure-style)))

(define-tag attributes '() (listof attribute-element/c))

(define-tag divisions '() (list/c str-positive-divisions/c))

;; ---------------------------------------------------------

;; Any -> Boolean
(define (divisions? v)
  (match v
    [(divisions _ _) #true]
    [_ #false]))

;; Attributes -> Boolean
(define (attributes-has-divisions? a)
  (match a
    [(attributes _ elements)
     (ormap divisions? elements)]))

;; Attributes -> PositiveInteger
(define (attributes-divisions a)
  (match a
    [(attributes _ elements)
     (for/first ([e (in-list elements)]
                 #:when (divisions? e))
       (match e
         [(divisions _ (list str-div)) (string->number str-div)]))]))

;; ---------------------------------------------------------

