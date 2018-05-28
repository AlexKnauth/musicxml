#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "duration.rkt"
         "note.rkt"
         "util/tag.rkt")

;; ---------------------------------------------------------

(define-tag figured-bass any/c
  (listof (or/c (tag/c figure)
                (tag/c duration)
                (tag/c footnote)
                (tag/c level))))

(define-tag figure any/c any/c)

;; ---------------------------------------------------------

;; FiguredBass -> PositiveDivisions
(define (figured-bass-duration-divisions fb)
  ;; Any -> Boolean
  (define (duration? v)
    (match v [(duration _ _) #true] [_ #false]))
  (match fb
    [(figured-bass _ (list _ ... (? duration? d) _ ...))
     (duration-divisions d)]))

;; ---------------------------------------------------------

