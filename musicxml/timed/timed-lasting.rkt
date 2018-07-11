#lang racket/base

(require racket/match
         racket/splicing
         "timed.rkt"
         "time-period.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/transformer))

;; ---------------------------------------------------------

(provide lasting?
         lasting
         lasting-duration
         lasting-value)

;; A [Lasting X] is a
;;   (lasting NonNegDivisions X)
(struct lasting [duration value] #:transparent)

;; ---------------------------------------------------------

(provide tila?
         tila
         tila-period
         tila-value)

;; A [Tila X] is a [Timed [Lasting X]]

;; tila? : Any -> Bool
(define (tila? v)
  (and (timed? v)
       (lasting? (timed-value v))))

;; tila : TimePeriod X -> [Tila X]
(splicing-local
    [(define (tila tp v)
       (match-define (time-period t d) tp)
       (timed t (lasting d v)))
     (define make-tila tila)
     (define (tila->maybe-list tl)
       (match tl
         [(timed t (lasting d v)) (list (time-period t d) v)]
         [_ #false]))]
  (define-match-expander tila
    (syntax-parser
      [(_ tp-pat:expr v-pat:expr) #'(app tila->maybe-list (list tp-pat v-pat))])
    (make-variable-like-transformer #'make-tila)))


;; tila-period : [Tila X] -> TimePeriod
(define (tila-period tlx)
  (time-period (timed-time tlx)
               (lasting-duration (timed-value tlx))))

;; tila-value : [Tila X] -> X
(define (tila-value tlx)
  (lasting-value (timed-value tlx)))

;; ---------------------------------------------------------

;; Sorting by time period

(provide sort-by-time-period)

;; sort-by-time-period : [Tila X] -> [Tila X]
(define (sort-by-time-period tlxs)
  (sort tlxs time-period<? #:key tila-period))

;; ---------------------------------------------------------

