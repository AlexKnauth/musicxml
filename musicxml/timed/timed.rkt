#lang racket/base

(provide (struct-out timed)
         timed-measure
         timed-map
         group-and-sort-by-measure)

(require racket/list
         racket/match
         "time.rkt")

;; ---------------------------------------------------------

;; A [Timed X] is a (timed Time X)
(struct timed [time value] #:transparent)

;; [Timed X] -> Integer
(define (timed-measure tx)
  (time-measure (timed-time tx)))

;; ---------------------------------------------------------

;; [X -> Y] [Timed X] -> [Timed Y]
(define (timed-map f tx)
  (match tx
    [(timed t x)
     (timed t (f x))]))

;; ---------------------------------------------------------

;; [Listof [Timed X]] -> [Listof [Listof Timed X]]
(define (group-and-sort-by-measure txs)
  ;; group the timed elements by measure
  ;; each group has at least one element
  (define groups
    (group-by timed-measure txs))
  ;; sort the groups by measure
  (define (group-measure g)
    (timed-measure (first g)))
  (sort groups < #:key group-measure))

;; ---------------------------------------------------------

