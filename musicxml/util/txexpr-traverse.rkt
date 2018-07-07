#lang racket/base

(provide txexpr-traverse/recur)

(require txexpr
         txexpr/stx)

;; ---------------------------------------------------------

;; txexpr-traverse/recur :
;; StxTXexpr [StxTXexpr -> StxTXexpr] -> StxTXexpr
;; Note that rec must be able to take both syntax and
;; non-stx-but-containing-stx values, in other words,
;; rec must be able to take something that might come
;; from syntax-e
(define (txexpr-traverse/recur stx rec)
  (define e (stx-e stx))
  (restore
   stx
   (cond
     [(atomic-literal? e) stx]
     [(stx-txexpr? e)
      (define-values [tag attrs elements]
        (stx-txexpr->values e))
      (txexpr tag attrs (map rec elements))]
     [else
      (error 'txexpr-traverse "unrecognized value: ~v" stx)])))

;; stx-e : Stx -> E
(define (stx-e stx)
  (if (syntax? stx) (syntax-e stx) stx))

;; restore : Stx E -> Stx
(define (restore stx e)
  (if (syntax? stx) (datum->syntax stx e stx stx) e))

;; atomic-literal? : E -> Boolean
(define (atomic-literal? e)
  (or (null? e) (boolean? e) (number? e) (symbol? e)
      (string? e) (bytes? e)
      (regexp? e) (byte-regexp? e)))

;; ---------------------------------------------------------

