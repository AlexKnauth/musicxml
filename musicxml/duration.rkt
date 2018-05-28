#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "attributes.rkt"
         "util/tag.rkt")
(module+ test
  (require rackunit))


(define-tag duration '() (list/c str-positive-divisions/c))

;; duration-divisions : Duration -> PositiveDivisions
(define (duration-divisions d)
  (match d
    [(duration '() (list str))
     (string->number str)]))

;; ---------------------------------------------------------

(module+ test
  (check-equal? (duration-divisions (duration '() '("1"))) 1)
  (check-equal? (duration-divisions (duration '() '("2"))) 2)
  (check-equal? (duration-divisions (duration '() '("4"))) 4)
  (check-equal? (duration-divisions (duration '() '("6"))) 6)
  (check-equal? (duration-divisions (duration '() '("8"))) 8)
  (check-equal? (duration-divisions (duration '() '("10"))) 10)
  (check-equal? (duration-divisions (duration '() '("16"))) 16)
  (check-equal? (duration-divisions (duration '() '("17"))) 17)
  (check-equal? (duration-divisions (duration '() '("25"))) 25)
  (check-equal? (duration-divisions (duration '() '("63"))) 63)
  (check-equal? (duration-divisions (duration '() '("128"))) 128)
  )

