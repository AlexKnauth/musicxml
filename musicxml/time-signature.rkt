#lang racket/base

;; Time Signatures

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         "str-number.rkt"
         "util/tag.rkt")

;; ---------------------------------------------------------

(define-tag time any/c
  (or/c
   ; explicity no time signature
   (list/c (tag/c senza-misura))
   ; a time signature with beats
   (list/c (tag/c beats) (tag/c beat-type))))

;; explicitly no time signature
(define-tag senza-misura '() '())

;; the number of beats in a time signature
(define-tag beats '() (list/c str-nonnegative-integer?))

;; the type of the beats in a time signature
(define-tag beat-type '() (list/c str-positive-integer?))

