#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "duration.rkt"
         "util/tag.rkt")

;; ---------------------------------------------------------

(define-tag barline any/c
  (listof (or/c (tag/c bar-style)
                (tag/c footnote)
                (tag/c level)
                (tag/c wavy-line)
                (tag/c segno)
                (tag/c coda)
                (tag/c fermata)
                (tag/c ending)
                (tag/c repeat))))

(define-tag bar-style any/c any/c)
(define-tag wavy-line any/c any/c)
(define-tag segno any/c any/c)
(define-tag coda any/c any/c)
(define-tag fermata any/c any/c)
(define-tag ending any/c any/c)
(define-tag repeat any/c any/c)

;; ---------------------------------------------------------

