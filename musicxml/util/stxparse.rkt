#lang racket/base

(provide (all-from-out syntax/parse)
         str
         num int nat pos-int
         str-num str-int str-nat str-pos-int)

(require (rename-in (except-in syntax/parse ~rest)
                    [attribute @]
                    [str stx-str])
         syntax/parse/define)

;; ---------------------------------------------------------

(define-syntax-class str
  #:attributes [string]
  [pattern s:stx-str
    #:attr string (syntax-e #'s)])

;; ---------------------------------------------------------

(define-syntax-class num
  #:attributes [number]
  [pattern n:number
    #:attr number (syntax-e #'n)])

(define-syntax-class int
  #:attributes [number]
  [pattern :num
    #:when (exact-integer? (@ number))])

(define-syntax-class nat
  #:attributes [number]
  [pattern :num
    #:when (exact-nonnegative-integer? (@ number))])

(define-syntax-class pos-int
  #:attributes [number]
  [pattern :num
    #:when (exact-positive-integer? (@ number))])

;; ---------------------------------------------------------

(define-simple-macro
  (define-str-numeric-syntax-class name num-class)
  (define-syntax-class name
    #:auto-nested-attributes
    [pattern {~var || str}
      #:with {~var || num-class} (string->number (@ string))]))

(define-str-numeric-syntax-class str-num num)
(define-str-numeric-syntax-class str-int int)
(define-str-numeric-syntax-class str-nat nat)
(define-str-numeric-syntax-class str-pos-int pos-int)

;; ---------------------------------------------------------

