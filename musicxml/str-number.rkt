#lang racket/base

(provide str-decimal?
         str-integer?
         str-positive-integer?
         str-nonnegative-integer?)

(define (str-decimal? s)
  (and (string? s)
       (real? (string->number s))))

(define (str-integer? s)
  (and (string? s)
       (exact-integer? (string->number s))))

(define (str-positive-integer? s)
  (and (string? s)
       (exact-positive-integer? (string->number s))))

(define (str-nonnegative-integer? s)
  (and (string? s)
       (exact-nonnegative-integer? (string->number s))))

