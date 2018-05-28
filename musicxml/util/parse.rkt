#lang racket/base

(provide parse/c ~tag)

(require racket/contract/base
         syntax/parse/define
         syntax/parse
         "tag.rkt"
         (for-syntax racket/base))

(define-simple-macro
  (parse/c pat)
  (flat-named-contract
   '(parse/c pat)
   (syntax-parser
     [pat #true]
     [_ #false])))

(define-syntax-class (datum-pred passes?)
  [pattern stx
           #:when (passes? (syntax->datum #'stx))])

(define-syntax ~tag
  (pattern-expander
   (syntax-parser
     [(_ name:id)
      #'{~var _ (datum-pred (tag/c name))}])))

