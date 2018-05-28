#lang racket/base

(provide define/contract/match-expander)

(require racket/contract/region
         racket/match
         syntax/parse/define
         (for-syntax racket/base
                     "id-transformer.rkt"))

(define-simple-macro
  (define/contract/match-expander name:id
    ctc:expr
    fn-expr:expr
    match-transformer:expr)
  (begin
    (define/contract fn-id ctc
      (let ([name fn-expr])
        name))
    (define-match-expander name
      match-transformer
      (var-like-transformer
       (λ (stx) (quote-syntax fn-id))))))

