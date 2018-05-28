#lang racket/base

(provide define-tag
         attrs-must/c
         attrs-may/c
         tag/c)

(require racket/contract/base
         syntax/parse/define
         (submod txexpr safe)
         "match.rkt"
         (for-syntax racket/base))

(define-simple-macro (implies a b) (if a b #true))

(begin-for-syntax
  (define (txexpr-tag-match-transformer sym)
    (syntax-parser
      [(_ attrs-pat:expr elems-pat:expr)
       #:with name sym
       #'(? txexpr? (app txexpr->values 'name attrs-pat elems-pat))])))

(define-simple-macro
  (define-tag name:id attrs/c:expr elems/c:expr)
  (define/contract/match-expander name
    (-> attrs/c elems/c any)
    (λ (attrs elems) (txexpr 'name attrs elems))
    (txexpr-tag-match-transformer 'name)))

;; attrs-must/c : Symbol FlatContract -> FlatContract
(define (attrs-must/c key value/c)
  (define value? (flat-contract-predicate value/c))
  (flat-named-contract
   `(attrs-must/c ',key ,(contract-name value/c))
   (λ (attrs)
     (define hsh (attrs->hash attrs))
     (and (hash-has-key? hsh key)
          (value? (hash-ref hsh key))))))

;; attrs-may/c : Symbol FlatContract -> FlatContract
(define (attrs-may/c key value/c)
  (define value? (flat-contract-predicate value/c))
  (flat-named-contract
   `(attrs-may/c ',key ,(contract-name value/c))
   (λ (attrs)
     (define hsh (attrs->hash attrs))
     (implies (hash-has-key? hsh key)
              (value? (hash-ref hsh key))))))

(define (*tag/c sym)
  (flat-named-contract
   `(tag/c ,sym)
   (λ (tx)
     (and (txexpr? tx)
          (eq? (get-tag tx) sym)))))

(define-syntax-parser tag/c
  [(_ sym:id)
   (syntax-property #'(*tag/c 'sym)
                    'disappeared-use
                    (list (syntax-local-introduce #'sym)))])

