#lang racket/base

(provide define-tag
         attrs-must/c
         attrs-may/c
         tag/c)

(require racket/contract/base
         racket/contract/region
         syntax/parse/define
         (submod txexpr safe)
         txexpr/stx
         txexpr/stx/parse
         "match.rkt"
         (for-syntax racket/base
                     racket/syntax))

;; stx-e : Stx -> Any
(define (stx-e stx)
  (if (syntax? stx) (syntax-e stx) stx))

;; stx->datum : Stx -> Any
(define (stx->datum stx)
  (syntax->datum (datum->syntax #f stx)))

(define-simple-macro (implies a b) (if a b #true))

(begin-for-syntax
  (define (txexpr-tag-match-transformer sym)
    (syntax-parser
      [(_ attrs-pat:expr elems-pat:expr)
       #:with name sym
       #'(? stx-txexpr?
            (app stx-txexpr->values
                 (app stx-e 'name)
                 attrs-pat
                 elems-pat))])))

(define-simple-macro
  (define-tag name:id attrs/c:expr elems/c:expr)
  #:with name* (format-id #'name "~a*" #'name
                          #:source #'name #:props #'name)
  #:with stxparse-pat-name (format-id #'name "~~~a" #'name
                                      #:source #'name #:props #'name)
  (begin
    (define attrs-c attrs/c)
    (define elems-c elems/c)
    (define-txexpr-pattern-expander stxparse-pat-name name)
    (define/contract/match-expander name
      (-> attrs-c elems-c any)
      (λ (attrs elems) (txexpr 'name attrs elems))
      (txexpr-tag-match-transformer 'name))
    (define/contract name*
      (->* () (attrs-c) #:rest elems-c any)
      (λ ([attrs '()] . elems) (txexpr 'name attrs elems)))))

;; attrs-must/c : Symbol FlatContract -> FlatContract
(define (attrs-must/c key value/c)
  (define value? (flat-contract-predicate value/c))
  (flat-named-contract
   `(attrs-must/c ',key ,(contract-name value/c))
   (λ (attrs)
     (define hsh (attrs->hash (stx->datum attrs)))
     (and (hash-has-key? hsh key)
          (value? (hash-ref hsh key))))))

;; attrs-may/c : Symbol FlatContract -> FlatContract
(define (attrs-may/c key value/c)
  (define value? (flat-contract-predicate value/c))
  (flat-named-contract
   `(attrs-may/c ',key ,(contract-name value/c))
   (λ (attrs)
     (define hsh (attrs->hash (stx->datum attrs)))
     (implies (hash-has-key? hsh key)
              (value? (hash-ref hsh key))))))

(define (*tag/c sym)
  (flat-named-contract
   `(tag/c ,sym)
   (λ (tx)
     (and (stx-txexpr? tx)
          (eq? (stx->datum (stx-txexpr-tag tx)) sym)))))

(define-syntax-parser tag/c
  [(_ sym:id)
   (syntax-property #'(*tag/c 'sym)
                    'disappeared-use
                    (list (syntax-local-introduce #'sym)))])

