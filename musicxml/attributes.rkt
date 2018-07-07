#lang racket/base

(provide (all-defined-out)
         key
         time
         clef)

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "time-signature.rkt"
         "clef.rkt"
         "key.rkt"
         "editorial.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")
(module+ test
  (require rackunit))

;; ---------------------------------------------------------

;; The divisions type is used to express values in terms of the musical
;; divisions defined by the `divisions` element. 
(define str-divisions/c (flat-named-contract 'str-divisions/c str-integer?))

(define str-positive-divisions/c
  (flat-named-contract 'str-positive-divisions/c str-positive-integer?))

;; ---------------------------------------------------------

(define attribute-element/c
  (or/c (tag/c footnote)
        (tag/c level)
        (tag/c divisions)
        (tag/c key)
        (tag/c time)
        (tag/c staves)
        (tag/c part-symbol)
        (tag/c instruments)
        (tag/c clef)
        (tag/c staff-details)
        (tag/c transpose)
        (tag/c measure-style)))

(define-tag attributes '() (listof attribute-element/c))

(define-tag divisions '() (list/c str-positive-divisions/c))

;; ---------------------------------------------------------

(define-syntax-class attributesₑ
  #:attributes []
  [pattern {~attributes
            ()
            (:%editorial
             {~optional :divisionsₑ}
             :keyₑ
             ...
             :timeₑ
             ...
             ;; TODO: staves, part-symbol, and instruments
             :clefₑ
             ...
             ;; TODO: staff-details, transpose
             ;; TODO: :directiveₑ ...
             ;; TODO: measure-style
             )}])

(define-syntax-class divisionsₑ
  #:attributes [divisions-number]
  [pattern {~divisions () (n:str-pos-int)}
    #:attr divisions-number (@ n.number)])

;; ---------------------------------------------------------

;; Any -> Boolean
(define (divisions? v)
  (syntax-parse v
    [:divisionsₑ #true]
    [_ #false]))

;; Attributes -> Boolean
(define (attributes-has-divisions? a)
  (match a
    [(attributes _ elements)
     (ormap divisions? elements)]))

;; Attributes -> PositiveInteger
(define (attributes-divisions a)
  (match a
    [(attributes _ elements)
     (for/first ([e (in-list elements)]
                 #:when (divisions? e))
       (syntax-parse e
         [:divisionsₑ (@ divisions-number)]))]))

;; ---------------------------------------------------------

(module+ test
  (check-true (attributes-has-divisions?
               #`(attributes (divisions "4"))))
  (check-false (attributes-has-divisions?
                #`(attributes)))

  (check-true
   (syntax-parse
       #'(attributes
          (divisions "24")
          (key (fifths "3"))
          (time (beats "4") (beat-type "4"))
          (clef (sign "C") (line "3")))
     [:attributesₑ #true]
     [_ #false]))
  )

;; ---------------------------------------------------------

