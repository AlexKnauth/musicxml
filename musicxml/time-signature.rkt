#lang racket/base

;; Time Signatures

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         "str-number.rkt"
         "note-type.rkt"
         "util/stxparse.rkt"
         "util/tag.rkt")
(module+ test
  (require rackunit))

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

;; ---------------------------------------------------------

(define-syntax-class timeₑ
  #:attributes [senza-misura? beats beat-type]
  [pattern {~time _ (:senza-misuraₑ)} #:attr beats #f #:attr beat-type #f]
  [pattern {~time _ (:beatsₑ :beat-typeₑ)} #:attr senza-misura? #f])

(define-syntax-class senza-misuraₑ
  #:attributes [senza-misura?]
  [pattern {~senza-misura () ()}
    #:attr senza-misura? #t])

(define-syntax-class beatsₑ
  #:attributes [beats]
  [pattern {~beats () (b:str-nat)}
    #:attr beats (@ b.number)])

(define-syntax-class beat-typeₑ
  #:attributes [beat-type]
  [pattern {~beat-type () (t:str-pos-int)}
    #:attr beat-type (@ t.number)])

;; ---------------------------------------------------------

(module+ test
  (check-true
   (syntax-parse
       #'(time (beats "4") (beat-type "4"))
     [:timeₑ #true]
     [_ #false]))

  (syntax-parse
      #'(time (beats "4") (beat-type "4"))
    [t:timeₑ
     (check-equal? (@ t.senza-misura?) #f)
     (check-equal? (@ t.beats) 4)
     (check-equal? (@ t.beat-type) 4)])
  )
