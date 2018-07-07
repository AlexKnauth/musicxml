#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         syntax/parse/define
         (submod txexpr safe)
         "str-number.rkt"
         "attributes.rkt"
         "pitch.rkt"
         "frame.rkt"
         "util/tag.rkt"
         "util/parse.rkt"
         "util/stxparse.rkt"
         (for-syntax racket/base))

(define-simple-macro
  (define-str-enum ctc-name:id class-name:id
    [[elem-name1:id] ... [elem-str2:str] ...])
  #:with [elem-str1 ...]
  (for/list ([elem-name1 (in-list (syntax->list #'[elem-name1 ...]))])
    (datum->syntax elem-name1 (symbol->string (syntax-e elem-name1))))
  (begin
    (define elem-name1 elem-str1)
    ...
    (define ctc-name
      (or/c elem-str1 ... elem-str2 ...))
    (define-syntax-class class-name
      [pattern elem-str1]
      ...
      [pattern elem-str2]
      ...)))

;; ---------------------------------------------------------

;; The harmony-type type differentiates different types of
;; harmonies when alternate harmonies are possible.
;; Explicit harmonies have all notes present in the music;
;; implied have some notes missing but implied; alternate
;; represents alternate analyses.
(define str-harmony-type/c
  (or/c "explicit"
        "implied"
        "alternate"))

;; A kind-value indicates the type of chord. Degree
;; elements can then add, subtract, or alter from these
;; starting points.
;; Values include:
;; Triads:
;;  - major (major third, perfect fifth)
;;  - minor (minor third, perfect fifth)
;;  - augmented (major third, augmented fifth)
;;  - diminished (minor third, diminished fifth)
;; Sevenths:
;;  - dominant (major triad, minor seventh)
;;  - major-seventh (major triad, major seventh)
;;  - minor-seventh (minor triad, minor seventh)
;;  - diminished-seventh (diminished triad, diminished seventh)
;;  - augmented-seventh (augmented triad,
;; minor seventh)
;;  - half-diminished (diminished triad, minor seventh)
;;  - major-minor (minor triad, major seventh)
;; Sixths:
;;  - major-sixth (major triad, added sixth)
;;  - minor-sixth (minor triad, added sixth)
;; Ninths:
;;  - dominant-ninth (dominant-seventh, major ninth)
;;  - major-ninth (major-seventh, major ninth)
;;  - minor-ninth (minor-seventh, major ninth)
;; 11ths (usually as the basis for alteration):
;;  - dominant-11th (dominant-ninth, perfect 11th)
;;  - major-11th (major-ninth, perfect 11th)
;;  - minor-11th (minor-ninth, perfect 11th)
;; 13ths (usually as the basis for alteration):
;;  - dominant-13th (dominant-11th, major 13th)
;;  - major-13th (major-11th, major 13th)
;;  - minor-13th (minor-11th, major 13th)
;; Suspended:
;;  - suspended-second (major second, perfect fifth)
;;  - suspended-fourth (perfect fourth, perfect fifth)
;; Functional sixths: Neapolitan Italian French German
;; Other: pedal (pedal-point bass) power (perfect fifth) Tristan
;; The "other" kind is used when the harmony is entirely
;; composed of add elements.
;; The "none" kind is used to explicitly encode absence of
;; chords or functional harmony.

(define-str-enum str-kind-value/c str-kind-value
  [[major]
   [minor]
   [augmented]
   [diminished]

   [dominant]
   [major-seventh]
   [minor-seventh]
   [diminished-seventh]
   [augmented-seventh]
   [half-diminished]
   [major-minor]

   [major-sixth]
   [minor-sixth]

   [dominant-ninth]
   [major-ninth]
   [minor-ninth]

   [dominant-11th]
   [major-11th]
   [minor-11th]
   [dominant-13th]
   [major-13th]
   [minor-13th]

   [suspended-second]
   [suspended-fourth]

   [pedal]
   [power]

   [other]

   ["none"]])

;; ---------------------------------------------------------

;; The degree-type-value type indicates whether the
;; current degree element is an addition, alteration, or
;; subtraction to the kind of the current chord in the
;; harmony element.
(define add "add")
(define alter "alter")
(define subtract "subtract")

(define str-degree-type-value/c
  (or/c add alter subtract))

(define-syntax-class str-degree-type-value
  [pattern "add"]
  [pattern "alter"]
  [pattern "subtract"])

;; ---------------------------------------------------------

(define-tag harmony
  (attrs-may/c 'type str-harmony-type/c)
  (parse/c
   ({~seq
     {~or {~tag function}
          {~tag root}}
     {~tag kind}
     {~optional {~tag inversion}}
     {~optional {~tag bass}}
     {~tag degree}
     ...}
    ...+
    {~optional {~tag frame}}
    {~optional {~tag offset}}
    {~optional {~tag footnote}}
    {~optional {~tag level}}
    {~optional {~tag staff}})))

(define-tag function any/c (list/c string?))

;; Kind indicates the type of chord. Degree elements can
;; then add, subtract, or alter from these starting points.
;; Since the kind element is the constant in all the
;; harmony-chord groups that can make up a polychord, many
;; formatting attributes are here. The alignment attributes
;; are for the entire harmony-chord group of which this
;; kind element is a part.
(define-tag kind any/c (list/c str-kind-value/c))

;; The inversion type represents harmony inversions. The
;; value is a number indicating which inversion is used:
;; 0 for root position, 1 for first inversion, etc.
(define-tag inversion any/c (list/c str-nonnegative-integer?))

;; The degree type is used to add, alter, or subtract
;; individual notes in the chord. The degree-value and
;; degree-type text attributes specify how the value and
;; type of the degree should be displayed.
;; A harmony of kind "other" can be spelled explicitly by
;; using a series of degree elements together with a root.
(define-tag degree '()
  (list/c (tag/c degree-value)
          (tag/c degree-alter)
          (tag/c degree-type)))

(define-tag degree-value any/c (list/c str-positive-integer?))
(define-tag degree-alter any/c (list/c str-decimal?))
(define-tag degree-type any/c (list/c str-degree-type-value/c))

;; ---------------------------------------------------------

(define-syntax-class harmonyₑ
  #:attributes []
  [pattern {~harmony
            _
            (;; more than one chord means a polychord
             chord:%harmony-chord
             ...+
             {optional frame:frameₑ})}])

;; ---------------------------------------------------------

(define-splicing-syntax-class %harmony-chord
  [pattern {~seq {~or root:rootₑ :functionₑ}
                 :kindₑ
                 {~optional :inversionₑ}
                 {~optional bass:bassₑ}
                 :degreeₑ
                 ...}])

(define-syntax-class functionₑ
  #:attributes []
  [pattern {~function () (:str)}])

(define-syntax-class kindₑ
  #:attributes []
  [pattern {~kind () kind-string*:str}
    #:when (str-kind-value/c (@ kind-string*.string))])

(define-syntax-class inversionₑ
  #:attributes [inversion-number]
  [pattern {~inversion () (inversion:str-nat)}
    #:attr inversion-number (@ inversion.number)])

(define-syntax-class degreeₑ
  #:attributes []
  [pattern {~degree ()
                    (:degree-valueₑ
                     :degree-alterₑ
                     :degree-typeₑ)}])

(define-syntax-class degree-valueₑ
  #:attributes []
  [pattern {~degree-value () (:str-pos-int)}])

(define-syntax-class degree-alterₑ
  #:attributes []
  [pattern {~degree-alter () (:str-int)}])

(define-syntax-class degree-typeₑ
  #:attributes []
  [pattern {~degree-type () (:str-degree-type-value)}])

;; ---------------------------------------------------------

