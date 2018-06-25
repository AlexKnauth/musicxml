#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         (except-in syntax/parse ~rest)
         "str-number.rkt"
         "attributes.rkt"
         "note.rkt"
         "util/tag.rkt"
         "util/parse.rkt")

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

(define major "major")
(define minor "minor")
(define augmented "augmented")
(define diminished "diminished")

(define dominant "dominant")
(define major-seventh "major-seventh")
(define minor-seventh "minor-seventh")
(define diminished-seventh "diminished-seventh")
(define augmented-seventh "augmented-seventh")
(define half-diminished "half-diminished")
(define major-minor "major-minor")

(define major-sixth "major-sixth")
(define minor-sixth "minor-sixth")

(define dominant-ninth "dominant-ninth")
(define major-ninth "major-ninth")
(define minor-ninth "minor-ninth")

(define dominant-11th "dominant-11th")
(define major-11th "major-11th")
(define minor-11th "minor-11th")
(define dominant-13th "dominant-13th")
(define major-13th "major-13th")
(define minor-13th "minor-13th")

(define suspended-second "suspended-second")
(define suspended-fourth "suspended-fourth")

(define pedal "pedal")
(define power "power")

(define other "other")

(define str-kind-value/c
  (or/c major minor augmented diminished
        dominant major-seventh minor-seventh
        diminished-seventh augmented-seventh
        half-diminished major-minor
        major-sixth minor-sixth
        dominant-ninth major-ninth minor-ninth
        dominant-11th major-11th minor-11th
        dominant-13th major-13th minor-13th
        suspended-second suspended-fourth
        pedal power
        other
        "none"))

;; The degree-type-value type indicates whether the
;; current degree element is an addition, alteration, or
;; subtraction to the kind of the current chord in the
;; harmony element.
(define add "add")
(define alter "alter")
(define subtract "subtract")

(define str-degree-type-value/c
  (or/c add alter subtract))

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

(define-tag root '()
  (or/c (list/c (tag/c root-step))
        (list/c (tag/c root-step) (tag/c root-alter))))

(define-tag root-step any/c (list/c str-step/c))
(define-tag root-alter any/c (list/c str-decimal?))

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

;; The bass type is used to indicate a bass note in
;; popular music chord symbols, e.g. G/C. It is generally
;; not used in functional harmony, as inversion is
;; generally not used in pop chord symbols. As with root,
;; it is divided into step and alter elements, similar to
;; pitches.
(define-tag bass '()
  (or/c (list/c (tag/c bass-step))
        (list/c (tag/c bass-step) (tag/c bass-alter))))

(define-tag bass-step any/c (list/c str-step/c))
(define-tag bass-alter any/c (list/c str-decimal?))

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

(define-tag frame any/c any/c)
(define-tag frame-strings any/c any/c)
(define-tag frame-frets any/c any/c)
(define-tag frame-note any/c any/c)
(define-tag string any/c any/c)
(define-tag fret any/c any/c)

