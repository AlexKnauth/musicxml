#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         "str-number.rkt"
         "util/tag.rkt")

;; The mode type is used to specify major/minor and other
;; mode distinctions. Valid mode values include major,
;; minor, dorian, phrygian, lydian, mixolydian, aeolian,
;; ionian, locrian, and none.
(define major "major")
(define minor "minor")
(define dorian "dorian")
(define phrygian "phrygian")
(define lydian "lydian")
(define mixolydian "mixolydian")
(define aeolian "aeolian")
(define ionian "ionian")
(define locrian "locrian")

(define str-mode/c
  (or/c major minor dorian phrygian lydian mixolydian
        aeolian ionian locrian "none"))

;; ---------------------------------------------------------

(define-tag key any/c
  (listof (or/c (tag/c cancel)
                (tag/c fifths)
                (tag/c mode))))

(define-tag cancel '() (list/c str-integer?))
(define-tag fifths '() (list/c str-integer?))
(define-tag mode '() (list/c str-mode/c))

