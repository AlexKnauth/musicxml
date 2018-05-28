#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         "str-number.rkt"
         "util/tag.rkt")
(module+ example
  (provide (all-defined-out)))

(define str-staff-number/c str-positive-integer?)

;; The clef-sign element represents the different clef
;; symbols. The jianpu sign indicates that the music that
;; follows should be in jianpu numbered notation, just as
;; the TAB sign indicates that the music that follows
;; should be in tablature notation. Unlike TAB, a jianpu
;; sign does not correspond to a visual clef notation.
(define G "G")  ; treble
(define F "F")  ; bass
(define C "C")  ; alto
(define percussion "percussion")
(define TAB "TAB")
(define jianpu "jianpu")

(define str-clef-sign/c
  (or/c G  ; treble
        F  ; bass
        C  ; alto
        percussion
        TAB
        jianpu
        "none"))

;; The staff-line type indicates the line on a given staff.
;; Staff lines are numbered from bottom to top, with 1
;; being the bottom line on a staff. Staff line values can
;; be used to specify positions outside the staff, such as
;; a C clef positioned in the middle of a grand staff.
(define str-staff-line/c str-integer?)

;; ---------------------------------------------------------

(define-tag clef any/c
  (or/c (list/c (tag/c sign))
        (list/c (tag/c sign) (tag/c line))
        (list/c (tag/c sign) (tag/c clef-octave-change))
        (list/c (tag/c sign) (tag/c line) (tag/c clef-octave-change))))

(define-tag sign '() (list/c str-clef-sign/c))
(define-tag line '() (list/c str-staff-line/c))
(define-tag clef-octave-change '() (list/c str-integer?))

;; ---------------------------------------------------------

(module+ example
  (define treble-clef
    (clef (sign G) (line "2")))

  (define bass-clef
    (clef (sign F) (line "4")))

  (define vocal-tenor-clef
    (clef (sign G)
          (line "2")
          (clef-octave-change "-1")))

  (define alto-clef
    (clef (sign C) (line "3"))))

