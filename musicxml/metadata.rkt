#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         "util/tag.rkt")

(define work-element/c
  (or/c (tag/c work-number)
        (tag/c work-title)
        (tag/c opus)))

;; Works are optionally identified by number and title. The
;; work type also may indicate a link to the opus document
;; that composes multiple scores into a collection.
(define-tag work '() (listof work-element/c))

(define-tag work-number '() (list/c string?))
(define-tag work-title '() (list/c string?))
(define-tag opus any/c '())

(define-tag movement-number '() (list/c string?))
(define-tag movement-title '() (list/c string?))

(define identification-element/c
  (or/c (tag/c creator)
        (tag/c rights)
        (tag/c encoding)
        (tag/c source)
        (tag/c relation)
        (tag/c miscellaneous)))

;; Identification contains basic metadata about the score.
;; It includes the information in MuseData headers that may
;; apply at a score-wide, movement-wide, or part-wide level.
(define-tag identification '()
  (listof identification-element/c))

;; The creator element is used for the creators of the
;; score. The type attribute is used to distinguish
;; different creative contributions. Thus, there can be
;; multiple creators within an identification. Standard type
;; values are composer, lyricist, and arranger. Other type
;; values may be used for different types of creative roles.
;; The type attribute should usually be used even if there
;; is just a single creator element. 
(define-tag creator
  (attrs-must/c 'type string?)
  (list/c string?))


