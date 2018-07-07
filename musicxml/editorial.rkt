#lang racket/base

(provide %editorial
         %editorial-voice
         staff ~staff staffₑ)

(require racket/contract/base
         "str-number.rkt"
         "voice.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")

;; ---------------------------------------------------------

(define-splicing-syntax-class %editorial
  #:attributes []
  [pattern {~seq {~optional :footnoteₑ}
                 {~optional :levelₑ}}])

(define-splicing-syntax-class %editorial-voice
  #:attributes []
  [pattern {~seq {~optional :footnoteₑ}
                 {~optional :levelₑ}
                 {~optional :voiceₑ}}])

;; ---------------------------------------------------------

;; Staff assignment is only needed for music notated on
;; multiple staves. Used by both notes and directions. Staff
;; values are numbers, with 1 referring to the top-most staff
;; in a part.
(define-tag staff '() (list/c str-positive-integer?))

(define-syntax-class staffₑ
  #:attributes [staff-number]
  [pattern {~staff () (staff:str-pos-int)}
    #:attr staff-number (@ staff.number)])

;; ---------------------------------------------------------

(define-tag footnote any/c (list/c string?))
(define-tag level any/c (list/c string?))

(define-syntax-class footnoteₑ
  #:attributes [footnote-string]
  [pattern {~footnote _ (footnote:str)}
    #:attr footnote-string (@ footnote.string)])

(define-syntax-class levelₑ
  #:attributes [level-string]
  [pattern {~level _ (level:str)}
    #:attr level-string (@ level.string)])

;; ---------------------------------------------------------

