#lang racket/base

(provide note-type/dots
         type ~type typeₑ
         dot ~dot dotₑ
         ;; ---
         beat-unit/dots
         beat-unit ~beat-unit beat-unitₑ
         beat-unit-dot ~beat-unit-dot beat-unit-dotₑ
         ;; ---
         str-note-type
         str-note-type/c)

(require racket/contract/base
         racket/match
         syntax/parse/define
         "util/tag.rkt"
         "util/stxparse.rkt")
(module+ test
  (require rackunit))

;; ---------------------------

;; The note-type type indicates the graphic note type,
;; such as quarter or eighth. It does not include dots.
(define str-note-type/c
  (or/c "1024th" "512th" "256th" "128th" "64th" "32nd" "16th"
        "eighth" "quarter" "half" "whole"
        "breve" "long" "maxima"))

(define-syntax-class str-note-type
  #:attributes [duration/whole]
  [pattern "whole"   #:attr duration/whole 1]
  [pattern "half"    #:attr duration/whole 1/2]
  [pattern "quarter" #:attr duration/whole 1/4]
  [pattern "eighth"  #:attr duration/whole 1/8]
  [pattern "16th"    #:attr duration/whole 1/16]
  [pattern "32nd"    #:attr duration/whole 1/32]
  [pattern "64th"    #:attr duration/whole 1/64]
  [pattern "128th"   #:attr duration/whole 1/128]
  [pattern "256th"   #:attr duration/whole 1/256]
  [pattern "512th"   #:attr duration/whole 1/512]
  [pattern "1024th"  #:attr duration/whole 1/1024]
  [pattern "breve"   #:attr duration/whole 2]
  [pattern "long"    #:attr duration/whole 4]
  [pattern "maxima"  #:attr duration/whole 8])

;; Integer -> ExactRational
(define (dots-multiplier d)
  (match d
    [0 1]
    [1 3/2]
    [_ (- 2 (/ (expt 2 d)))]))

;; ---------------------------

;; NOTE:
;;   These "durations" are usually only for notation
;;   purposes. For example, triplets might have `duration`
;;   elements with a factor of three less than other notes,
;;   but they might be notated as just eighth notes in
;;   groups of three. In that case, this `duration/whole`
;;   attribute will be the same as an eighth note, but the
;;   `duration` element will show the true duration.

(define-tag type '() (list/c str-note-type/c))
(define-tag dot '() '())

(define-tag beat-unit '() (list/c str-note-type/c))
(define-tag beat-unit-dot '() '())

;; ---------------------------

(define-splicing-syntax-class note-type/dots
  #:attributes [note-type-string dots duration/whole]
  [pattern {~seq t:typeₑ d:dotₑ ...}
    #:attr note-type-string (@ t.note-type-string)
    #:attr dots (length (@ d))
    #:attr duration/whole (* (@ t.duration/whole)
                             (dots-multiplier (@ dots)))])

(define-syntax-class typeₑ
  #:attributes [note-type-string duration/whole]
  [pattern {~type () (note-type:str-note-type)}
    #:attr note-type-string (syntax-e #'note-type)
    #:when (str-note-type/c (@ note-type-string))
    #:attr duration/whole (@ note-type.duration/whole)])

(define-syntax-class dotₑ
  #:attributes []
  [pattern {~dot () ()}])

;; ---------------------------

(define-splicing-syntax-class beat-unit/dots
  #:attributes [beat-unit-string dots duration/whole]
  [pattern {~seq t:beat-unitₑ d:beat-unit-dotₑ ...}
    #:attr beat-unit-string (@ t.beat-unit-string)
    #:attr dots (length (@ d))
    #:attr duration/whole (* (@ t.duration/whole)
                             (dots-multiplier (@ dots)))])

(define-syntax-class beat-unitₑ
  #:attributes [beat-unit-string duration/whole]
  [pattern {~beat-unit () (note-type:str-note-type)}
    #:attr beat-unit-string (syntax-e #'note-type)
    #:when (str-note-type/c (@ beat-unit-string))
    #:attr duration/whole (@ note-type.duration/whole)])

(define-syntax-class beat-unit-dotₑ
  #:attributes []
  [pattern {~beat-unit-dot () ()}])

;; ---------------------------------------------------------

(module+ test
  (define-simple-macro (t-dur stx ...)
    (syntax-parse #'[stx ...]
      [[:note-type/dots] (@ duration/whole)]))

  (check-equal? (t-dur (type "quarter"))       1/4)
  (check-equal? (t-dur (type "quarter") (dot)) 3/8)
  (check-equal? (t-dur (type "half"))                    1/2)
  (check-equal? (t-dur (type "half") (dot))              3/4)
  (check-equal? (t-dur (type "half") (dot) (dot))        7/8)
  (check-equal? (t-dur (type "half") (dot) (dot) (dot)) 15/16)
  (check-equal? (t-dur (type "16th"))                    1/16)
  (check-equal? (t-dur (type "16th") (dot))              3/32)
  (check-equal? (t-dur (type "16th") (dot) (dot) (dot)) 15/128)
  )

