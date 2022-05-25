#lang racket/base

;; Pitch, Unpitched, Rest, Root, Bass

(provide pitch ~pitch pitchₑ
         unpitched ~unpitched unpitchedₑ
         rest ~rest restₑ
         root ~root rootₑ
         bass ~bass bassₑ
         ;; ---
         step ~step stepₑ
         alter ~alter alterₑ
         octave ~octave octaveₑ
         display-step ~display-step display-stepₑ
         display-octave ~display-octave display-octaveₑ
         root-step ~root-step root-stepₑ
         root-alter ~root-alter root-alterₑ
         bass-step ~bass-step bass-stepₑ
         bass-alter ~bass-alter bass-alterₑ
         ;; ---
         str-step/c str-step
         )

(require racket/contract/base
         "str-number.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")

;; ---------------------------------------------------------

;; The step type represents a step of the diatonic scale,
;; represented using the English letters A through G.
(define str-step/c
  (or/c "A" "B" "C" "D" "E" "F" "G"))

(define-syntax-class str-step
  [pattern "A"]
  [pattern "B"]
  [pattern "C"]
  [pattern "D"]
  [pattern "E"]
  [pattern "F"]
  [pattern "G"])

;; ---------------------------------------------------------

;; Pitch is represented as a combination of the step of the
;; diatonic scale, the chromatic alteration, and the octave.
;; The step element uses the English letters A through G. 
;; The alter element represents chromatic alteration in
;; number of semitones (e.g., -1 for flat, 1 for sharp).
;; Decimal values like 0.5 (quarter tone sharp) are 
;; used for microtones. The octave element is represented
;; by an integer where 4 indicates the octave
;; started by middle C.
(define-tag pitch '()
  (or/c (list/c (tag/c step) (tag/c octave))
        (list/c (tag/c step) (tag/c alter) (tag/c octave))))

(define-tag step '() (list/c str-step/c))
(define-tag alter '() (list/c str-decimal?))
(define-tag octave '() (list/c str-integer?))

;; The unpitched element indicates musical elements that are
;; notated on the staff but lack definite pitch, such as
;; unpitched percussion and speaking voice. Like pitches, it
;; uses step and octave elements to indicate placement on the
;; staff, following the current clef. If percussion clef is
;; used, the display-step and display-octave elements are
;; interpreted as if in treble clef, with a G in octave 4 on
;; line 2. If not present, the note is placed on the middle
;; line of the staff, generally used for a one-line staff.
(define-tag unpitched '()
  (or/c (list/c)
        (list/c (tag/c display-step) (tag/c display-octave))))

;; The rest element indicates notated rests or silences. Rest
;; elements are usually empty, but placement on the staff can
;; be specified using display-step and display-octave
;; elements. 
(define-tag rest '()
  (or/c (list/c)
        (list/c (tag/c display-step) (tag/c display-octave))))

(define-tag display-step '() (list/c str-step/c))
(define-tag display-octave '() (list/c str-integer?))

;; The root type is used in `harmony` elements to indicate
;; the root of a chord.
(define-tag root '()
  (or/c (list/c (tag/c root-step))
        (list/c (tag/c root-step) (tag/c root-alter))))

(define-tag root-step any/c (list/c str-step/c))
(define-tag root-alter any/c (list/c str-decimal?))

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

;; ---------------------------------------------------------

(define-syntax-class pitchₑ
  #:attributes [step-string alter-semitones octave]
  [pattern {~pitch
            ()
            (~!
             :stepₑ
             {~optional :alterₑ #:defaults ([alter-semitones 0])}
             :octaveₑ)}])

(define-syntax-class unpitchedₑ
  #:attributes []
  [pattern {~unpitched () ()}]
  [pattern {~unpitched () (:display-stepₑ :display-octaveₑ)}])

(define-syntax-class restₑ
  #:attributes []
  [pattern {~rest () ()}]
  [pattern {~rest () (:display-stepₑ :display-octaveₑ)}])

(define-syntax-class rootₑ
  #:attributes [step-string alter-semitones]
  [pattern {~root
            ()
            (:root-stepₑ
             {~optional :root-alterₑ #:defaults ([alter-semitones 0])})}])

(define-syntax-class bassₑ
  #:attributes [step-string alter-semitones]
  [pattern {~root
            ()
            (:bass-stepₑ
             {~optional :bass-alterₑ #:defaults ([alter-semitones 0])})}])

;; -----------------

(define-syntax-class stepₑ
  #:attributes [step-string]
  [pattern {~step () (step:str)}
    #:when (str-step/c (@ step.string))
    #:attr step-string (@ step.string)])

(define-syntax-class alterₑ
  #:attributes [alter-semitones]
  [pattern {~alter () (semitones:str-int)}
    #:attr alter-semitones (@ semitones.number)])

(define-syntax-class octaveₑ
  #:attributes [octave]
  [pattern {~octave () (octave*:str-int)}
    #:attr octave (@ octave*.number)])

(define-syntax-class display-stepₑ
  #:attributes [step-string]
  [pattern {~display-step () (step:str)}
    #:when (str-step/c (@ step.string))
    #:attr step-string (@ step.string)])

(define-syntax-class display-octaveₑ
  #:attributes [octave]
  [pattern {~display-octave () (octave*:str-int)}
    #:attr octave (@ octave*.number)])

(define-syntax-class root-stepₑ
  #:attributes [step-string]
  [pattern {~root-step () (step:str)}
    #:when (str-step/c (@ step.string))
    #:attr step-string (@ step.string)])

(define-syntax-class root-alterₑ
  #:attributes [alter-semitones]
  [pattern {~root-alter () (semitones:str-int)}
    #:attr alter-semitones (@ semitones.number)])

(define-syntax-class bass-stepₑ
  #:attributes [step-string]
  [pattern {~bass-step () (step:str)}
    #:when (str-step/c (@ step.string))
    #:attr step-string (@ step.string)])

(define-syntax-class bass-alterₑ
  #:attributes [alter-semitones]
  [pattern {~bass-alter () (semitones:str-int)}
    #:attr alter-semitones (@ semitones.number)])

;; ---------------------------------------------------------

