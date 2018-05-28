#lang racket/base

(provide direction
         direction-type
         metronome
         beat-unit
         beat-unit-dot
         per-minute
         sound
         ;; ---
         direction-has-voice?
         direction-voice-string
         )

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "note.rkt"
         "voice.rkt"
         "util/tag.rkt")

(define yes-no/c (or/c "yes" "no"))

(define above-below/c (or/c "above" "below"))

;; ---------------------------------------------------------

;; Musical directions used for expression marks, such as tempo, style,
;; dynamics, etc.
(define-tag direction
  (and/c (attrs-may/c 'placement above-below/c)
         (attrs-may/c 'directive yes-no/c))
  ;; TODO: restrict the elements inside
  any/c)

(define-tag direction-type '() any/c)

;; ---------------------------------------------------------

(define-tag metronome any/c
  (or/c (list/c (tag/c beat-unit) (tag/c per-minute))
        (list/c (tag/c beat-unit) (tag/c beat-unit-dot) (tag/c per-minute))))

(define-tag beat-unit '() (list/c str-note-type/c))

(define-tag beat-unit-dot '() '())

;; How it will display. Can have any string but usually a number-ish thing
(define-tag per-minute any/c (list/c string?))

;; The sound tag can be either within a direction tag or
;; outside it as music-data by itself.
;; Currently the sound element determines the tempo when the song is played,
;; but in the future it might determine more things about how it is played
;; than just tempo.
(define-tag sound
  (and/c (attrs-may/c 'tempo str-nonnegative-integer?)
         (attrs-may/c 'dynamics str-nonnegative-integer?))
  '())

;; ---------------------------------------------------------

;; private
;; Any -> Boolean
(define (voice? v)
  (match v [(voice _ _) #true] [_ #false]))

;; direction-has-voice? : Direction -> Boolean
(define (direction-has-voice? fw)
  (match fw
    [(direction _ elements)
     (ormap voice? elements)]))

;; direction-voice-string : Direction -> [Maybe VoiceStr]
(define (direction-voice-string fw)
  (match fw
    [(direction _ (list _ ... (? voice? v) _ ...))
     (voice-string v)]
    [_
     #false]))

;; ---------------------------------------------------------

