#lang racket/base

(provide direction ~direction directionₑ
         direction-type ~direction-type direction-typeₑ
         ;; ---
         dynamics ~dynamics dynamicsₑ
         ;; ---
         
         metronome ~metronome metronomeₑ
         beat-unit
         beat-unit-dot
         per-minute
         ;; ---
         sound ~sound soundₑ
         ;; ---
         print ~print printₑ
         ;; ---
         direction-has-voice?
         direction-voice-string
         )

(require racket/contract/base
         racket/match
         syntax/parse/define
         (submod txexpr safe)
         "str-number.rkt"
         "note.rkt"
         "note-type.rkt"
         "voice.rkt"
         "note-type.rkt"
         "editorial.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")

(define yes-no/c (or/c "yes" "no"))

(define above-below/c (or/c "above" "below"))

(define-syntax-class yes-no [pattern "yes"] [pattern "no"])

(define-syntax-class above-below [pattern "above"] [pattern "below"])

;; ---------------------------------------------------------

;; Musical directions used for expression marks, such as tempo, style,
;; dynamics, etc.
(define-tag direction
  (and/c (attrs-may/c 'placement above-below/c)
         (attrs-may/c 'directive yes-no/c))
  (listof (or/c (tag/c direction-type)
                (tag/c offset)
                (tag/c footnote)
                (tag/c level)
                (tag/c voice)
                (tag/c staff)
                (tag/c sound))))

(define-tag direction-type
  '()
  (listof (or/c (tag/c rehearsal)
                (tag/c segno)
                (tag/c words)
                (tag/c coda)
                (tag/c wedge)
                (tag/c dynamics)
                (tag/c dashes)
                (tag/c bracket)
                (tag/c pedal)
                (tag/c metronome)
                (tag/c octave-shift)
                (tag/c harp-pedals)
                (tag/c damp)
                (tag/c damp-all)
                (tag/c eyeglasses)
                (tag/c string-mute)
                (tag/c scordatura)
                (tag/c image)
                (tag/c principal-voice)
                (tag/c accordion-registration)
                (tag/c percussion)
                (tag/c other-direction))))

;; ---------------------------------------------------------

(define-syntax-class directionₑ
  #:attributes []
  #:datum-literals [placement directive]
  [pattern {~direction
            ({~alt {~optional [placement :above-below]}
                   {~optional [directive :yes-no]}}
             ...)
            (:direction-typeₑ
             ...+
             ;; TODO: offset
             :%editorial-voice
             {~optional :staffₑ}
             {~optional :soundₑ})}])

(define-syntax-class direction-typeₑ
  #:attributes []
  [pattern {~direction-type () (:direction-type-element)}])

(define-splicing-syntax-class direction-type-element
  ;; TODO: rehearsal, segno, etc.
  [pattern {~seq :dynamicsₑ ...+}]
  ;; TODO: dashes, bracket, etc.
  [pattern {~seq :metronomeₑ}])

;; ---------------------------------------------------------

(define-simple-macro (define-empty-tags t:id ...)
  (begin (define-tag t '() '()) ...))

(define-tag dynamics any/c
  (listof (or/c (tag/c p) (tag/c pp) (tag/c ppp) (tag/c pppp)
                (tag/c ppppp) (tag/c pppppp)
                (tag/c f) (tag/c ff) (tag/c fff) (tag/c ffff)
                (tag/c fffff) (tag/c ffffff)
                (tag/c mp) (tag/c mf))))
(define-empty-tags
  p pp ppp pppp ppppp pppppp
  f ff fff ffff fffff ffffff
  mp mf)

(define-tag metronome any/c
  (or/c (list/c (tag/c beat-unit) (tag/c per-minute))
        (list/c (tag/c beat-unit) (tag/c beat-unit-dot) (tag/c per-minute))))

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

(define-syntax-class dynamicsₑ
  #:attributes []
  [pattern {~dynamics _ (:dynamics-element ...)}])

(define-syntax-class dynamics-element
  #:attributes []
  [pattern {~p () ()}]
  [pattern {~pp () ()}]
  [pattern {~ppp () ()}]
  [pattern {~pppp () ()}]
  [pattern {~ppppp () ()}]
  [pattern {~pppppp () ()}]
  [pattern {~f () ()}]
  [pattern {~ff () ()}]
  [pattern {~fff () ()}]
  [pattern {~ffff () ()}]
  [pattern {~fffff () ()}]
  [pattern {~ffffff () ()}]
  [pattern {~mp () ()}]
  [pattern {~mf () ()}])

;; ---------------------------------------------------------

(define-syntax-class metronomeₑ
  #:attributes []
  [pattern {~metronome
            _
            (b:beat-unit/dots pm:per-minuteₑ)}])

(define-syntax-class per-minuteₑ
  #:attributes [per-minute-string]
  [pattern {~per-minute () (per-minute:str)}
    #:attr per-minute-string (@ per-minute.string)])

(define-syntax-class soundₑ
  #:attributes []
  [pattern {~sound _ _}])

;; ---------------------------------------------------------

(define-tag print any/c any/c)

(define-syntax-class printₑ
  #:attributes []
  ;; TODO
  [pattern {~print _ _}])

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

