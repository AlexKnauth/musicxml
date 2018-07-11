#lang racket/base

;; Notes and stuff!

(provide (all-defined-out))

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "attributes.rkt"
         "pitch.rkt"
         "duration.rkt"
         "voice.rkt"
         "editorial.rkt"
         "note-type.rkt"
         "util/stxparse.rkt"
         "util/tag.rkt")
(module+ test
  (require rackunit))

(define start-stop/c (or/c "start" "stop"))

(define str-stem-value/c (or/c "down" "up" "none" "double"))
(define-syntax-class str-stem-value
  [pattern "down"]
  [pattern "up"]
  [pattern "none"]
  [pattern "double"])

;; ---------------------------------------------------------


;; TODO: restrict the elements inside note
(define-tag note any/c
  ;; TODO: restrict the order, since that's important
  (listof (or/c (tag/c chord) (tag/c pitch) (tag/c rest)
                (tag/c unpitched) (tag/c duration) (tag/c tie)
                (tag/c cue) (tag/c grace) (tag/c instrument)
                (tag/c footnote) (tag/c level) (tag/c voice)
                (tag/c type) (tag/c dot) (tag/c accidental)
                (tag/c time-modification) (tag/c stem)
                (tag/c notehead) (tag/c notehead-text) (tag/c staff)
                (tag/c beam) (tag/c notations) (tag/c lyric)
                (tag/c play))))

(define-tag chord '() '())

(define-tag tie (attrs-must/c 'type start-stop/c) '())

(define-tag stem '() (list/c str-stem-value/c))

;; TODO: restrict the elements inside notations
(define-tag notations '()
  (listof (or/c (tag/c footnote) (tag/c level)
                (tag/c tied) (tag/c slur) (tag/c tuplet) (tag/c glissando)
                (tag/c slide) (tag/c ornaments) (tag/c technical)
                (tag/c articulations) (tag/c dynamics) (tag/c fermata)
                (tag/c arpeggiate) (tag/c non-arpeggiate)
                (tag/c accidental-mark) (tag/c other-notation))))

(define-tag tied (attrs-must/c 'type start-stop/c) '())

(define-tag cue '() '())
(define-tag grace any/c '())

;; ---------------------------------------------------------

(define-syntax-class chordₑ
  #:attributes []
  [pattern {~chord () ()}])

(define-syntax-class graceₑ
  #:attributes []
  [pattern {~grace _ ()}])

(define-syntax-class cueₑ
  #:attributes []
  [pattern {~cue () ()}])

;; ---------------------------------------------------------

(define-syntax-class noteₑ
  #:attributes [grace?
                cue?
                chord? pitch unpitched rest]
  [pattern {~note _
             ({~or {~seq grace:graceₑ :%full-note :tie-info}
                   {~seq cue:cueₑ :%full-note :durationₑ}
                   {~seq :%full-note :durationₑ :tie-info}}
              :%editorial-voice
              {~optional :note-type/dots}
              ;; TODO: accidental, etc.
              {~optional :stemₑ}
              ;; TODO: notehead, etc.
              {~optional :staffₑ}
              ;; TODO: beam
              :notationsₑ
              ...
              ;; TODO: lyric, etc.
              )}
    #:attr grace? (and (@ grace) #true)
    #:attr cue? (and (@ cue) #true)])

(define-splicing-syntax-class %full-note
  #:attributes [chord? pitch unpitched rest]
  [pattern {~seq {~optional chord:chordₑ}
                 {~or pitch:pitchₑ unpitched:unpitchedₑ rest:restₑ}}
    #:attr chord? (and (@ chord) #true)])

(define-syntax-class stemₑ
  #:attributes []
  ;; TODO
  [pattern {~stem () (:str-stem-value)}])

(define-splicing-syntax-class tie-info
  #:attributes [start? stop?]
  #:datum-literals [type]
  [pattern {~seq {~alt {~optional {~tie ([type (~and start "start")]) ()}}
                       {~optional {~tie ([type (~and stop "stop")]) ()}}}
                 ...}
    #:attr start? (and (@ start) #t)
    #:attr stop? (and (@ stop) #t)])

(define-splicing-syntax-class tied-info
  #:attributes [start? stop?]
  #:datum-literals [type]
  [pattern {~seq {~alt {~optional {~tied ([type (~and start "start")]) ()}}
                       {~optional {~tied ([type (~and stop "stop")]) ()}}}
                 ...}
    #:attr start? (and (@ start) #t)
    #:attr stop? (and (@ stop) #t)])

;; ---------------------------------------------------------

;; Notations are musical notations, not XML notations. Multiple
;; notations are allowed in order to represent multiple editorial
;; levels.

(define-syntax-class notationsₑ
  #:attributes []
  [pattern {~notations ()
                       (:%editorial
                        :tied-info
                        ;; TODO: tied, slur, tuplet, etc.
                        )}])

;; ---------------------------------------------------------

;; note-duration-divisions : Note -> PositiveDivisions
(define (note-duration-divisions n)
  ;; Any -> Boolean
  (define (duration? v)
    (match v [(duration _ _) #true] [_ #false]))
  (match n
    [(note _ (list _ ... (? duration? d) _ ...))
     (duration-divisions d)]))

;; note-pitch/rest/unpitched : Note -> (U Pitch Rest Unpitched)
(define (note-pitch/rest/unpitched n)
  ;; Any -> Boolean
  (define (p/r/u? v)
    (match v
      [(or (pitch _ _) (rest _ _) (unpitched _ _)) #true]
      [_ #false]))
  ;; the p/r/u must only come first or after these
  (define pre?
    (or/c (tag/c cue) (tag/c grace) (tag/c chord)))
  (match n
    [(note _ (list (? pre? _) ... (? p/r/u? p/r/u) _ ...))
     p/r/u]))

;; ---------------------------------------------------------

;; note-has-chord? : Note -> Boolean
;; Returns true if the note is marked as being in a chord
;; with the previous note, false otherwise. Every note
;; except the first note in a chord will be marked like
;; this.
(define (note-has-chord? n)
  ;; Any -> Boolean
  (define (chord? v)
    (match v [(chord _ _) #true] [_ #false]))
  (match n
    [(note _ elements)
     (ormap chord? elements)]))

;; node-add-chord : Note -> Note
;; Marks the note as being in a chord with the previous
;; note.
;; Call this function on every note except the first note of
;; a chord.
(define (note-add-chord n)
  (match n
    ;; don't add anything if it's already there
    [(note _ (list-rest (chord _ _) _))  n]
    [(note _ (list-rest (cue _ _) (chord _ _) _))  n]
    [(note _ (list-rest (grace _ _) (chord _ _) _))  n]

    ;; otherwise add it just before the pitch/rest/unpitched choice
    [(note attrs
           (and elements
                (list-rest (or (pitch _ _) (rest _ _) (unpitched _ _))
                           _)))
     (note attrs (cons (chord '() '()) elements))]

    [(note attrs
           (list-rest
            (and fst (cue _ _))
            (and rst
                 (list-rest (or (pitch _ _) (rest _ _) (unpitched _ _))
                            _))))
     (note attrs (list* fst (chord '() '()) rst))]

    [(note attrs
           (list-rest
            (and fst (grace _ _))
            (and rst
                 (list-rest (or (pitch _ _) (rest _ _) (unpitched _ _))
                            _))))
     (note attrs (list* fst (chord '() '()) rst))]))

;; ---------------------------------------------------------

;; note-has-voice? : Note -> Boolean
(define (note-has-voice? n)
  ;; Any -> Boolean
  (define (voice? v)
    (match v [(voice _ _) #true] [_ #false]))
  (match n
    [(note _ elements)
     (ormap voice? elements)]))

;; note-voice-string : Note -> [Maybe VoiceStr]
(define (note-voice-string n)
  ;; Any -> Boolean
  (define (voice? v)
    (match v [(voice _ _) #true] [_ #false]))
  (match n
    [(note _ (list _ ... (? voice? v) _ ...))
     (voice-string v)]
    [_
     #false]))

;; note-assign-voice : Note/wo-voice String -> Note
;; Assigns a voice to the given note. That note must *not*
;; already have a voice.
(define (note-assign-voice n vc-str)
  (when (note-has-voice? n)
    (error 'note-assign-voice "note must not already have a voice: ~v" n))

  ;; it should add the voice after the "pre" elements and before the
  ;; "post" elements
  (define pre?
    (or/c (tag/c chord) (tag/c pitch) (tag/c rest)
          (tag/c unpitched) (tag/c duration) (tag/c tie)
          (tag/c cue) (tag/c grace) (tag/c instrument)
          (tag/c footnote) (tag/c level)))
  (define post?
    (or/c (tag/c type) (tag/c dot) (tag/c accidental)
          (tag/c time-modification) (tag/c stem) (tag/c notehead)
          (tag/c notehead-text) (tag/c staff) (tag/c beam)
          (tag/c notations) (tag/c lyric) (tag/c play)))

  (match n
    [(note attrs (list (? pre? pre) ... (? post? post) ...))
     (note attrs
           (append pre (list (voice '() (list vc-str))) post))]))

;; ---------------------------------------------------------

(module+ test
  (test-case "note-add-chord"
    ;; adds one if it doesn't exist yet
    (check-equal? (note-add-chord
                   (note '()
                         (list
                          (pitch '() (list (step '() '("C")) (octave '() '("4"))))
                          (duration '() '("2"))
                          (voice '() '("1"))
                          (type '() '("quarter"))
                          (notations '() '()))))
                  (note '()
                        (list
                         (chord '() '())
                         (pitch '() (list (step '() '("C")) (octave '() '("4"))))
                         (duration '() '("2"))
                         (voice '() '("1"))
                         (type '() '("quarter"))
                         (notations '() '()))))

    ;; doesn't add a second one
    (check-equal? (note-add-chord
                   (note '()
                         (list
                          (chord '() '())
                          (pitch '() (list (step '() '("C")) (octave '() '("4"))))
                          (duration '() '("2"))
                          (voice '() '("1"))
                          (type '() '("quarter"))
                          (notations '() '()))))
                  (note '()
                        (list
                         (chord '() '())
                         (pitch '() (list (step '() '("C")) (octave '() '("4"))))
                         (duration '() '("2"))
                         (voice '() '("1"))
                         (type '() '("quarter"))
                         (notations '() '()))))
    )

  (test-case "note-assign-voice"
    (check-equal? (note-assign-voice
                   (note '()
                         (list
                          (pitch '() (list (step '() '("C")) (octave '() '("4"))))
                          (duration '() '("2"))
                          (type '() '("quarter"))
                          (notations '() '())))
                   "3")
                  (note '()
                        (list
                         (pitch '() (list (step '() '("C")) (octave '() '("4"))))
                         (duration '() '("2"))
                         (voice '() '("3"))
                         (type '() '("quarter"))
                         (notations '() '()))))
    )
  )

