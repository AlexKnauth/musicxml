#lang racket/base

;; Notes and stuff!

(provide (all-defined-out))

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "attributes.rkt"
         "duration.rkt"
         "voice.rkt"
         "util/tag.rkt")
(module+ test
  (require rackunit))

(define start-stop/c (or/c "start" "stop"))

;; The step type represents a step of the diatonic scale,
;; represented using the English letters A through G.
(define str-step/c
  (or/c "A" "B" "C" "D" "E" "F" "G"))

;; The note-type type indicates the graphic note type,
;; such as quarter or eighth.
(define str-note-type/c
  (or/c "1024th" "512th" "256th" "128th" "64th" "32nd" "16th"
        "eighth" "quarter" "half" "whole"
        "breve" "long" "maxima"))


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

(define-tag rest '() '())

(define-tag chord '() '())

(define-tag pitch '()
  (or/c (list/c (tag/c step) (tag/c octave))
        (list/c (tag/c step) (tag/c alter) (tag/c octave))))

(define-tag step '() (list/c str-step/c))
(define-tag alter '() (list/c str-decimal?))
(define-tag octave '() (list/c str-integer?))

(define-tag unpitched '()
  (or/c (list/c)
        (list/c (tag/c display-step) (tag/c display-octave))))

(define-tag display-step '() (list/c str-step/c))
(define-tag display-octave '() (list/c str-integer?))

(define-tag tie (attrs-must/c 'type start-stop/c) '())

(define-tag type '() (list/c str-note-type/c))

(define-tag dot '() '())

;; TODO: restrict the elements inside notations
(define-tag notations '() any/c)

(define-tag tied (attrs-must/c 'type start-stop/c) '())

(define-tag cue '() '())
(define-tag grace any/c '())

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

