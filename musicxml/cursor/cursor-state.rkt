#lang racket/base

(provide cursor-state-time
         cursor-state-start
         cursor-state-start-measure-number
         cursor-state-start-measure
         cursor-state-update/music-data)

(require racket/contract/base
         racket/match
         racket/math
         (submod txexpr safe)
         "../measure.rkt"
         "../music-data.rkt"
         "../attributes.rkt"
         "../forward-backup.rkt"
         "../note.rkt"
         "../figured-bass.rkt"
         "../direction.rkt"
         "../barline.rkt"
         (except-in "../harmony.rkt" alter)
         (prefix-in t: "../timed/time.rkt")
         "../util/tag.rkt")

;; A CursorState is a
;;   (cursor-state [Hashof Integer PositiveInteger] Time)
(struct cursor-state
  [; how many "divisions" per quarter note, for every measure
   measure-divisions ; : [Hashof Integer PositiveInteger]
   ; the current time (contains divisions in terms of measure-divisions)
   time
   ])

(define (cursor-state-measure cs)
  (t:time-measure (cursor-state-time cs)))

;; -> CursorState
(define (cursor-state-start)
  (cursor-state (hash) (t:time 0 0)))

;; CusrorState Integer -> CursorState
(define (cursor-state-start-measure-number cs n)
  (struct-copy cursor-state cs
    [time (t:time n 0)]))

;; CursorState Measure -> CursorState
(define (cursor-state-start-measure cs m)
  (define n->div (cursor-state-measure-divisions cs))
  (define n (measure-number m))
  (struct-copy cursor-state cs
    [time (t:time n 0)]
    [measure-divisions
     (cond [(measure-has-divisions? m)
            (hash-set n->div n (measure-divisions m))]
           [else
            (unless (hash-has-key? n->div (sub1 n))
              (error 'cursor-state-start-measure
                     (string-append
                      "either the measure must have its own divisions, "
                      "or it must carry over from the previous measure")))
            (hash-set n->div n (hash-ref n->div (sub1 n)))])]))

;; CursorState -> [MaybeDivisions]
(define (cursor-state-divisions cs)
  (hash-ref (cursor-state-measure-divisions cs)
            (cursor-state-measure cs)
    #f))

;; ---------------------------------------------------------

;; CursorState MusicData -> CursorState
(define (cursor-state-update/music-data cs m)
  (match m
    [(attributes _ _)     (cursor-state-update/attributes cs m)]
    [(barline _ elements) (cursor-state-update/barline cs m)]
    [(note _ _)           (cursor-state-update/note cs m)]
    [(forward _ _)        (+time cs (forward-duration-divisions m))]
    [(backup _ _)         (+time cs (- (backup-duration-divisions m)))]
    [(figured-bass _ _)   (+time cs (figured-bass-duration-divisions m))]

    [(direction _ _) cs]
    [(harmony _ _) cs]
    [(? (tag/c print)) cs]
    [(? (tag/c sound)) cs]))

;; CursorState Attributes -> CursorState
(define (cursor-state-update/attributes cs a)
  (match a
    [(attributes _ elements)
     (for/fold ([cs cs])
               ([e (in-list elements)])
       (match e
         [(? (tag/c footnote)) cs]
         [(? (tag/c level)) cs]
         [(divisions _ (list div-str))
          (set-divisions cs (string->number div-str))]
         [(key _ _) cs]
         [(clef _ _) cs]
         ;; time signature does not affect number of divisions per quarter note
         [(time _ _) cs]
         ;; TODO: more
         ))]))

;; CusrorState Barline -> CursorState
(define (cursor-state-update/barline cs b)
  (define t (cursor-state-time cs))
  (match b
    [(barline attrs elements)
     (define location (attr-ref b 'location "right"))
     (when (attrs-have-key? b 'divisions)
       (error 'cursor-state-update
              "TODO: what do divisions on a barline mean?"))
     (cond
       [(ormap (or/c (tag/c ending) (tag/c repeat)) elements)
        (error 'cursor-state-update "TODO: update on endings and repeats")]
       [else
        cs])]))

;; CursorState Note -> CursorState
(define (cursor-state-update/note cs n)
  (cond
    ;; If it's the second/third/... note of a chord, don't advance
    ;; the cursor. Any state changes were *already handled* by the
    ;; first note of the chord.
    [(note-has-chord? n) cs]
    [else
     (+time cs (note-duration-divisions n))]))

;; ---------------------------------------------------------

;; set-divisions : CursorState PositiveInteger -> CursorState
(define (set-divisions cs div)
  (define m (cursor-state-measure cs))
  (define t (cursor-state-time cs))
  (define m->div (cursor-state-measure-divisions cs))
  (unless (t:time-beginning-of-measure? t)
    (error 'cursor-update-state
           "can only set divisions at the beginning of a measure"))
  (struct-copy cursor-state cs
    [measure-divisions (hash-set m->div m div)]))

;; +time : CursorState Divisions -> CursorState
(define (+time cs dt)
  (define t (cursor-state-time cs))
  (struct-copy cursor-state cs
    [time (t:time+ t dt)]))

;; ---------------------------------------------------------

