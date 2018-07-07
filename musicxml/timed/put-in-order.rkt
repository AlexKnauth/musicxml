#lang racket/base

(provide part/timed->part
         timed-music-data->measures)

(require (except-in racket/list rest)
         racket/match
         (only-in srfi/1 append-reverse)
         "time.rkt"
         "timed.rkt"
         "part-timed.rkt"
         "../cursor/cursor-state.rkt"
         "../score-partwise.rkt"
         "../measure.rkt"
         "../music-data.rkt"
         (except-in "../attributes.rkt" time)
         "../note.rkt"
         "../pitch.rkt"
         "../duration.rkt"
         )

;; The functions in this module assume that every note or
;; element that needs a voice already has one assigned.
;; Every chord must have all notes in the same voice.

;; These elements are expected to have voices:
;;  - note
;;  - forward
;;  - direction
;; And these elements are expected to not have voices:
;;  - backup
;;  - ???

;; ---------------------------

;; PartTimed -> Part
(define (part/timed->part p/t)
  (match-define (part/timed id tes) p/t)
  (define st (cursor-state-start))
  (define-values [st* measures]
    (timed-music-data->measures st tes))
  (part `([id ,id]) measures))

;; ---------------------------

;; State [Listof [Timed MusicData]]
;; ->
;; (values State [Listof Measure])
(define (timed-music-data->measures st tes)
  ;; `tes` stands for "timed elements"
  ;; `tems` stands for "timed element measures"
  (define tems (group-and-sort-by-measure tes))
  (for/fold ([st st]
             [rev-ms '()]
             #:result (values st (reverse rev-ms)))
            ([tem (in-list tems)])
    (define vcs (find-all-voices (map timed-value tem)))
    (define-values [st* m]
      (timed-music-data->measure st tem vcs))
    (values st* (cons m rev-ms))))

;; State [Listof [Timed MusicData]] [Listof Voice]
;; ->
;; (values State Measure)
;; All of the timed elements must come from the same measure
(define (timed-music-data->measure st tes vcs)
  ;; the all come from measure `n`
  (define n (timed-measure (first tes)))

  ;; start the state with measure `n`
  (define st/n (cursor-state-start-measure-number st n))

  ;; group the elements by voice
  (define voice-groups (group-by-voice vcs tes))

  (define-values [st* elements]
    (for/fold ([st st/n]
               [rev-elems '()]
               #:result (values st (reverse rev-elems)))
              ([v (in-list vcs)]
               [g (in-list voice-groups)])
      (define-values [st* elems*]
        (voice-timed-music-data->music-data* st g v))
      (values st* (append-reverse elems* rev-elems))))

  (values
   st*
   (measure `([number ,(number->string n)]) elements)))

;; State [Listof [Timed MusicData]] Voice
;; ->
;; (values State [Listof MusicData])
;; All of the timed elements must come from the same measure
;; and the same voice `vc` (if they have one)
(define (voice-timed-music-data->music-data* st tes vc)
  (define tes/sorted
    (sort tes timed-music-data<?))
  (for/fold ([st st]
             [rev-elems '()]
             #:result (values st (reverse rev-elems)))
            ([te (in-list tes/sorted)])
    (define-values [st* e*]
      (timed-music-data->music-data st te vc))
    (values st* (append-reverse e* rev-elems))))

;; State [Timed MusicData] Voice
;; ->
;; (values State [Listof MusicData])
(define (timed-music-data->music-data st te vc)
  (match-define (timed t e) te)
  ;; adjust the time so that `st*` is at time `t`
  (define-values [st* adj] (adjust-time st t vc))
  ;; `e` takes the state from `st*` to `st**`
  (define st** (cursor-state-update/music-data st* e))
  (values st** (append adj (list e))))

;; ---------------------------------------------------------

;; [Listof Voice] [Listof [Timed MusicData]]
;; ->
;; [Listof [Listof [Timed MusicData]]]
;; the result list is in the same order as `vcs`
(define (group-by-voice vcs tes)
  ;; if things aren't assigned a voice, they get grouped
  ;; with the first voice
  (define fst-vc (first vcs))
  ;; an element `e` matches a voice `vc` when either the
  ;; the element has a voice equal to `vc` or the element
  ;; has no voice and `vc` is `fst-vc`
  ;; MusicData Voice -> Boolean
  (define (element-matches-voice? e v)
    (cond [(music-data-has-voice? e)
           (string=? (music-data-voice-string e) v)]
          [else
           (string=? fst-vc v)]))
  (for/list ([v (in-list vcs)])
    (for/list ([te (in-list tes)]
               #:when (element-matches-voice? (timed-value te) v))
      te)))

;; ---------------------------------------------------------

;; State Time Voice
;; ->
;; (values State [Listof MusicData])
;; the times must be in the same measure
(define (adjust-time st new-t vc)
  (define old-t (cursor-state-time st))
  (define adj
    (cond [(time=? old-t new-t) '()]
          [(time<? old-t new-t)
           ;; add a rest to make up the difference
           (list (note-assign-voice
                  (divisions->rest (time-divisions∆ old-t new-t))
                  vc))]
          [else ; new-t < old-t
           ;; add a backup to make up the difference
           (list (divisions->backup (time-divisions∆ new-t old-t)))]))
  (define st*
    (for/fold ([st st])
              ([e (in-list adj)])
      (cursor-state-update/music-data st e)))
  ;; check that it actually got to the goal of `new-t`
  (unless (time=? new-t (cursor-state-time st*))
    (error 'adjust-time-as-music-data
           "failed to get from time ~v to time ~v\n  instead got to: ~v"
           old-t new-t (cursor-state-time st*)))
  (values st* adj))

;; ---------------------------------------------------------

;; PositiveDivisions -> RestNote
;; creates a rest element without a voice
(define (divisions->rest div)
  (note '()
    (list (rest '() '())
          (duration '() (list (number->string div))))))

;; PositiveDivisions -> Backup
(define (divisions->backup div)
  (backup '()
    (list (duration '() (list (number->string div))))))

;; ---------------------------------------------------------

(define (find-all-voices es)
  (define rev-vcs
    (for/fold ([acc '()])
              ([e (in-list es)]
               #:when (music-data-has-voice? e)
               #:when (not (member (music-data-voice-string e) acc)))
      (cons (music-data-voice-string e) acc)))
  (cond
    [(empty? rev-vcs) (list "")]
    [else (reverse rev-vcs)]))

;; ---------------------------------------------------------

(define (attributes? v)
  (match v [(attributes _ _) #t] [_ #f]))
(define (direction? v)
  (match v [(direction _ _) #t] [_ #f]))

(define (timed-music-data<? a b)
  (match* [a b]
    [[(timed at av) (timed bt bv)]
     (or (time<? at bt)
         (and (time=? at bt)
              (same-time-music-data<? av bv)))]))

(define (same-time-music-data<? av bv)
  (cond
    [(attributes? av)
     (or (not (attributes? bv))
         (and (attributes? bv)
              (same-time-attributes<? av bv)))]
    [(direction? av)
     (and (not (attributes? bv))
          (not (direction? bv)))]
    [else
     #false]))

(define (same-time-attributes<? a b)
  (or (and (attributes-has-divisions? a)
           (not (attributes-has-divisions? b)))))

