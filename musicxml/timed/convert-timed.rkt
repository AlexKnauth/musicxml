#lang racket/base

(provide part->part/timed
         measures->timed-music-data)

(require racket/match
         (only-in (submod txexpr safe) attr-ref)
         "time.rkt"
         "timed.rkt"
         "part-timed.rkt"
         "../cursor/cursor-state.rkt"
         "../score-partwise.rkt"
         "../measure.rkt"
         )

;; ---------------------------
;; Processing A Part

;; Part -> PartTimed
(define (part->part/timed p)
  (match p
    [(part attrs measures)
     (define id (attr-ref p 'id))
     (define st (cursor-state-start))
     (define-values [st* timed-elements]
       (measures->timed-music-data st measures))
     (part/timed id timed-elements)]))

;; ---------------------------
;; Processing Measures

;; State [Listof Measure]
;; ->
;; (values State [Listof [Timed MusicData]])
(define (measures->timed-music-data st ms)
  (match ms
    ['() (values st '())]
    [(cons m rst)
     (define-values [st* tm]
       (measure->timed-music-data st m))
     (define-values [st** trst]
       (measures->timed-music-data st* rst))
     (values st** (append tm trst))]))

;; State Measure
;; ->
;; (values State [Listof [Timed MusicData]])
(define (measure->timed-music-data st m)
  (define st/m (cursor-state-start-measure st m))
  (match m
    [(measure _ elements)
     (music-data->timed-music-data* st/m elements)]))

;; ---------------------------
;; Processing MusicData

;; State [Listof MusicData]
;; ->
;; (values State [Listof [Timed MusicData]]
(define (music-data->timed-music-data* st elements)
  (match elements
    ['() (values st '())]
    [(cons e rst)
     (define-values [st* te]
       (music-data->timed-music-data st e))
     (define-values [st** trst]
       (music-data->timed-music-data* st* rst))
     (values st** (cons te trst))]))

;; State MusicData
;; ->
;; (values State [Timed MusicData])
(define (music-data->timed-music-data st e)
  (define t (cursor-state-time st))
  (define st* (cursor-state-update/music-data st e))
  (values st* (timed t e)))

;; ---------------------------

