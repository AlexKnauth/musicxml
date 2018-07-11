#lang racket/base

(provide timed-music-data->tila)

(require racket/match
         "timed.rkt"
         "time-period.rkt"
         "timed-lasting.rkt"
         ;; ---
         musicxml/music-data
         musicxml/note
         musicxml/forward-backup
         )

;; For "time-period overlapping" purposes:
;;  - a note or forward element has lasting-duration
;;  - a backup element has zero lasting-duration

;; ---------------------------------------------------------

;; music-data->lasting : MusicData -> [Lasting MusicData]
;; Converts a music-data element into the "lasting" version
;; for time-period overlapping purposes.
(define (music-data->lasting md)
  (lasting (music-data-lasting-duration-divisions md) md))

;; ---------------------------------------------------------

;; timed-music-data->tila : [Timed MusicData] -> [Tila MusicData]
;; Converts a timed music-data element into the "tila" version
;; for time-period overlapping purposes.
(define (timed-music-data->tila tmd)
  (match tmd
    [(timed t md)
     (timed t (music-data->lasting md))]))

;; ---------------------------------------------------------

