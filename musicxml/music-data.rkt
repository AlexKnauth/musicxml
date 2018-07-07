#lang racket/base

(provide (all-defined-out)
         attributes
         backup
         forward
         direction
         barline
         figured-bass
         harmony
         note
         sound)

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "attributes.rkt"
         "direction.rkt"
         "barline.rkt"
         "note.rkt"
         "forward-backup.rkt"
         "figured-bass.rkt"
         (except-in "harmony.rkt" alter)
         "voice.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")

;; ------------------------------------------------------------------------

;; %music-data

(define music-data/c
  (or/c (tag/c attributes)
        (tag/c backup)
        (tag/c forward)
        (tag/c direction)
        (tag/c barline)
        (tag/c figured-bass)
        (tag/c harmony)
        (tag/c note)
        (tag/c print)
        (tag/c sound)))

(define-syntax-class %music-data
  [pattern :noteₑ]
  [pattern :backupₑ]
  [pattern :forwardₑ]
  [pattern :directionₑ]
  [pattern :attributesₑ]
  [pattern :harmonyₑ]
  [pattern :figured-bassₑ]
  [pattern :printₑ]
  [pattern :soundₑ]
  [pattern :barlineₑ]
  ;; TODO: grouping, link, and bookmark
  )

;; ---------------------------------------------------------

;; MusicData -> Boolean
(define (music-data-has-voice? md)
  (match md
    [(note _ _) (note-has-voice? md)]
    [(forward _ _) (forward-has-voice? md)]
    [(direction _ _) (direction-has-voice? md)]
    [_
     (when (ormap (tag/c voice) (get-elements md))
       (error 'music-data-has-voice?
              "unknown tag: ~a\n  in: ~v"
              (get-tag md) md))
     #false]))

;; MusicData -> [Maybe VoiceStr]
(define (music-data-voice-string md)
  (match md
    [(note _ _) (note-voice-string md)]
    [(forward _ _) (forward-voice-string md)]
    [(direction _ _) (direction-voice-string md)]
    [_
     (when (ormap (tag/c voice) (get-elements md))
       (error 'music-data-voice-string
              "unknown tag: ~a\n  in: ~v"
              (get-tag md) md))
     #false]))

;; ---------------------------------------------------------

