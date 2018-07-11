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
  #:attributes
  [;; for overlapping-time-period purposes
   lasting-duration-divisions]

  [pattern :noteₑ
    #:attr lasting-duration-divisions (@ duration-divisions)]
  [pattern :backupₑ
    #:attr lasting-duration-divisions 0]
  [pattern :forwardₑ
    #:attr lasting-duration-divisions (@ forward-duration-divisions)]
  [pattern :directionₑ
    #:attr lasting-duration-divisions 0]
  [pattern :attributesₑ
    #:attr lasting-duration-divisions 0]
  [pattern :harmonyₑ
    #:attr lasting-duration-divisions 0]
  [pattern :figured-bassₑ
    #:attr lasting-duration-divisions 0]
  [pattern :printₑ
    #:attr lasting-duration-divisions 0]
  [pattern :soundₑ
    #:attr lasting-duration-divisions 0]
  [pattern :barlineₑ
    #:attr lasting-duration-divisions 0]
  ;; TODO: grouping, link, and bookmark
  )

;; ---------------------------------------------------------

;; MusicData -> NonnegativeDivisions
;; for overlapping-time-period purposes
(define (music-data-lasting-duration-divisions md)
  (syntax-parse md
    [md:%music-data
     (@ md.lasting-duration-divisions)]))

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

