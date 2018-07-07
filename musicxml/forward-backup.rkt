#lang racket/base

(provide forward ~forward forwardₑ
         backup ~backup backupₑ
         forward-duration-divisions
         backup-duration-divisions
         forward-has-voice?
         forward-voice-string)

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "duration.rkt"
         "voice.rkt"
         "editorial.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")

;; Forward elements can have voices, but Backup elements
;; cannot.

;; ---------------------------------------------------------

(define-tag forward '()
  (or/c (list/c (tag/c duration))
        (list/c (tag/c duration) (tag/c voice))))

(define-tag backup '() (list/c (tag/c duration)))

;; ---------------------------------------------------------

(define-syntax-class forwardₑ
  #:attributes [forward-duration-divisions]
  [pattern {~forward ()
                     (d:durationₑ
                      :%editorial-voice
                      {~optional :staffₑ})}
    #:attr forward-duration-divisions (@ d.duration-divisions)])

(define-syntax-class backupₑ
  #:attributes [backup-duration-divisions]
  [pattern {~backup ()
                    (d:durationₑ
                     :%editorial)}
    #:attr backup-duration-divisions (@ d.duration-divisions)])

;; ---------------------------------------------------------

;; private
;; Any -> Boolean
(define (duration? v)
  (match v [(duration _ _) #true] [_ #false]))

;; Forward -> PositiveDivisions
(define (forward-duration-divisions fw)
  (syntax-parse fw
    [:forwardₑ
     (@ forward-duration-divisions)]))

;; Backup -> PositiveDivisions
(define (backup-duration-divisions bk)
  (syntax-parse bk
    [:backupₑ
     (@ backup-duration-divisions)]))

;; ---------------------------------------------------------

;; private
;; Any -> Boolean
(define (voice? v)
  (match v [(voice _ _) #true] [_ #false]))

;; forward-has-voice? : Forward -> Boolean
(define (forward-has-voice? fw)
  (match fw
    [(forward _ elements)
     (ormap voice? elements)]))

;; forward-voice-string : Forward -> [Maybe VoiceStr]
(define (forward-voice-string fw)
  (match fw
    [(forward _ (list _ ... (? voice? v) _ ...))
     (voice-string v)]
    [_
     #false]))

;; ---------------------------------------------------------

