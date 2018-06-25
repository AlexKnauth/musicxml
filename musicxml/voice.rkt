#lang racket/base

(provide voice
         ~voice
         voiceₑ
         voice-string)

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "util/stxparse.rkt"
         "util/tag.rkt")
(module+ test
  (require rackunit))

;; ---------------------------------------------------------

(define-tag voice '() (list/c string?))

;; "voice element"
(define-syntax-class voiceₑ
  #:attributes [voice-string]
  [pattern {~voice _ (s:str)}
    #:attr voice-string (@ s.string)])

;; ---------------------------------------------------------

;; VoiceElem -> VoiceStr
(define (voice-string ve)
  (syntax-parse ve
    [ve:voiceₑ (@ ve.voice-string)]))

(module+ test
  (check-equal? (voice-string (voice '() '("1"))) "1")
  (check-equal? (voice-string (voice '() '("Alto"))) "Alto")
  (check-equal? (voice-string (voice '() '("Viola 2"))) "Viola 2")
  )

;; ---------------------------------------------------------

