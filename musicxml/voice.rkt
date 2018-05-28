#lang racket/base

(provide voice
         voice-string)

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "util/tag.rkt")
(module+ test
  (require rackunit))

;; ---------------------------------------------------------

(define-tag voice '() (list/c string?))

;; ---------------------------------------------------------

;; VoiceElem -> VoiceStr
(define (voice-string ve)
  (match ve
    [(voice _ (list (? string? s)))
     s]))

(module+ test
  (check-equal? (voice-string (voice '() '("1"))) "1")
  (check-equal? (voice-string (voice '() '("Alto"))) "Alto")
  (check-equal? (voice-string (voice '() '("Viola 2"))) "Viola 2")
  )

;; ---------------------------------------------------------

