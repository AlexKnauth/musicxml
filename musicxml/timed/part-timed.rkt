#lang racket/base

(require racket/contract/base)
(provide (contract-out
          [struct part/timed
            ([id string?]
             [timed-elements (listof timed?)])]))

(require "timed.rkt")

;; ---------------------------

;; A PartTimed is a
;;   (part/timed String
;;               [Listof [Timed MusicData]])
(struct part/timed [id timed-elements])

