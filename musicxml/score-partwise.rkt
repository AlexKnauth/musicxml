#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         syntax/parse
         "metadata.rkt"
         "str-number.rkt"
         "music-data.rkt"
         "measure.rkt"
         "util/tag.rkt"
         "util/parse.rkt")

;; A MXexpr is a TXexpr in MusicXML format

(define str-version/c string?)

;; ------------------------------------------------------------------------

;; %partwise

(define-tag score-partwise
  (attrs-may/c 'version str-version/c)
  (parse/c
   ({~optional {~tag work}}
    {~optional {~tag movement-number}}
    {~optional {~tag movement-title}}
    {~optional {~tag identification}}
    {~tag part-list}
    {~tag part}
    ...)))

;; ------------------------------------------------------------------------

;; %score-header

;; See also metadata.rkt

(define-tag part-list '()
  (listof (tag/c score-part)))

(define-tag score-part
  (attrs-must/c 'id string?)
  (list/c (tag/c part-name)))

(define-tag part-name any/c
  (list/c string?))

;; ------------------------------------------------------------------------

(define-tag part
  (attrs-must/c 'id string?)
  (listof (tag/c measure)))

