#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         "metadata.rkt"
         "str-number.rkt"
         "music-data.rkt"
         "measure.rkt"
         "util/tag.rkt"
         "util/parse.rkt"
         "util/stxparse.rkt")
(module+ test
  (require rackunit))

;; A MXexpr is a TXexpr in MusicXML format

(define str-version/c string?)

;; ---------------------------------------------------------

;; %partwise

(define-tag score-partwise
  (attrs-may/c 'version str-version/c)
  (parse/c
   (:%score-header
    :partₑ
    ...+)))

(define-syntax-class score-partwiseₑ
  #:attributes []
  [pattern {~score-partwise
            _
            (:%score-header :partₑ ...+)}])

;; ---------------------------------------------------------

;; %score-header

;; See also metadata.rkt

(define-tag defaults '()
  (listof (or/c (tag/c scaling) (tag/c page-layout)
                (tag/c system-layout) (tag/c staff-layout)
                (tag/c appearance) (tag/c music-font)
                (tag/c word-font) (tag/c lyric-font)
                (tag/c lyric-language))))

(define-tag credit '()
  (listof (or/c (tag/c credit-type) (tag/c link)
                (tag/c bookmark) (tag/c credit-image)
                (tag/c credit-words))))

(define-tag part-list '()
  (listof (tag/c score-part)))

(define-tag score-part
  (attrs-must/c 'id string?)
  (list/c (or/c (tag/c identification)
                (tag/c part-name) (tag/c part-name-display)
                (tag/c part-abbreviation) (tag/c part-abbreviation-display)
                (tag/c group) (tag/c score-instrument)
                (tag/c midi-device) (tag/c midi-instrument))))

(define-tag part-name any/c
  (list/c string?))

(define-tag part-abbreviation any/c
  (list/c string?))

(define-tag score-instrument any/c any/c)
(define-tag midi-device any/c any/c)
(define-tag midi-instrument any/c any/c)

(define-splicing-syntax-class %score-header
  [pattern {~seq {~optional :workₑ}
                 {~optional :movement-numberₑ}
                 {~optional :movement-titleₑ}
                 {~optional :identificationₑ}
                 {~optional :defaultsₑ}
                 :creditₑ
                 ...
                 :part-listₑ}])

(define-syntax-class defaultsₑ
  #:attributes []
  ;; TODO
  [pattern {~defaults _ _}])

(define-syntax-class creditₑ
  #:attributes []
  ;; TODO
  [pattern {~credit _ _}])

(define-syntax-class part-listₑ
  #:attributes []
  [pattern {~part-list () (:score-partₑ ...+)}])

(define-syntax-class score-partₑ
  #:attributes [part-id part-name-string]
  #:datum-literals [id]
  [pattern {~score-part ([id part-id*:str])
                        (:part-nameₑ
                         {~optional :part-abbreviationₑ}
                         :score-instrumentₑ
                         ...
                         {~alt :midi-deviceₑ
                               :midi-instrumentₑ}
                         ...)}
    #:attr part-id (@ part-id*.string)])

(define-syntax-class part-nameₑ
  #:attributes [part-name-string]
  [pattern {~part-name () (name:str)}
    #:attr part-name-string (@ name.string)])

(define-syntax-class part-abbreviationₑ
  #:attributes [part-abbreviation-string]
  [pattern {~part-abbreviation () (abbr:str)}
    #:attr part-abbreviation-string (@ abbr.string)])

(define-syntax-class score-instrumentₑ
  #:attributes []
  ;; TODO
  [pattern {~score-instrument _ _}])

(define-syntax-class midi-deviceₑ
  #:attributes []
  ;; TODO
  [pattern {~midi-device _ _}])

(define-syntax-class midi-instrumentₑ
  #:attributes []
  ;; TODO
  [pattern {~midi-instrument _ _}])

;; ---------------------------------------------------------

(define-tag part
  (attrs-must/c 'id string?)
  (listof (tag/c measure)))

(define-syntax-class partₑ
  #:attributes [part-id]
  #:datum-literals [id]
  [pattern {~part ([id part-id*:str]) (:measureₑ ...+)}
    #:attr part-id (@ part-id*.string)])

;; ---------------------------------------------------------

(module+ test
  (check-true
   (syntax-parse
       #'(part-list
          (score-part ((id "P1"))
            (part-name "Viola")
            (part-abbreviation "Vla.")
            (score-instrument ((id "P1-I1")) (instrument-name "Viola"))
            (midi-device ((id "P1-I1") (port "1")))
            (midi-instrument ((id "P1-I1"))
              (midi-channel "1")
              (midi-program "42")
              (volume "78.7402")
              (pan "0")))
          (score-part ((id "P2"))
            (part-name "Viola")
            (part-abbreviation "Vla.")
            (score-instrument ((id "P2-I1")) (instrument-name "Viola"))
            (midi-device ((id "P2-I1") (port "1")))
            (midi-instrument ((id "P2-I1"))
              (midi-channel "4")
              (midi-program "42")
              (volume "78.7402")
              (pan "0")))
          (score-part ((id "P3"))
            (part-name "Viola")
            (part-abbreviation "Vla.")
            (score-instrument ((id "P3-I1")) (instrument-name "Viola"))
            (midi-device ((id "P3-I1") (port "1")))
            (midi-instrument ((id "P3-I1"))
              (midi-channel "7")
              (midi-program "42")
              (volume "78.7402")
              (pan "0")))
          (score-part ((id "P4"))
            (part-name "Classical Guitar")
            (part-abbreviation "Guit.")
            (score-instrument ((id "P4-I1"))
                              (instrument-name "Classical Guitar"))
            (midi-device ((id "P4-I1") (port "1")))
            (midi-instrument ((id "P4-I1"))
              (midi-channel "11")
              (midi-program "25")
              (volume "78.7402")
              (pan "0"))))
     [:part-listₑ #true]
     [_ #false])))
