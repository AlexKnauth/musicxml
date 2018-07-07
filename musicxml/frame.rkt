#lang racket/base

(provide frame ~frame frameₑ
         frame-strings ~frame-strings frame-stringsₑ
         frame-frets ~frame-frets frame-fretsₑ
         frame-note ~frame-note frame-noteₑ
         ;; ---
         string ~string stringₑ
         fret ~fret fretₑ
         barre ~barre barreₑ)

(require racket/contract/base
         "str-number.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")

(define start-stop/c (or/c "start" "stop"))

;; ---------------------------------------------------------

;; The frame element represents a frame or fretboard diagram
;; used together with a chord symbol. 
;; The frame-strings and frame-frets elements give the 
;; overall size of the frame in vertical lines (strings) and 
;; horizontal spaces (frets).
(define-tag frame any/c any/c)
(define-tag frame-strings '() (list/c str-nonnegative-integer?))
(define-tag frame-frets '() (list/c str-nonnegative-integer?))
(define-tag first-fret any/c any/c)

;; The frame-note element represents each note included in
;; the frame.
;; String numbers start at 1.
;; An open string will have a fret value of 0, while a
;; muted string will not be associated with a frame-note
;; element.
(define-tag frame-note any/c any/c)
(define-tag string any/c any/c)
(define-tag fret any/c any/c)
(define-tag barre (attrs-must/c 'type start-stop/c) '())

;; ---------------------------------------------------------

(define-syntax-class frameₑ
  #:attributes []
  [pattern {~frame
            _
            (:frame-stringsₑ
             :frame-fretsₑ
             {~optional :first-fretₑ}
             frame-note:frame-noteₑ
             ...+)}])

(define-syntax-class frame-stringsₑ
  #:attributes [strings-number]
  [pattern {~frame-strings () (n:str-nat)}
    #:attr strings-number (@ n.number)])

(define-syntax-class frame-fretsₑ
  #:attributes [frets-number]
  [pattern {~frame-frets () (n:str-nat)}
    #:attr frets-number (@ n.number)])

(define-syntax-class first-fretₑ
  #:attributes []
  [pattern {~first-fret () (n:str-nat)}])

(define-syntax-class frame-noteₑ
  #:attributes [string-number fret-number]
  [pattern {~frame-note
            ()
            (:stringₑ
             :fretₑ
             {~optional :barreₑ})}])

(define-syntax-class stringₑ
  #:attributes [string-number]
  [pattern {~string () (n:str-pos-int)}
    #:attr string-number (@ n.number)])

(define-syntax-class fretₑ
  #:attributes [fret-number]
  [pattern {~fret () (n:str-nat)}
    #:attr fret-number (@ n.number)])

(define-syntax-class barreₑ
  #:attributes [start? stop?]
  #:datum-literals [type]
  [pattern {~barre ([type "start"]) ()}
    #:attr start? #true
    #:attr stop? #false]
  [pattern {~barre ([type "stop"]) ()}
    #:attr start? #false
    #:attr stop? #true])

;; ---------------------------------------------------------

