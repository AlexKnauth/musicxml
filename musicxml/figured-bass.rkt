#lang racket/base

(provide figured-bass ~figured-bass figured-bassₑ
         figure ~figure figureₑ
         figure-number ~figure-number figure-numberₑ
         ;; ---
         figured-bass-duration-divisions)

(require racket/contract/base
         racket/match
         (submod txexpr safe)
         "str-number.rkt"
         "duration.rkt"
         "note.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")

;; ---------------------------------------------------------

;; Figured bass elements take their position from the first
;; regular note (not a grace note or chord note) that follows
;; in score order. The optional duration element is used to
;; indicate changes of figures under a note.	

;; Figures are ordered from top to bottom. A figure-number is a
;; number. Values for prefix and suffix include the accidental
;; values sharp, flat, natural, double-sharp, flat-flat, and
;; sharp-sharp. Suffixes include both symbols that come after
;; the figure number and those that overstrike the figure number.
;; The suffix value slash is used for slashed numbers indicating
;; chromatic alteration. 

(define-tag figured-bass any/c
  (listof (or/c (tag/c figure)
                (tag/c duration)
                (tag/c footnote)
                (tag/c level))))

(define-tag figure any/c
  (listof (or/c (tag/c prefix)
                (tag/c figure-number)
                (tag/c suffix))))

(define-tag figure-number any/c any/c)
(define-tag prefix any/c any/c)
(define-tag suffix any/c any/c)

;; ---------------------------------------------------------

(define-syntax-class figured-bassₑ
  #:attributes []
  [pattern {~figured-bass
            ()
            (:figureₑ
             ...+
             {~optional :durationₑ})}])

(define-syntax-class figureₑ
  #:attributes []
  [pattern {~figure
            ()
            ({~optional :prefixₑ}
             {~optional :figure-numberₑ}
             {~optional :suffixₑ})}])

(define-syntax-class figure-numberₑ
  #:attributes [figure-number]
  [pattern {~figure-number () (n:str-num)}
    #:attr figure-number (@ n.number)])

(define-syntax-class prefixₑ
  #:attributes []
  [pattern {~prefix _ _}])

(define-syntax-class suffixₑ
  #:attributes []
  [pattern {~suffix _ _}])

;; ---------------------------------------------------------

;; FiguredBass -> PositiveDivisions
(define (figured-bass-duration-divisions fb)
  ;; Any -> Boolean
  (define (duration? v)
    (match v [(duration _ _) #true] [_ #false]))
  (match fb
    [(figured-bass _ (list _ ... (? duration? d) _ ...))
     (duration-divisions d)]))

;; ---------------------------------------------------------

