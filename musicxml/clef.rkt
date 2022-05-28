#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         "str-number.rkt"
         "util/tag.rkt"
         "util/stxparse.rkt")
(module+ example
  (provide (all-defined-out)))
(module+ test
  (require rackunit (submod ".." example)))

(define str-staff-number/c str-positive-integer?)

;; The clef-sign element represents the different clef
;; symbols. The jianpu sign indicates that the music that
;; follows should be in jianpu numbered notation, just as
;; the TAB sign indicates that the music that follows
;; should be in tablature notation. Unlike TAB, a jianpu
;; sign does not correspond to a visual clef notation.
(define G "G")  ; treble
(define F "F")  ; bass
(define C "C")  ; alto
(define percussion "percussion")
(define TAB "TAB")
(define jianpu "jianpu")

(define str-clef-sign/c
  (or/c G  ; treble
        F  ; bass
        C  ; alto
        percussion
        TAB
        jianpu
        "none"))

;; The staff-line type indicates the line on a given staff.
;; Staff lines are numbered from bottom to top, with 1
;; being the bottom line on a staff. Staff line values can
;; be used to specify positions outside the staff, such as
;; a C clef positioned in the middle of a grand staff.
(define str-staff-line/c str-integer?)

;; ---------------------------------------------------------

(define-tag clef any/c
  (or/c (list/c (tag/c sign))
        (list/c (tag/c sign) (tag/c line))
        (list/c (tag/c sign) (tag/c clef-octave-change))
        (list/c (tag/c sign) (tag/c line) (tag/c clef-octave-change))))

(define-tag sign '() (list/c str-clef-sign/c))
(define-tag line '() (list/c str-staff-line/c))
(define-tag clef-octave-change '() (list/c str-integer?))

;; ---------------------------------------------------------

(define-syntax-class clefₑ
  #:attributes [sign line clef-octave-change]
  [pattern {~clef _ (:signₑ {~optional :lineₑ} {~optional :clef-octave-changeₑ})}])

(define-syntax-class signₑ
  #:attributes [sign]
  [pattern {~sign _ (s:str)}
    #:attr sign (@ s.string)])

(define-syntax-class lineₑ
  #:attributes [line]
  [pattern {~line _ (l:str-int)}
    #:attr line (@ l.number)])

(define-syntax-class clef-octave-changeₑ
  #:attributes [clef-octave-change]
  [pattern {~clef-octave-change _ (o:str-int)}
    #:attr clef-octave-change (@ o.number)])

;; ---------------------------------------------------------

(module+ example
  (define treble-clef
    (clef* '() (sign* '() G) (line* '() "2")))

  (define bass-clef
    (clef* '() (sign* '() F) (line* '() "4")))

  (define vocal-tenor-clef
    (clef* '()
           (sign* '() G)
           (line* '() "2")
           (clef-octave-change* '() "-1")))

  (define alto-clef
    (clef* '() (sign* '() C) (line* '() "3"))))

(module+ test
  (syntax-parse treble-clef
    [clef:clefₑ
     (check-equal? (@ clef.sign) G)
     (check-equal? (@ clef.line) 2)
     (check-equal? (@ clef.clef-octave-change) #f)])

  (syntax-parse bass-clef
    [clef:clefₑ
     (check-equal? (@ clef.sign) F)
     (check-equal? (@ clef.line) 4)
     (check-equal? (@ clef.clef-octave-change) #f)])

  (syntax-parse vocal-tenor-clef
    [clef:clefₑ
     (check-equal? (@ clef.sign) G)
     (check-equal? (@ clef.line) 2)
     (check-equal? (@ clef.clef-octave-change) -1)])

  (syntax-parse alto-clef
    [clef:clefₑ
     (check-equal? (@ clef.sign) C)
     (check-equal? (@ clef.line) 3)
     (check-equal? (@ clef.clef-octave-change) #f)]))
