#lang racket/base

(provide (all-defined-out))

(require racket/contract/base
         (submod txexpr safe)
         "str-number.rkt"
         "util/stxparse.rkt"
         "util/tag.rkt")
(module+ test
  (require rackunit))

;; The mode type is used to specify major/minor and other
;; mode distinctions. Valid mode values include major,
;; minor, dorian, phrygian, lydian, mixolydian, aeolian,
;; ionian, locrian, and none.
(define major "major")
(define minor "minor")
(define dorian "dorian")
(define phrygian "phrygian")
(define lydian "lydian")
(define mixolydian "mixolydian")
(define aeolian "aeolian")
(define ionian "ionian")
(define locrian "locrian")

(define str-mode/c
  (or/c major minor dorian phrygian lydian mixolydian
        aeolian ionian locrian "none"))

;; ---------------------------------------------------------

(define-tag key any/c
  (listof (or/c (tag/c cancel)
                (tag/c fifths)
                (tag/c mode))))

(define-tag cancel '() (list/c str-integer?))
(define-tag fifths '() (list/c str-integer?))
(define-tag mode '() (list/c str-mode/c))

(define-syntax-class keyₑ
  #:attributes [cancel-fifths fifths mode]
  [pattern {~key _
                 ({~optional :cancelₑ}
                  :fifthsₑ
                  {~optional :modeₑ})}])

(define-syntax-class cancelₑ
  #:attributes [cancel-fifths]
  [pattern {~cancel () (fifths:str-int)}
    #:attr cancel-fifths (@ fifths.number)])

(define-syntax-class fifthsₑ
  #:attributes [fifths]
  [pattern {~fifths () (fifths*:str-int)}
    #:attr fifths (@ fifths*.number)])

(define-syntax-class modeₑ
  #:attributes [mode]
  [pattern {~mode () (mode*:str)}
    #:attr mode (@ mode*.string)
    #:when (str-mode/c (@ mode))])

;; ---------------------------------------------------------

(module+ test
  (check-true
   (syntax-parse
       #'(key (fifths "3"))
     [:keyₑ #true]
     [_ #false]))

  (syntax-parse
      #'(key (fifths "3"))
    [k:keyₑ
     (check-equal? (@ k.cancel-fifths) #f)
     (check-equal? (@ k.fifths) 3)
     (check-equal? (@ k.mode) #f)]))
