#lang racket/base

(provide (struct-out time)
         time=? time<? time<=?
         time-beginning-of-measure?
         time+
         time-divisions∆)

(require racket/math
         racket/match)

;; ---------------------------------------------------------

;; A Time is a (time Integer NonNegDivisions)
(struct time
  [; the measure number (same as the `number` field in the measure element)
   measure             ; : Integer
   ; the time within the measure (in divisions)
   time-within-measure ; : NonNegDivisions
   ]
  #:transparent)

;; Time Time -> Boolean
(define (time=? a b)
  (match* [a b]
    [[(time an ad) (time bn bd)]
     (and (= an bn) (= ad bd))]))

;; Time Time -> Boolean
(define (time<? a b)
  (match* [a b]
    [[(time an ad) (time bn bd)]
     (or (< an bn) (and (= an bn) (< ad bd)))]))

;; Time Time -> Boolean
(define (time<=? a b)
  (match* [a b]
    [[(time an ad) (time bn bd)]
     (or (< an bn) (and (= an bn) (<= ad bd)))]))

;; ---------------------------------------------------------

;; Time -> Bool
(define (time-beginning-of-measure? t)
  (zero? (time-time-within-measure t)))

;; Time Divisions -> Time
(define (time+ t d)
  (define td (time-time-within-measure t))
  (define td* (+ td d))
  (unless (nonnegative-integer? td*)
    (error 'time+
           "cannot rewind before the beginning of the measure"))
  (struct-copy time t
    [time-within-measure td*]))

;; Time Time -> Divisions
;; the two times must be within the same measure
(define (time-divisions∆ a b)
  (match* [a b]
    [[(time am ad) (time bm bd)]
     (unless (= am bm)
       (error 'time-divisions∆
              "times must be within the same measure: ~v and ~v"
              a b))
     (- bd ad)]))

;; ---------------------------------------------------------

