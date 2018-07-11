#lang agile

(require "time.rkt")
(module+ test
  (require rackunit))

;; The data representation for time periods does not include ties.

;; ------------------------------------------------------------------------

(provide time-period time-period?
         time-period-start
         time-period-duration
         time-period-end
         time-period-contains-time?
         time-period-overlap?
         time-periods-overlap?)

;; A TimePeriod is a (time-period Time NonNegDivisions)
(struct time-period [start duration] #:transparent)

;; time-period=? : TimePeriod TimePeriod -> Boolean
(define (time-period=? a b)
  (match* [a b]
    [[(time-period ap ad) (time-period bp bd)]
     (and (time=? ap bp)
          (= ad bd))]))

;; time-period-end : TimePeriod -> Position
(define (time-period-end tp)
  (time+ (time-period-start tp) (time-period-duration tp)))

;; time-period-contains-pos? : TimePeriod Position -> Bool
;; Currently does not account for measure length
(define (time-period-contains-time? tp t)
  (and (time<=? (time-period-start tp) t)
       (time<? t (time-period-end tp))))

;; time-period-overlap? : TimePeriod TimePeriod -> Bool
;; Currently does not account for measure length
;; (a-start < b-end) and (b-start < a-end)
(define (time-period-overlap? a b)
  (and (time<? (time-period-start a) (time-period-end b))
       (time<? (time-period-start b) (time-period-end a))))

;; time-periods-overlap? : [Listof TimePeriod] -> Bool
;; Currently does not account for measure length
(define (time-periods-overlap? tps)
  (for/or ([(a b) (in-combinations tps 2)])
    (time-period-overlap? a b)))

;; ------------------------------------------------------------------------

;; Sorting by time period

(provide time-period<?)

;; time-period<? : TimePeriod TimePeriod -> Boolean
(define (time-period<? a b)
  (match* [a b]
    [[(time-period ap ad) (time-period bp bd)]
     (or (time<? ap bp)
         (and (time=? ap bp)
              (< ad bd)))]))

;; ------------------------------------------------------------------------

(module+ test
  (check-true (time-period-overlap?
               (time-period (time 0 4) 1)
               (time-period (time 0 2) 3)))
  (check-false (time-period-overlap?
                (time-period (time 5 4) 2)
                (time-period (time 5 2) 2)))
  (check-true (time-period-overlap?
               (time-period (time 2 4) 2)
               (time-period (time 2 2) 8)))
  (check-false (time-period-overlap?
                (time-period (time 6 2) 2)
                (time-period (time 6 4) 8)))
  (check-true (time-period-overlap?
               (time-period (time 3 3) 2)
               (time-period (time 3 4) 8)))
  )

;; ------------------------------------------------------------------------

