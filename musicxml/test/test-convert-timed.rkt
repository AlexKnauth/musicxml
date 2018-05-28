#lang racket/base

(require racket/function
         (except-in racket/list rest)
         racket/match
         "../timed/convert-timed.rkt"
         "../timed/put-in-order.rkt"
         "../timed/timed.rkt"
         "../timed/part-timed.rkt"
         "../score-partwise.rkt"
         "../measure.rkt"
         "../attributes.rkt"
         "../direction.rkt"
         "../clef.rkt"
         "../key.rkt"
         "../time-signature.rkt"
         "../forward-backup.rkt"
         "../note.rkt"
         "../duration.rkt"
         "../voice.rkt"
         )

(module+ test
  (require rackunit
           (submod txexpr safe))

  (define-check (check-convert a->b b->a a-example)
    (check-equal? (b->a (a->b a-example)) a-example))

  (define-check (check-convert* a->b
                                mangle-b
                                b->a
                                a-example)
    (check-convert a->b b->a a-example "non-mangled version")
    (check-equal? (b->a (mangle-b (a->b a-example))) a-example))

  (define (group-by-chords elements)
    (let loop ([groups '()] [g '()] [elems elements])
      (match elems
        ['() (reverse (cons g groups))]
        [(cons e rst)
         (match e
           [(timed _ (and n (note _ _)))
            (if (note-has-chord? n)
                (loop groups (cons e g) rst)
                (loop (cons g groups) (list e) rst))]
           [_ (loop (cons g groups) (list e) rst)])])))

  (define (mangle-part/timed p/t)
    ;; TODO:
    ;;   randomize placement, remove forward and backup
    ;;   elements, remove rests, stuff like that, but
    ;;   without messing up chords
    (match p/t
      [(part/timed id elements)
       (part/timed
        id
        (append*
         (shuffle
          (group-by-chords
           (filter (compose
                    (negate
                     (λ (e)
                       (match e
                         [(forward _ _) #t]
                         [(backup _ _) #t]
                         ;[(note _ (list (rest _ _) _ ...)) #t]
                         [_ #f])))
                    timed-value)
                   elements)))))]))
  )

;; -------------------------------------

(module+ test
  (check-convert* part->part/timed
                  mangle-part/timed
                  part/timed->part
                  (part `([id "123"]) '()))

  (check-convert* part->part/timed
                  mangle-part/timed
                  part/timed->part
                  (part '([id "456"])
                   (list
                    (measure '([number "1"])
                      (list
                       (attributes '()
                         (list (divisions '() '("1")))))))))

  (check-convert* part->part/timed
                  mangle-part/timed
                  part/timed->part
                  (part '([id "789"])
                   (list
                    (measure '([number "1"])
                      (list
                       (attributes '()
                         (list
                          (divisions '() '("1"))
                          (clef '()
                            (list (sign '() '("G"))
                                  (line '() '("2"))))
                          (key '() (list (fifths '() '("0"))))
                          (time '()
                            (list (beats '() '("4"))
                                  (beat-type '() '("4"))))))
                       (note '()
                         (list
                          (pitch '() (list (step '() '("C")) (octave '() '("4"))))
                          (duration '() '("1"))
                          (voice '() '("1"))
                          (type '() '("quarter"))
                          (notations '() '())))
                       (note '()
                         (list
                          (rest '() '())
                          (duration '() '("1"))
                          (voice '() '("1"))))
                       (note '()
                         (list
                          (rest '() '())
                          (duration '() '("2"))
                          (voice '() '("1"))))
                       )))))

  (check-convert*
    part->part/timed
    mangle-part/timed
    part/timed->part
    '(part
      ((id "P1"))
      (measure
       ((number "1"))
       (attributes
        (divisions "2")
        (clef (sign "G") (line "2"))
        (key (fifths "0"))
        (time (beats "4") (beat-type "4")))
       (note (rest) (duration "2"))
       (note (pitch (step "C") (octave "4")) (duration "2") (type "quarter"))
       (note (pitch (step "D") (octave "4")) (duration "2") (type "quarter"))
       (note (pitch (step "E") (octave "4")) (duration "2") (type "quarter")))
      (measure
       ((number "2"))
       (note (pitch (step "F") (octave "4")) (duration "2") (type "quarter"))
       (note (pitch (step "E") (octave "4")) (duration "1") (type "eighth"))
       (note (pitch (step "D") (octave "4")) (duration "1") (type "eighth"))
       (note (pitch (step "E") (octave "4")) (duration "4") (type "half")))))

  ;; -------------------------

  (check-convert
    part->part/timed
    part/timed->part
    (part '([id "P2"])
      (list
       (measure '([number "1"])
        (list
         (attributes '()
          (list
           (divisions '() '("2"))
           (clef '()
                 (list (sign '() '("G"))
                       (line '() '("2"))))
           (key '() (list (fifths '() '("0"))))
           (time '()
                 (list (beats '() '("4"))
                       (beat-type '() '("4"))))))
         (direction '([placement "above"])
           (list
            (direction-type '()
              (list
               (metronome '()
                          (list (beat-unit '() '("quarter"))
                                (per-minute  '() '("100"))))))
            (sound '([tempo "100"]) '())))
         (note '()
          (list
           (rest '() '())
           (duration '() '("2"))
           (voice '() '("1"))))
         (note '()
          (list
           (pitch '() (list (step '() '("C")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("D")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("E")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (chord '() '())
           (pitch '() (list (step '() '("G")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))))
       (measure '([number "2"])
        (list
         (note '()
          (list
           (pitch '() (list (step '() '("F")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (chord '() '())
           (pitch '() (list (step '() '("A")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("B")) (octave '() '("4"))))
           (duration '() '("2"))
           (voice '() '("1"))
           (type '() '("quarter"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("E")) (octave '() '("4"))))
           (duration '() '("4"))
           (voice '() '("1"))
           (type '() '("half"))
           (notations '() '())))
         (note '()
          (list
           (chord '() '())
           (pitch '() (list (step '() '("C")) (octave '() '("5"))))
           (duration '() '("4"))
           (voice '() '("1"))
           (type '() '("half"))
           (notations '() '())))
         (backup '()
          (list (duration '() '("6"))))
         (note '()
          (list
           (pitch '() (list (step '() '("E")) (octave '() '("4"))))
           (duration '() '("1"))
           (voice '() '("2"))
           (type '() '("eighth"))
           (notations '() '())))
         (note '()
          (list
           (pitch '() (list (step '() '("D")) (octave '() '("4"))))
           (duration '() '("1"))
           (voice '() '("2"))
           (type '() '("eighth"))
           (notations '() '())))
         (note '()
          (list
           (rest '() '())
           (duration '() '("4"))
           (voice '() '("2")))))))))
  )

