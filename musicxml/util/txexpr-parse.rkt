#lang racket/base

(provide txexpr-parse)

;; txexpr-parse
;; meant to be sort of like syntax-parse, but for txexprs

;; (txexpr-parse to-match-expr
;;   [pat body-expr]
;;   ...)

;;    pat = id
;;        | string
;;        | number
;;        | (quote datum)
;;        | (txexpr pat pat pat)
;;        | (list hpat ...)
;;
;;   hpat = pat

(require (submod txexpr safe)
         syntax/parse/define
         syntax/parse
         racket/match
         (for-syntax racket/base
                     racket/syntax))
(module+ test
  (require rackunit))

(begin-for-syntax
  ;; for now, don't support ellipses

  (define-syntax-class clause
    #:attributes [stxparse-clause]
    [pattern [pat:pat body:expr]
      #:with stxparse-clause
      #'[pat.stxparse-pat
         body]])

  (define-syntax-class pat
    #:attributes [stxparse-pat [id 1]]

    #:literals [quote txexpr list]
    [pattern x:id
      #:with tmp (generate-temporary #'x)
      #:with stxparse-pat #'(~and tmp
                                  (~do (define x (syntax->datum #'tmp))))
      #:with [id ...] #'[x]]
    [pattern s:str
      #:with stxparse-pat #'s
      #:with [id ...] '()]
    [pattern n:number
      #:with stxparse-pat #'n
      #:with [id ...] '()]
    [pattern (q:quote dat)
      #:with stxparse-pat #'(~datum dat)
      #:with [id ...] '()]

    [pattern (tx:txexpr a:pat b:pat c:pat)
      #:with tmp (generate-temporary)
      #:with stxparse-pat #'(~and tmp
                                  (~parse (a.stxparse-pat
                                           b.stxparse-pat
                                           c.stxparse-pat)
                                          (txexpr->list
                                           (syntax->datum #'tmp))))
      #:with [id ...] #'[a.id ... b.id ... c.id ...]]

    [pattern (ls:list hp:hpat ...)
      #:with stxparse-pat #'(hp.stxparse-pat ...)
      #:with [id ...] #'[hp.id ... ...]]
    )

  ;; eventually, this will support ellipses
  (define-syntax-class hpat
    #:attributes [stxparse-pat [id 1]]
    [pattern :pat])
  )

(define-syntax-parser txexpr-parse
  [(_ to-match:expr clause:clause ...)
   (syntax/loc this-syntax
     (syntax-parse to-match
       clause.stxparse-clause
       ...))])

;; ---------------------------------------------------------

(module+ test
  (check-equal? (txexpr-parse 1 ['1 'a] ['2 'b])
                'a)
  (check-equal? (txexpr-parse 2 ['1 'a] ['2 'b])
                'b)

  (check-equal? (txexpr-parse "natu" [x x]) "natu")
  (check-equal? (txexpr-parse (list 1 2 3 4)
                  [(list 1 2 x 4) x])
                3)

  (check-equal? (txexpr-parse (txexpr 'a '()
                                (list (txexpr 'b '() (list "c"))))
                  [(txexpr 'a '() (list (txexpr 'b '() (list c))))
                   c])
                "c")

  )

