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
         txexpr/stx
         txexpr/stx/parse
         syntax/parse/define
         syntax/parse
         racket/match
         (for-syntax racket/base
                     racket/match
                     racket/syntax
                     racket/string))
(module+ test
  (require rackunit))

(begin-for-syntax
  ;; for now, don't support ellipses

  (define-syntax-class clause
    #:attributes [stxparse-clause]
    [pattern [{~var pat (pat 0)} body:expr]
      #:with stxparse-clause
      #'[pat.stxparse-pat
         body]])

  (define-syntax-class (pat d)
    #:attributes [stxparse-pat]

    #:literals [syntax quote txexpr list]

    [pattern (syntax stxparse-pat)]
    [pattern (quote dat) #:with stxparse-pat #'(~datum dat)]

    [pattern {~and x:id {~not {~literal ...}}}
      #:when (not (string-contains? (symbol->string (syntax-e #'x)) ":"))
      #:with tmp (generate-temporary #'x)
      #:with stxparse-pat #'(~and tmp
                                  (~do (define x #'tmp)))]
    [pattern {~and x:id {~not {~literal ...}}}
      #:do [(define str (symbol->string (syntax-e #'x)))]
      #:when (string-contains? str ":")
      #:do [(match-define (cons fst rst) (string-split str ":" #:trim? #false))]
      #:with fst-x (format-id #'x "~a" fst)
      #:with rst-class (format-id #'x "~a" (string-join rst ":"))
      #:with tmp (generate-temporary #'x)
      #:with stxparse-pat #'(~and (~var tmp rst-class)
                                  (~do (define fst-x #'tmp)))]

    [pattern s:str
      #:with stxparse-pat #'s]
    [pattern n:number
      #:with stxparse-pat #'n]

    [pattern (tx:txexpr {~var a (pat d)}
                        {~var b (pat d)}
                        {~var c (pat d)})
      #:with stxparse-pat
      #'(~txexpr a.stxparse-pat b.stxparse-pat c.stxparse-pat)]

    [pattern (ls:list . {~var lp (lpat d)})
      #:with stxparse-pat #'lp.stxparse-pat]
    )

  ;; eventually, this will support ellipses
  (define-syntax-class (lpat d)
    #:attributes [stxparse-pat]
    [pattern {~and () stxparse-pat}]
    [pattern {~and ({~var hp (hpat d)} . {~var lp (lpat d)})}
      #:with stxparse-pat #'(hp.stxparse-pat . lp.stxparse-pat)]
    [pattern {~and ({~var ehp (ehpat (add1 d))}
                    {~and ooo {~literal ...}}
                    .
                    {~var lp (lpat d)})}
      #:with stxparse-pat #'(ehp.stxparse-pat ooo . lp.stxparse-pat)]
    )
  
  (define-syntax-class (hpat d)
    #:attributes [stxparse-pat]
    [pattern {~var || (pat d)}])

  (define-syntax-class (ehpat d)
    #:attributes [stxparse-pat]
    [pattern {~var || (hpat d)}])
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

  (check-equal? (txexpr-parse "natu" [x (syntax->datum x)]) "natu")
  (check-equal? (txexpr-parse (list 1 2 3 4)
                  [(list 1 2 x 4) (syntax->datum x)])
                3)

  (check-equal? (txexpr-parse (txexpr 'a '()
                                (list (txexpr 'b '() (list "c"))))
                  [(txexpr 'a '() (list (txexpr 'b '() (list c))))
                   (syntax->datum c)])
                "c")

  )

