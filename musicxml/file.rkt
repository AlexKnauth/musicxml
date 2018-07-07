#lang racket/base

(provide read-musicxml-file
         write-musicxml-file
         open-musicxml-file/MuseScore-2)

(require racket/path
         racket/string
         racket/system
         xml
         txexpr
         txexpr/stx
         "util/txexpr-traverse.rkt")
(module+ test
  (require rackunit))

;; ---------------------------------------------------------

;; An MXexpr is a StxXexpr that represents MusicXml

;; ---------------------------------------------------------

;; read-musicxml-file : PathString -> MXexpr
(define (read-musicxml-file file-path)
  (call-with-input-file* file-path read-musicxml))

;; read-musicxml : InputPort -> MXexpr
(define (read-musicxml in)
  (musicxml-trim-whitespace (syntax:read-xml in)))

;; ---------------------------------------------------------

;; write-musicxml-file : MXexpr PathString -> Void
(define (write-musicxml-file mx file-path #:exists [exists 'error])
  (call-with-output-file* file-path
    (λ (out) (write-musicxml mx out))
    #:exists exists))

;; write-musicxml : MXexpr OutputPort -> Void
(define (write-musicxml mx out)
  (write-string XML-declaration out)
  (newline out)
  (write-string MusicXML-DOCTYPE-declaration out)
  (newline out)
  (write-xexpr (stx->datum mx) out))

;; pretty-write-xexpr : Xexpr OutputPort -> Void
;; For some reason, this causes breaks and crashes
(define (pretty-write-xexpr xexpr out)
  (display-xml/content (xexpr->xml xexpr) out))

;; ---------------------------------------------------------

;; open-musicxml-file/MuseScore-2 : PathString -> Void
(define (open-musicxml-file/MuseScore-2 file-path)
  (system (format "open -a ~v ~v"
                  "MuseScore 2"
                  (path->string (simple-form-path file-path)))))

;; ---------------------------------------------------------

(define XML-declaration
  #<<```
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
```
  )

(define MusicXML-DOCTYPE-declaration
  #<<```
<!DOCTYPE score-partwise PUBLIC
    "-//Recordare//DTD MusicXML 3.0 Partwise//EN"
    "http://www.musicxml.org/dtds/partwise.dtd">
```
  )

;; ---------------------------------------------------------

;; musicxml-trim-whitespace : StxXexpr -> StxXexpr
(define (musicxml-trim-whitespace mx)
  (define e (stx-e mx))
  (restore
   mx
   (cond
     [(string? e)
      (string-trim e)]
     [(stx-txexpr? e)
      (define-values [tag attrs elements]
        (stx-txexpr->values e))
      (list* tag
             attrs
             (musicxml-elements-trim-whitespace elements))]
     [else
      (txexpr-traverse/recur e musicxml-trim-whitespace)])))

;; musicxml-elements-trim-whitespace :
;; [Listof StxXexpr] -> [Listof StxXexpr]
(define (musicxml-elements-trim-whitespace elements)
  (filter
   (compose not empty-string? stx-e)
   (map musicxml-trim-whitespace elements)))

;; ---------------------------------------------------------

;; stx-e : Stx -> E
(define (stx-e stx)
  (if (syntax? stx) (syntax-e stx) stx))

;; stx->datum : Stx -> Any
(define (stx->datum stx)
  (syntax->datum (datum->syntax #f stx)))

;; restore : Stx E -> Stx
(define (restore stx e)
  (if (syntax? stx) (datum->syntax stx e stx stx) e))

;; empty-string? : Any -> Bool
(define (empty-string? v)
  (and (string? v) (string=? v "")))

;; ---------------------------------------------------------

(module+ test
  (define-check (check-wr-musicxml mx)
    (let-values ([[in out] (make-pipe)])
      (write-musicxml mx out)
      (close-output-port out)
      (check-txexprs-equal? (stx->datum (read-musicxml in))
                            (stx->datum mx))
      (close-input-port in)))

  (check-wr-musicxml '(aoe ([mka "etuu"]) "netend"))
  (check-wr-musicxml '(aoe ([mka "etuu"]) (nested "netend")))
  )

;; ---------------------------------------------------------

