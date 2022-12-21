#lang racket

(require racket/file)
(require (for-syntax racket/syntax))
(require "../base.rkt" (for-syntax "../base.rkt"))
(require "../hash.rkt")
(require "../io.rkt")
(require "fonts.rkt")
(require "../seqs.rkt" (for-syntax "../seqs.rkt"))
(require (for-syntax "../controls.rkt"))
(require compatibility/defmacro)
(require racket/runtime-path)

(define-runtime-path rootpath "..")

(provide (all-defined-out))

(define-macro (svg . args)
  (let* ( (xmlns (if (indexof? args 'xmlns) #t #f))
          (xlink (if (indexof? args 'xlink) #t #f))
          (styles (if (indexof? args 'styles) #t #f))
          (scripts (if (indexof? args 'scripts) #t #f))
          (params (car
                    (zor
                      (filter (λ (x) (and (list? x) (equal? (car x) '@))) args)
                      (list '(@)))))
          ;(viewbox (hash-ref params 'viewbox #f))
          (body (clean (λ (x) (or (equal? x 'xmlns) (equal? x 'xlink) (and (list? x) (equal? (car x) '@)) (equal? x 'styles) (equal? x 'scripts))) args)))
    `(svg-f ,xmlns ,xlink ,params ,styles ,scripts ,@body)))

(define (svg-f
          xmlns
          xlink
          params
          styles
          scripts
          . body)
  (let* ([xmlns (if xmlns
                      " xmlns=\"http://www.w3.org/2000/svg\""
                      "")]
        [xlink (if xlink
                      " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
                      "")]
        [viewbox (hash-ref params 'viewbox #f)]
        [viewbox-w (if viewbox
                        (nth viewbox 3)
                        100)]
        [viewbox-h (if viewbox
                        (nth viewbox 4)
                        100)]
        [styles (if styles
                      (str  "\n<style type=\"text/css\">\n"
                            "/* <![CDATA[ */\n"
                            (read-file (str rootpath "\\templates\\styles.css"))
                            ;"\n\n/* barchart: */\n\n"
                            ;(read-file (str rootpath "\\templates\\barchart.css"))
                            "/* ]]> */\n"
                            "</style>\n")
                      "")]
        [scripts (if scripts
                      (str  "\n<script type=\"text/ecmascript\">\n"
                            "/* <![CDATA[ */\n"
                            "\n\n/* base.js: */\n\n"
                            (read-file (str rootpath "\\templates\\gui\\base.js"))
                            "/* ]]> */\n"
                            "</script>\n")
                      "")]
        [body (if (empty? body) "" (apply string-append body))])

    (format "<svg~a~a~a>~a~a~a</svg>"
            xmlns
            xlink
            (if viewbox
              (format " viewBox=\"0 0 ~a ~a\"" viewbox-w viewbox-h)
              "")
            styles
            scripts
            body)))

; e.g.: (rect x 10 y 10 width 100 height 100) as well as (rect 'x 10 'y 10 'width 100 'height 100)
(define-macro (make-single-tag tagname)
  `(define-macro (,tagname . body)
    (define (odd-f f lst)
      (cond
        ((null? lst) null)
        ((null? (cadr lst)) (cdr lst))
        (else (cons (f (car lst)) (cons (cadr lst) (odd-f f (cddr lst)))))))
    (let ([nbody (odd-f (λ(x) (if (symbol? x) (symbol->string x) x)) body)]
          [t (symbol->string (quote ,tagname))])
      `(str "<" ,t (print-hash " ~a=\"~a\"" (hash ,@nbody)) " />"))))

(define-syntax (make-tag stx)
  (let ((tagname (symbol->string (list-ref (syntax->datum stx) 1))))
    (datum->syntax
      stx
      `(begin
          (define (,(string->symbol (string-append tagname "1"))
                    (attrs (hash)) . body)
                      (string-append
                        ,(string-append "<" tagname)
                        (if ((hash-length attrs) . > . 0) (print-hash " ~a=\"~a\"" attrs) "")
                        ">"
                        (apply str (if (empty? body) empty body))
                        ,(string-append "</" tagname ">")))
          (define (,(string->symbol (string-append tagname "2"))
                    . body)
                      (string-append
                        ,(string-append "<" tagname ">")
                        (apply str (if (empty? body) empty body))
                        ,(string-append "</" tagname ">")))
          (define-syntax (,(string->symbol tagname) stx)
                      (let (( tl (syntax->datum stx)))
                        (if ((length tl) . < . 2)
                          (datum->syntax stx (string-append "<" ,tagname "></" ,tagname ">"))
                          (let* ( (arg1 (if (list? (cadr tl)) (caadr tl) null))
                                  (args (cdr tl))
                                  (,(string->symbol (string-append tagname "1")) (datum->syntax stx (cons ',(string->symbol (string-append tagname "1")) args)))
                                  (,(string->symbol (string-append tagname "2")) (datum->syntax stx (cons ',(string->symbol (string-append tagname "2")) args))))
                            ;(println (cons ',(string->symbol (string-append tagname "1")) args))
                            ;(printf "~n~n")
                            (if (equal? arg1 '@)
                              ,(string->symbol (string-append tagname "1"))
                              ,(string->symbol (string-append tagname "2")))))))))))

(make-single-tag rect)
(make-single-tag circle)
(make-single-tag line)
(make-single-tag path)
(make-tag g)
(make-tag a)
(make-tag text)
(make-tag tspan)
(make-tag title)

(make-single-tag image)
;(define (image file #:x (x 0) #:y (y 0) #:width (width 100) #:height (height 100))
;  (image-tag 'x x 'y y 'width width 'height height 'xlink:href file))

(module+ test

  (require rackunit)
  (require "../main.rkt")

  (check-equal?
    (svg)
    (rtrim ; remove \r at the end of the string
    #<<SVG
<svg></svg>
SVG
))

  (check-equal?
    (svg xmlns)
    (rtrim
    #<<svg
<svg xmlns="http://www.w3.org/2000/svg"></svg>
svg
))

  (check-equal?
    (svg xmlns xlink)
    (rtrim
    #<<svg
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"></svg>
svg
))

  (check-equal? (g) "<g></g>")
  (check-equal? (g (g)) "<g><g></g></g>")
  (check-equal? (g (@ 'id "id0") (g)) "<g id=\"id0\"><g></g></g>")

  (check-equal? (g (rect x 10)) "<g><rect x=\"10\" /></g>")
  (check-equal? (string-length (g (rect x 10 y 10 width 100 height 100))) (string-length "<g><rect x=\"10\" y=\"10\" width=\"100\" height=\"100\" /></g>"))

  ; simple case
  (check-equal?
    (string-length
      (svg (@)
        (g
          (rect x 10 y 10 width 100 height 150 fill "red" data-comment "rect in the simple case"))))
    (string-length (rtrim
    #<<svg
<svg><g><rect x="10" y="10" width="100" height="150" fill="red" data-comment="rect in the simple case" /></g></svg>
svg
)))

  ;; more complex case
  (check-equal?
    (string-length
      (svg xmlns
        (g)
        (g (@ 'id "group1")
          (rect x 10 y 10 width 100 height 150 fill "green")
          (g (g (@ 'id "subgroup1") (g (rect x 100 y 400 width 200 height 300)))))))
    (string-length (rtrim
    #<<svg
<svg xmlns="http://www.w3.org/2000/svg"><g></g><g id="group1"><rect x="10" y="10" width="100" height="150" fill="green" /><g><g id="subgroup1"><g><rect x="100" y="400" width="200" height="300" /></g></g></g></g></svg>
svg
)))
)
