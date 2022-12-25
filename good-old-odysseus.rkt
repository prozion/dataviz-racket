#lang racket

(require odysseus/base)

(provide (all-defined-out))

(define (zor . body)
  (cond
    ((null? body) #t)
    ((null? (cdr body)) (car body))
    (else (if
            (or
              (nil? (car body))
              (equal? (car body) 0))
                (apply zor (cdr body))
                (car body)))))

(define (print-hash format-str h)
  (for/fold
    ([res ""])
    ([(k v) (in-hash h)])
    (string-append res (format format-str k v))))

(define (setn seq index newel)
  (cond
    ((nil? seq) seq)
    ((> index (length seq)) seq)
    (else (list-update seq index (const newel)))))
