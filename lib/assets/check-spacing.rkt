#lang racket

(require framework
         wxme
         racket/gui/base
         racket/class
         racket/port
         racket/list racket/set
         racket/string
         racket/contract
         racket/format
         syntax/modread)
(provide
 (contract-out
  ;; Return the line numbers of lines that are too long
  [missing-spaces (->* [string?]
                       [listof
                        [list/c
                         string?                          ;; message
                         (and/c integer? (>=/c 0))]])]))  ;; line#

(define (missing-spaces source)
  (let-values ([(all line-info) (read-stx source)])
    (map render-msg (check-spacing all line-info #f #f))))

(define known-stx (make-hash))
(define known-lines (make-hash))
(define line-offsets (make-hash))
(define (fix-lines file line)
  (+ 0 line (- (hash-ref! line-offsets file (λ () 0)))))
(define (pos->line/col port)
  (define ht (make-hash))
  (let loop ()
    (define-values (line col pos) (port-next-location port))
    (hash-set! ht pos (cons line col))
    (define c (read-char port))
    (unless (eof-object? c)
      (loop)))
  ht)

(define (call-with-naive-reader dir file thunk)
  (with-module-reading-parameterization
    (λ ()
      (parameterize [(current-directory dir)
                     (read-accept-dot #f)
                     (read-accept-infix-dot #f) ;; disable this so we just get the dots as given
                     (current-readtable (make-readtable #f #\. #\a #f))]
        (call-with-input-file file thunk)))))
(define (read-stx file)
  (define syntax
    (hash-ref!
     known-stx file
     (λ ()
       (let-values ([(dir filename _) (split-path file)])
         (call-with-naive-reader
          dir file
          (λ (port)
            (when (is-wxme-stream? port)
              (set! port (wxme-port->port port)))
            (define header ";; The first three lines of this file were inserted by DrRacket.")
            (when (string=? (peek-string (string-length header) 0 port) header)
              (hash-set! line-offsets file 3))
            (port-count-lines! port)
            (read-syntax file port)))))))
  (define line-info
    (hash-ref!
     known-lines file
     (λ ()
       (let-values ([(dir filename _) (split-path file)])
         (call-with-naive-reader
          dir file
          (λ (port)
            (when (is-wxme-stream? port)
              (set! port (wxme-port->text-port port)))
            (define header ";; The first three lines of this file were inserted by DrRacket.")
            (when (string=? (peek-string (string-length header) 0 port) header)
              (hash-set! line-offsets file 3))
            (port-count-lines! port)
            (pos->line/col port)))))))
  (values syntax line-info))
(define (check-spacing stx line-info pos want-space?)
  (define lst (syntax->list stx))
  #;(eprintf "Pos ~a, Syntax ~a,~nlst ~a~n" pos stx lst)
  (define is-quote?
    (and (cons? lst)
         (member (syntax->datum (first lst))
                 '(quote quasiquote unquote unquote-splicing
                         syntax quasisyntax unsyntax unsyntax-splicing))))
  (define is-short-quote?
    (and is-quote? (syntax-span (first lst)) (<= (syntax-span (first lst)) 3)))
  (define spacing-start-errors
    (cond
      [(and pos (syntax-position stx)
            ((if want-space? = >) (syntax-position stx) pos))
       #;(eprintf "~a~n~a~npos ~a, syntax-position stx ~a, want-space? ~a~n"
                  stx lst pos (syntax-position stx) want-space?)
       (list (list 
              (fix-lines (syntax-source stx) (syntax-line stx))
              (syntax-column stx)
              (if want-space? "should" "should not")
              "before"
              (describe (syntax->datum stx))))]
      [else '()]))
  (define spacing-end-errors
    (cond
      [(empty? lst)
       (define start-pos (and (syntax-original? stx) (syntax-position stx)))
       (define final-pos (and (syntax-position stx) (syntax-span stx)
                              (+ (syntax-position stx) (syntax-span stx))))
       (define linecol-at-start (and start-pos (hash-ref line-info start-pos)))
       (define linecol-at-final (and final-pos (hash-ref line-info (sub1 final-pos))))
       #;(eprintf "~a~nstart-pos ~a, final-pos ~a, line-col-at-start ~a, line-col-at-final ~a~n"
                  stx start-pos final-pos linecol-at-start linecol-at-final)
       (if (and linecol-at-start linecol-at-final
                (not (and (= (car linecol-at-start) (car linecol-at-final))
                          (= (cdr linecol-at-start) (sub1 (cdr linecol-at-final))))))
           (list (list (fix-lines (syntax-source stx) (car linecol-at-final))
                       (cdr linecol-at-final)
                       "should not"
                       "before"
                       "parenthesis"))
           '())]
      [(cons? lst)
       (define last-stx (last lst))
       (define last-pos (if (and (syntax-position last-stx) (syntax-span last-stx))
                            (+ (syntax-position last-stx) (syntax-span last-stx))
                            #f))
       (define final-pos (if (and (syntax-original? stx) (syntax-position stx) (syntax-span stx))
                             (+ (syntax-position stx) (syntax-span stx))
                             #f))
       (define linecol-at-last (and last-pos (hash-ref line-info last-pos)))
       (define linecol-at-paren (and final-pos
                                     (hash-ref line-info
                                               (if is-short-quote? final-pos (sub1 final-pos)))))
       (cond
         [(and linecol-at-last linecol-at-paren
               (not (equal? linecol-at-last linecol-at-paren)))
          #;(eprintf "~a~n~a~n~a~nlast-pos ~a, line-col-at-last ~a, line-col-at-paren ~a~n"
                     stx last-stx (syntax->list last-stx) last-pos linecol-at-last linecol-at-paren)
          (list (list (fix-lines (syntax-source stx) (car linecol-at-paren))
                      (cdr linecol-at-paren)
                      "should not"
                      "before"
                      "parenthesis"))]
         [else '()])]
      [else '()]))
  (define stx-errors (append spacing-start-errors spacing-end-errors))
  (cond
    [(list? lst)
     (define-values (_1 _2 errors)
       (for/fold ([want-space? #f]
                  [cur-pos (and (syntax-position stx) (add1 (syntax-position stx)))]
                  [stx-errors stx-errors])
                 ([item (in-list lst)])
         (define next-pos (if (and (syntax-position item) (syntax-span item) (syntax-original? item))
                              (+ (syntax-position item) (syntax-span item))
                              cur-pos))
         (values
          (not is-short-quote?)
          next-pos
          (append (check-spacing item line-info cur-pos want-space?) stx-errors))))
     errors]
    [else stx-errors]))

(define (describe d)
  (cond
    [(member d '(cond if define define-struct if big-bang check-expect)) "keyword"]
    [(symbol? d)
     (if (member d '(.. ... .... ..... ......))
         "placeholder"
         "identifier")]
    [else "expression"]))

(define (render-msg vals)
  (let-values (((line col want-space where type) (apply values vals)))
    (list (format "There ~a be space ~a this ~a, at column ~a"
                  want-space
                  where
                  type
                  col)
          line)))