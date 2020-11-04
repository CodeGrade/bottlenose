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
         syntax/modread
         syntax-color/module-lexer)
(provide
 (contract-out
  ;; Return the line numbers of lines that are too long
  [missing-spaces (->* [string?]
                       [listof
                        [list/c
                         string?                          ;; message
                         (and/c integer? (>=/c 0))]])]))  ;; line#

(define (missing-spaces source)
  (let-values ([(all line-info comment-info) (read-stx source)])
    (map render-msg (check-spacing all line-info comment-info #f 'no))))
(define (missing-spaces-text source)
  (let-values ([(all line-info comment-info) (read-stx-text source)])
    (map render-msg (check-spacing all line-info comment-info #f 'no))))

(define known-stx (make-hash))
(define known-lines (make-hash))
(define known-comments (make-hash))
(define line-offsets (make-hash))
(define (fix-lines file line)
  (+ 0 line (- (hash-ref! line-offsets file (λ () 0)))))
(define (pos->line/col port)
  (define ht (make-hash))
  (let loop ()
    (define-values (line col pos) (port-next-location port))
    (hash-set! ht pos (cons line col))
    (define c (read-char-or-special port))
    (unless (eof-object? c)
      (loop)))
  ht)

(define (tokens-for port)
  (let loop ([mode #f])
    (define-values (text tok delim start-pos end-pos _ new-mode) (module-lexer port 0 mode))
    (cond
      [(symbol=? tok 'eof) (list (list tok start-pos end-pos))]
      [else (cons (list (or delim tok) start-pos end-pos) (loop new-mode))])))

(define-struct comment-tok [start end sexp?] #:transparent)
(define-struct xref [prev-is-comment? prev-start prev-comment-start] #:transparent)
; Generate a dictionary from token start-positions to either comment-toks or xrefs
; A comment-tok stores its extent and whether it's an sexp-comment
; An xref stores whether the previous non-whitespace token is a comment, and links
; back to the previous token and the previous comment token, as well as store its own type.
(define (pos->comment-dict port)
  (define all-toks (tokens-for port))
  (define tok-dict (make-hash))
  (for/fold ([prev-comment-start #f]
             [prev-tok-start #f]
             [prev-is-comment #f]
             #:result tok-dict)
            ([tok-info (in-list all-toks)])
    (define-values (tok start-pos end-pos) (apply values tok-info))
    (cond
      [(member tok '(comment sexp-comment))
       (hash-set! tok-dict start-pos (make-comment-tok start-pos end-pos (equal? tok 'sexp-comment)))
       (values start-pos start-pos #t)]
      [else
       (hash-set! tok-dict start-pos
                  (make-xref prev-is-comment
                             prev-tok-start prev-comment-start))
       (values prev-comment-start start-pos (and (symbol=? tok 'white-space) prev-is-comment))])))



(define (read-comments-readtable #:readtable [readtable #f])
  (make-readtable
   (or readtable (current-readtable))
   #\. #\a #f ;; handle infix dots as symbols
   #\; 'dispatch-macro ;; _read_ sexp comments as if they're sexps
   (case-lambda 
     [(char port)
      (read/recursive port #\space)]
     [(char port src line col pos)
      (define ans (read-syntax/recursive src port #\space))
      (datum->syntax ans (make-special-comment (syntax->datum ans)) ans ans ans)])))

(define (call-with-naive-reader dir file handle-wxme thunk)
  (with-module-reading-parameterization
    (λ ()
      (parameterize [(current-directory dir)
                     (read-accept-dot #f)
                     (read-accept-infix-dot #f) ;; disable this so we just get the dots as given
                     (current-readtable (read-comments-readtable))]
        (call-with-input-file file
          (λ (port)
            (when (and handle-wxme (is-wxme-stream? port))
              (set! port (handle-wxme port)))
            (define header ";; The first three lines of this file were inserted by DrRacket.")
            (when (string=? (peek-string (string-length header) 0 port) header)
              (hash-set! line-offsets file 3))
            (port-count-lines! port)
            (thunk port)))))))
(define (call-with-naive-string-reader text thunk)
  (with-module-reading-parameterization
    (λ ()
      (parameterize [(read-accept-dot #f)
                     (read-accept-infix-dot #f) ;; disable this so we just get the dots as given
                     (current-readtable (read-comments-readtable))]
        (call-with-input-string text
          (λ (port)
            (define header ";; The first three lines of this file were inserted by DrRacket.")
            (when (string=? (peek-string (string-length header) 0 port) header)
              (hash-set! line-offsets text 3))
            (port-count-lines! port)
            (thunk port)))))))


(define (read-stx file)
  (define syntax
    (hash-ref!
     known-stx file
     (λ ()
       (let-values ([(dir filename _) (split-path file)])
         (call-with-naive-reader
          dir file wxme-port->port
          (λ (port) (read-syntax file port)))))))
  (define line-info
    (hash-ref!
     known-lines file
     (λ ()
       (let-values ([(dir filename _) (split-path file)])
         (call-with-naive-reader
          dir file wxme-port->port
          pos->line/col)))))
  (define comment-toks
    (hash-ref!
     known-comments file
     (λ ()
       (let-values ([(dir filename _) (split-path file)])
         (call-with-naive-reader
          dir file wxme-port->port
          pos->comment-dict)))))
  (values syntax line-info comment-toks))
(define (read-stx-text text)
  (values (call-with-naive-string-reader text (λ (port) (read-syntax text port)))
          (call-with-naive-string-reader text pos->line/col)
          (call-with-naive-string-reader text pos->comment-dict)))

;; space is one of 'no, 'yes, #f (for ignoring whether there's any whitespace, either way)
(define (check-spacing stx line-info comment-info prev-pos space)
  (define lst (syntax->list stx))
  (define (preceeded-by-comment? pos)
    (define ci-of-pos (and pos (hash-ref comment-info pos #false)))
    (and (xref? ci-of-pos) (xref-prev-is-comment? ci-of-pos)))
  (define (is-block-comment? pos)
    (define ci-of-pos (and pos (hash-ref comment-info pos #false)))
    (and (comment-tok? ci-of-pos) (comment-tok-sexp? ci-of-pos)))
  (define is-quote?
    (and (cons? lst)
         (member (syntax->datum (first lst))
                 '(quote quasiquote unquote unquote-splicing
                         syntax quasisyntax unsyntax unsyntax-splicing))))
  (define is-short-quote?
    (and is-quote? (syntax-span (first lst)) (<= (syntax-span (first lst)) 3)))
  (define spacing-start-errors
    (cond
      [(and prev-pos space (syntax-position stx)
            ((if (symbol=? space 'yes) = >) (syntax-position stx) prev-pos)
            (not (preceeded-by-comment? (syntax-position stx))))
       (list (list 
              (fix-lines (syntax-source stx) (syntax-line stx))
              (syntax-column stx)
              (if (symbol=? space 'yes) "should" "should not")
              "before"
              (if (or (syntax-property stx 'paren-shape) lst)
                  (describe-paren (syntax-property stx 'paren-shape) "left" "parenthesis")
                  (describe (syntax->datum stx)))))]
      [else '()]))
  (define spacing-end-errors
    (cond
      [(empty? lst)
       (define start-pos (and (syntax-original? stx) (syntax-position stx)))
       (define final-pos (and (syntax-position stx) (syntax-span stx)
                              (+ (syntax-position stx) (syntax-span stx))))
       (define linecol-at-start (and start-pos (hash-ref line-info start-pos)))
       (define linecol-at-final (and final-pos (hash-ref line-info (sub1 final-pos))))
       (cond
         [(and (not (preceeded-by-comment? (sub1 final-pos)))
               linecol-at-start linecol-at-final
               (not (and (= (car linecol-at-start) (car linecol-at-final))
                         (= (cdr linecol-at-start) (sub1 (cdr linecol-at-final))))))
          (list (list (fix-lines (syntax-source stx) (car linecol-at-final))
                      (cdr linecol-at-final)
                      "should not"
                      "before"
                      (describe-paren (syntax-property stx 'paren-shape) "right" "parenthesis")))]
         [else '()])]
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
         [(and final-pos (not (preceeded-by-comment? (sub1 final-pos)))
               linecol-at-last linecol-at-paren
               (not (equal? linecol-at-last linecol-at-paren)))
          (list (list (fix-lines (syntax-source stx) (car linecol-at-paren))
                      (cdr linecol-at-paren)
                      "should not"
                      "before"
                      (describe-paren (syntax-property stx 'paren-shape) "right" "parenthesis")))]
         [else '()])]
      [else '()]))
  (define stx-errors (append spacing-start-errors spacing-end-errors))
  (cond
    [(list? lst)
     (for/fold ([space 'no]
                [cur-pos (and (syntax-position stx) (add1 (syntax-position stx)))]
                [stx-errors stx-errors]
                #:result stx-errors)
               ([item (in-list lst)])
       (define next-pos (if (and (syntax-position item) (syntax-span item) (syntax-original? item))
                            (+ (syntax-position item) (syntax-span item))
                            cur-pos))
       (cond
         [(or (special-comment? item)
              (preceeded-by-comment? next-pos))
          (values 'ignore next-pos stx-errors)]
         [else
          (values
           (if (not is-short-quote?) 'yes 'no)
           next-pos
           (append (check-spacing item line-info comment-info cur-pos space) stx-errors))]))]
    [else stx-errors]))

(define (describe d)
  (cond
    [(member d '(cond if define define-struct if big-bang check-expect)) "keyword"]
    [(symbol? d)
     (if (member d '(.. ... .... ..... ......))
         "placeholder"
         "identifier")]
    [else "expression"]))

(define (describe-paren paren-shape side default)
  (cond [(equal? paren-shape #\() (string-append side "-parenthesis")]
        [(equal? paren-shape #\[) (string-append side "-bracket")]
        [(equal? paren-shape #\{) (string-append side "-brace")]
        [else (string-append side "-" default)]))

(define (render-msg vals)
  (let-values (((line col want-space where type) (apply values vals)))
    (list (format "There ~a be space ~a this ~a, at column ~a"
                  want-space
                  where
                  type
                  col)
          line)))

#;(module+ test
  (require rackunit))

#;(module+ test
  (define (check-num-errors num text)
    (check-equal? (length (missing-spaces-text text)) num))
  (check-num-errors 0 "(+ 1 2)")
  (check-num-errors 1 "(sin(+ 1 2))")
  (check-num-errors 1 "(local [\n(define x 5)] x)")
  (check-num-errors 2 "(local [\n(define x 5)\n] x)")
  (check-num-errors 0 "(local [; comment\n(define x 5)] x)")
  (check-num-errors 0 "(local [#;comment\n(define x 5)] x)")
  (check-num-errors 0 "(local [#|comment|#\n(define x 5)] x)")
  (check-num-errors 0 "(local [#|comment|#(define x 5)] x)"))