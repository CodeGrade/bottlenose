#lang racket/base

(require framework
         racket/gui/base
         racket/class
         racket/port
         racket/list racket/set
         racket/string
         racket/contract
         racket/format
         syntax/modread)
(require "render-racket.rkt" "retab.rkt")


(provide
 (contract-out
  [parse-errors (-> string? (or/c boolean? [list/c string? (and/c integer? (>=/c 0))]))]
  [parse-errors-text (-> (is-a?/c text%) (or/c boolean? [list/c string? (and/c integer? (>=/c 0))]))]
  [load-file (-> string? (is-a?/c text%))]
  ;; Return the line numbers of lines that are too long
  [bad-widths (->* [string?] [#:width (and/c integer? positive?)]
                   [listof
                    [list/c
                     (and/c integer? (>=/c 0))
                     (and/c integer? (>=/c 0))
                     string?]])]
  ;; Return the line numbers of lines that are too long
  [bad-widths-text (->* [(is-a?/c text%)] [#:width (and/c integer? positive?)]
                        [listof
                         [list/c
                          (and/c integer? (>=/c 0))
                          (and/c integer? (>=/c 0))
                          string?]])]
  ;; Return the line numbers of lines that are incorrectly indented
  [bad-indentation (-> string?
                       [listof (list/c (and/c integer? (>=/c 0))
                                       string?
                                       (and/c integer? (>=/c 0))
                                       (set/c (and/c integer? (>=/c 0))))])]
  ;; Return the line numbers of lines that are incorrectly indented
  [bad-indentation-text (-> (is-a?/c text%)
                            [listof (list/c (and/c integer? (>=/c 0))
                                            string?
                                            (and/c integer? (>=/c 0))
                                            (set/c (and/c integer? (>=/c 0))))])]))

(module+ test
  (require rackunit))

(define (load-file source)
  (string->text (bytes->string/utf-8 (render source #:verbose? #f))))

(define (string->text str)
  (define f-orig (new frame% [label ""]))
  (define t-orig (new racket:text%))
  (define ec-orig (new editor-canvas% [parent f-orig] [editor t-orig]))
  ;(send t-orig load-file source)
  (send t-orig insert str)
  t-orig)

(define (text-balanced? text [start 0] [in-end #f])
  (parameterize [(read-accept-reader #t)
                 (read-accept-lang #t)]
    (let* ([end (or in-end (send text last-position))]
           [port (open-input-text-editor text start end)])
      (with-handlers ([exn:fail:read:eof? (λ (x) x)]
                      [exn:fail:read? (λ (x) x)]) ;; Change #t --> x
        (let ([first (read port)])
          (cond
            [(eof-object? first) #t] ;;; Change #f --> x
            [else
             (let loop ()
               (let ([s (read port)])
                 (cond
                   [(eof-object? s) #t]
                   [else (loop)])))]))))))

(define (parse-errors source)
  (parse-errors-text (load-file source)))
(define (parse-errors-text t)
  (let ([err (text-balanced? t)])
    (if (boolean? err)
        err
        (list (exn-message err)
              (+ 1 (send t position-line
                         (srcloc-position (first (exn:fail:read-srclocs err)))))))))



#;(module+ test
    (check-equal? (correct-parse? "(+ 1 2)") #t)
    (check-equal? (correct-parse? "(+ 1") #f))

(define (correct-width? source
                        #:width [width 80])
  (null? (bad-widths source #:width width)))
#;(module+ test
    (check-equal? (correct-width? "abcdefg") #t)
    (check-equal? (correct-width? "abcdefg" #:width 2) #f))

(define (bad-widths source
                    #:width [width 80])
  (bad-widths-text (load-file source) #:width width))
(define (bad-widths-text t #:width [width 80])
  (define last-line (send t last-line))
  (define all-lines (port->lines (open-input-string (send t get-text))))
  (define drrackety?
    (and (cons? all-lines)
         (regexp-match
          ";; The first three lines of this file were inserted by DrRacket."
          (first all-lines))))
  (define real-lines (if drrackety? (drop all-lines 3) all-lines))
  (reverse
   (for/fold ([acc '()])
             ([line (in-list real-lines)]
              [line-num (in-naturals 1)])
     (define line-length (string-length line))
     (if (line-length . <= . width)
         acc
         (cons (list line-num line-length line)
               acc)))))
#;(module+ test
    (check-equal? (bad-widths "abcdefg") '())
    (check-equal? (bad-widths "abcdefg" #:width 2) '((0 . 7)))
    (check-equal? (bad-widths "abcd\nefg" #:width 2) '((0 . 4) (1 . 3)))
    (check-equal? (bad-widths "ab\ncde\nfg" #:width 2) '((1 . 3))))

(define (bad-indentation source)
  (bad-indentation-text (load-file source)))
(define (bad-indentation-text t)
  (define orig-text (send t get-text))
  (define-values (untabbed tabbed lambda-tabbed) (tabify-text t))
  (send t erase)
  (send t insert orig-text)
  (reverse
   (for/fold ([acc '()])
             ([s-line (in-lines (open-input-string untabbed))]
              [t-line (in-lines (open-input-string tabbed))]
              [l-line (in-lines (open-input-string lambda-tabbed))]
              [i (in-naturals 1)])
     (if (or (equal? s-line t-line) ; line is unchanged in default indentation, or
             (equal? s-line l-line) ; line is unchanged in lambda indentation, or
             (regexp-match #px"^\\s*$" s-line)) ; ignore whitespace-only lines
         acc
         (let ((indents (map (λ (l) (- (string-length l)
                                       (string-length (string-trim l #:right? #f))))
                             (list s-line t-line l-line))))
           (cons (list i s-line (first indents) (list->set (rest indents))) acc))))))
#;(module+ test
    (check-equal? (bad-indentation "(+ 1\n  2)") '())
    (check-equal? (bad-indentation "(+ 1\n    2)") '((1 "    2)" "  2)")))
    (check-equal? (bad-indentation " (+ 1\n    2)") '((0 " (+ 1" "(+ 1")
                                                      (1 "    2)" "  2)"))))
