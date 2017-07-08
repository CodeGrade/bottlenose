#lang racket/base

(require framework
         racket/gui/base
         racket/class
         racket/port
         racket/list
         racket/string
         racket/contract
         racket/sequence)
(require "render-racket.rkt")


(provide
 (contract-out
  [correct-parse? (-> string? boolean?)]
  [correct-parse-text? (-> (is-a?/c text%) boolean?)]
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
                                       string? string?
                                       (and/c integer? (>=/c 0))
                                       (and/c integer? (>=/c 0)))])]
  ;; Return the line numbers of lines that are incorrectly indented
  [bad-indentation-text (-> (is-a?/c text%)
                       [listof (list/c (and/c integer? (>=/c 0))
                                       string? string?
                                       (and/c integer? (>=/c 0))
                                       (and/c integer? (>=/c 0)))])]))

(module+ test
  (require rackunit))

(define (load-file source)
  (define f-orig (new frame% [label ""]))
  (define t-orig (new racket:text%))
  (define ec-orig (new editor-canvas% [parent f-orig] [editor t-orig]))
  (define text (bytes->string/utf-8 (render source #:verbose? #f)))
  ;(send t-orig load-file source)
  (send t-orig insert text)
  t-orig)

(define text-balanced? 
  (lambda (text [start 0] [in-end #f])
    (parameterize [(read-accept-reader #t)]
      (let* ([end (or in-end (send text last-position))]
             [port (open-input-text-editor text start end)])
        (with-handlers ([exn:fail:read:eof? (λ (x) #f)]
                        [exn:fail:read? (λ (x) #f)]) ;; Change #t --> #f
          (let ([first (read port)])
            (cond
              [(eof-object? first) #t] ;;; Change #f --> #t
              [else
               (let loop ()
                 (let ([s (read port)])
                   (cond
                     [(eof-object? s) #t]
                     [else (loop)])))])))))))

(define (correct-parse? source)
  (correct-parse-text? (load-file source)))
(define (correct-parse-text? t)
  (text-balanced? t))
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
         (string=? (first all-lines)
                   ";; The first three lines of this file were inserted by DrRacket. They record metadata")))
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
  (send t tabify-all)
  (define tabbed-text (send t get-text))
  (reverse (for/fold ([acc '()])
            ([s-line (in-lines (open-input-string orig-text))]
             [t-line (in-lines (open-input-string tabbed-text))]
             [i (in-naturals 1)])
    (if (or (equal? s-line t-line) ; line is unchanged, or 
            (regexp-match #px"^\\s*$" s-line)) ; ignore whitespace-only lines
        acc
        (let ((orig-indent (- (string-length s-line) (string-length (string-trim s-line #:right? #f))))
              (tabbed-indent (- (string-length t-line) (string-length (string-trim t-line #:right? #f)))))
          (cons (list i s-line t-line orig-indent tabbed-indent) acc))))))
#;(module+ test
  (check-equal? (bad-indentation "(+ 1\n  2)") '())
  (check-equal? (bad-indentation "(+ 1\n    2)") '((1 "    2)" "  2)")))
  (check-equal? (bad-indentation " (+ 1\n    2)") '((0 " (+ 1" "(+ 1")
                                                    (1 "    2)" "  2)"))))
