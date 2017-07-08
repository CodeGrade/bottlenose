#lang racket

(require racket/gui/base)
(require file/convertible)
(require net/base64)
(require stepper/private/xml-snip-helpers)
(require framework)
(require racket/string)

(provide render)

(define count 0)
(define (encomment s)
  (let* ((lines (regexp-split #rx"\n" s))
         (line-lengths
          (map (λ(l)
                 (string-length
                  (regexp-replace* #px"~embed:\\d+:s~[^~]+~embed:\\d+:e~"
                                   l ".")))
               lines))
         (max-len (apply max line-lengths))
         (border (string-append "#| " (make-string max-len #\#) " |#\n"))
         (lines (map (λ(l len)
                       (string-append l (make-string (- max-len len) #\ )))
                     lines line-lengths)))
    (string-append
     border
     (apply string-append
            (add-between
             (map (lambda (s) (string-append "#| " s " |#"))
                  lines)
             "\n"))
     "\n" border)))

(define (escape-tilde s)
  (string-replace s "~" "~tilde;" #:all? #t))

(define (display-all snip out #:verbose? [verbose? #t])
  (if (not snip)
      out
      (let ((snip-name (send (send snip get-snipclass) get-classname)))
        (cond
          [(is-a? snip string-snip%)
           (display (escape-tilde (send snip get-text 0 (send snip get-count)))
                    out)]
          [(is-a? snip comment-box:snip%)
           (let* ((comment (open-output-bytes))
                  (contents (display-all (send (send snip get-editor)
                                               find-first-snip)
                                         comment
                                         #:verbose? verbose?)))
             (display (encomment (get-output-string comment)) out))]
          [(equal? snip-name
                   "(lib \"number-snip.ss\" \"drscheme\" \"private\")")
           (when verbose? (display "#|Number|#" out))
           (display (send snip get-number) out)]
          [(or (is-a? snip xml-snip<%>)
               (equal? snip-name "(lib \"xml-snipclass.ss\" \"xml\")")
               (equal? snip-name "(lib \"xml-snipclass.rkt\" \"xml\")")
               (equal? snip-name "(lib \"xml.ss\" \"wxme\")"))
           (when verbose? (display "#|XML|#" out))
           (display-all (send (send snip get-editor) find-first-snip) out
                        #:verbose? verbose?)]
          [(or (equal? snip-name "(lib \"text-snipclass.ss\" \"xml\")")
               (equal? snip-name "(lib \"text-snipclass.rkt\" \"xml\")")
               (equal? snip-name "(lib \"text.ss\" \"wxme\")"))
           (when verbose? (display "#|TEXT|#" out))
           (display-all (send (send snip get-editor) find-first-snip) out
                        #:verbose? verbose?)]
          #;[(equal? (send (send snip get-snipclass) get-classname)
                     "wxmedia")
             (displayln "WXMEDIA")
             (display-all (send (send snip get-editor) find-first-snip) out)]
          [(or (is-a? snip scheme-snip<%>)
               (equal? snip-name "(lib \"scheme-snipclass.ss\" \"xml\")")
               (equal? snip-name "(lib \"scheme-snipclass.rkt\" \"xml\")")
               (equal? snip-name "(lib \"scheme.ss\" \"wxme\")"))
           (when verbose? (display "#|RACKET|#" out))
           (if (send snip get-splice?)
               (display ",@(" out)
               (display ",(" out))
           (display-all (send (send snip get-editor) find-first-snip) out
                        #:verbose? verbose?)
           (display ")" out)]
          #;[(is-a? snip editor-snip%)
             (displayln snip-name (current-error-port))
             (display-all (send (send snip get-editor) find-first-snip) out
                          #:verbose? verbose?)]
          [(convertible? snip)
           (let ((serial count))
             (set! count (+ 1 count))
             (if verbose?
                 (display
                  (format "~~embed:~a:s~~~a~a~~embed:~a:e~~"
                          serial
                          "data:image/png;base64,"
                          (base64-encode (convert snip 'png-bytes) "")
                          serial) out)
                 (display "~image~" out)))]
          [(syntax? snip)
           (display (syntax->datum snip) out)]
          [else
           (display (format ">?>~a<?<" snip-name) out)
           (display (format ">>>~a<<<" snip) out)]
          )
        (display-all (send snip next) out #:verbose? verbose?))))
(define (drop-header lines)
  (if (and (cons? lines)
           (regexp-match
            ";; The first three lines of this file were inserted by DrRacket."
            (first lines)))
      (drop lines 3)
      lines))
(define (pretty-display contents #:indent [indent ""])
  (cond
    [(list? contents)
     (display indent) (displayln "(")
     (map (λ(c) (pretty-display c #:indent (string-append indent " "))) contents)
     (display indent) (displayln ")")]
    [else
     (display indent) (displayln contents)]))
(define (render file #:verbose? [verbose? #t])
  (let* ((dummy (new text%))
         (_ (send dummy load-file file))
         (first (send dummy find-first-snip))
         (out (open-output-bytes))
         (contents (display-all first out #:verbose? verbose?))
         (text (get-output-string out))
         )
    ;(pretty-display contents)
    (apply bytes-append
           (add-between (drop-header (regexp-split #rx#"\n" text))
                        #"\n"))))

(define output-filename (make-parameter #f))

(module+ main
  (define file-to-compile
    (command-line
     #:program "render-racket"
     #:once-each [("-o") outfile
                         "Output filename (optional)"
                         (output-filename outfile)]
     #:args (filename)
     filename))
  (define rendered (render file-to-compile))
  (if (output-filename)
      (with-output-to-file (output-filename) #:exists 'replace
        (λ() (display rendered)))
      (display rendered)))

;(debug)
