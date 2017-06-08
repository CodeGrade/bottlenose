#lang racket

;(require wxme/private/readable-editor)
(require file/convertible)
(require net/base64)
(require xml)

(require racket/gui/base)
(require framework)

(define count 0)
(define (encomment s)
  (apply bytes-append
         (map (lambda (s)
                (bytes-append #";;; " s #"\n"))
              (regexp-split #rx#"\n" s))))

(define (display-all snip out)
  (if (not snip)
      out
      (begin
        (cond
          [(is-a? snip string-snip%)
           (display (send snip get-text 0 (send snip get-count)) out)]
          [(is-a? snip comment-box:snip%)
           (let* ((comment (open-output-bytes))
                  (contents (display-all (send (send snip get-editor) find-first-snip) comment)))
             (display (encomment (get-output-string contents)) out))]
          [(equal? (send (send snip get-snipclass) get-classname)
                   "(lib \"number-snip.ss\" \"drscheme\" \"private\")")
           (display "#|Number widget|#" out)
           (display (send snip get-number) out)
           ]
          [(equal? (send (send snip get-snipclass) get-classname)
                   "(lib \"xml-snipclass.ss\" \"xml\")")
           (display "#|XML|#" out)
           (display-all (send (send snip get-editor) find-first-snip) out)
           ]
          [(equal? (send (send snip get-snipclass) get-classname)
                   "(lib \"scheme-snipclass.ss\" \"xml\")")
           (display "#|RACKET|#" out)
           (if (send snip get-splice?)
               (display ",@(" out)
               (display ",(" out))
           (display-all (send (send snip get-editor) find-first-snip) out)
           (display ")" out)
           ]
          [(is-a? snip editor-snip%)
           (display-all (send (send snip get-editor) find-first-snip) out)
           ]
          [(convertible? snip)
           (let ((serial count))
             (set! count (+ 1 count))
             (display (format "~~embed:~a:s~~data:image/png;base64,~a~~embed:~a:e~~" serial (base64-encode (convert snip 'png-bytes) "") serial) out))]
          [else
           (display (format ">>>~a<<<" snip) out)]
          )
        (display-all (send snip next) out))))
(define (render file)
  (let* ((dummy (new text%))
         (_ (send dummy load-file file))
         (first (send dummy find-first-snip))
         (text (get-output-string (display-all first (open-output-bytes))))
         )
    (apply bytes-append
           (add-between (drop (regexp-split #rx#"\n" text) 3)
                        #"\n"))))

(define output-filename (make-parameter #f))

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
    (with-output-to-file (output-filename) #:exists 'replace (λ() (display rendered)))
    (display rendered))