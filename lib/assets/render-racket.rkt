#lang racket

(require racket/gui/base)
(require file/convertible)
(require net/base64)
(require stepper/private/xml-snip-helpers)
(require framework)
(require racket/string)

(provide render)

(define count 0)
(define (next-count)
  (set! count (+ 1 count))
  count)
(define patches '())
(define (encomment s)
  (let* ((lines (regexp-split #rx"\n" s))
         (line-lengths (map string-length lines))
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

(define (escape-tildes s)
  (string-replace s "~" "~tilde;" #:all? #t))
(define (unescape-tildes s)
  (string-replace s "~tilde;" "~" #:all? #t))
(define (escape-newlines s)
  (string-replace s "\n" "~n" #:all? #t))

(define (add-patch type)
  (λ(contents)
    (let* ((contents (if (bytes? contents)
                         (bytes->string/utf-8 contents)
                         contents))
           (serial (next-count))
           (contents (escape-newlines (escape-tildes contents))))
      (set! patches
            (cons
             (format "~~embed:~a:~a:s~~~a~~embed:~a:e~~"
                     type serial contents serial)
             patches))
    (format "~~embed:~a~~" serial))))

(define add-comment (add-patch "comment"))
(define add-xml (add-patch "xml"))
(define add-text (add-patch "text"))
(define add-number (add-patch "number"))
(define add-racket (add-patch "racket"))
(define add-splice (add-patch "splice"))
(define add-empty-image (add-patch "empty"))
(define add-image
  (let ((img-cache (make-hash))
        (cache-count 0))
    (λ(contents)
      (cond
        [(extracted-dir)
         (let ((cached (hash-ref img-cache contents #f)))
           (cond
             [cached ((add-patch "image-file") cached)]
             [else
              (set! cache-count (+ 1 cache-count))
              (let ((filename (build-path (extracted-dir)
                                          (format "embed~a.png"  cache-count))))
                (display-to-file contents filename #:exists 'replace)
                (hash-set! img-cache contents (path->string filename))
                ((add-patch "image-file") (path->string filename)))]))]
        [else
         ((add-patch "image-data") (base64-encode contents ""))]))))

(define (display-all snip out #:verbose? [verbose? #t])
  (if (not snip)
      out
      (let ((snip-name (send (send snip get-snipclass) get-classname)))
        (cond
          [(is-a? snip string-snip%)
           (display (escape-tildes (send snip get-text 0 (send snip get-count)))
                    out)]
          [(is-a? snip comment-box:snip%)
           (let* ((comment (open-output-bytes))
                  (contents (display-all (send (send snip get-editor)
                                               find-first-snip)
                                         comment
                                         #:verbose? verbose?)))
             (display (add-comment (get-output-string comment)) out))]
          [(equal? snip-name
                   "(lib \"number-snip.ss\" \"drscheme\" \"private\")")
           (display (add-number (number->string (send snip get-number))) out)]
          [(or (is-a? snip xml-snip<%>)
               (equal? snip-name "(lib \"xml-snipclass.ss\" \"xml\")")
               (equal? snip-name "(lib \"xml-snipclass.rkt\" \"xml\")")
               (equal? snip-name "(lib \"xml.ss\" \"wxme\")"))
           (let* ((xml (open-output-bytes))
                  (contents (display-all (send (send snip get-editor)
                                               find-first-snip)
                                         xml)))
             (display (add-xml (get-output-string xml)) out))]
          [(or (equal? snip-name "(lib \"text-snipclass.ss\" \"xml\")")
               (equal? snip-name "(lib \"text-snipclass.rkt\" \"xml\")")
               (equal? snip-name "(lib \"text.ss\" \"wxme\")"))
           (let* ((text (open-output-bytes))
                  (contents (display-all (send (send snip get-editor)
                                               find-first-snip)
                                         text)))
             (display (add-text (get-output-string text)) out))]
          ;; [(equal? (send (send snip get-snipclass) get-classname)
          ;;            "wxmedia")
          ;;    (displayln "WXMEDIA")
          ;;    (display-all (send (send snip get-editor) find-first-snip) out)]
          [(or (is-a? snip scheme-snip<%>)
               (equal? snip-name "(lib \"scheme-snipclass.ss\" \"xml\")")
               (equal? snip-name "(lib \"scheme-snipclass.rkt\" \"xml\")")
               (equal? snip-name "(lib \"scheme.ss\" \"wxme\")"))
           (let* ((rkt (open-output-bytes))
                  (contents (display-all (send (send snip get-editor)
                                               find-first-snip)
                                         rkt)))
             (if (send snip get-splice?)
                 (display (add-splice (get-output-string rkt)) out)
                 (display (add-racket (get-output-string rkt)) out)))]
          ;; [(is-a? snip editor-snip%)
          ;;    (displayln snip-name (current-error-port))
          ;;    (display-all (send (send snip get-editor) find-first-snip) out
          ;;                 #:verbose? verbose?)]
          [(convertible? snip)
           (let ((converted
                  (with-handlers ([exn:fail:contract?
                                   (λ(e) 'conversion-failure)])
                    (convert snip 'png-bytes 'conversion-failure))))
             (if (equal? converted 'conversion-failure)
                 (display (add-empty-image "")
                          out)
                 (display (add-image (convert snip 'png-bytes))
                          out)))]
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
(define (render file #:verbose? [verbose? #t])
  (let* ((dummy (new text%))
         (_ (send dummy load-file file))
         (first (send dummy find-first-snip))
         (out (open-output-bytes))
         (contents (display-all first out #:verbose? verbose?))
         (_ (when (and verbose? (cons? patches))
              (displayln "~~~~~EMBEDS~~~~~" out)))
         (_ (when verbose? (map (λ(p) (displayln p out)) (reverse patches))))
         (text (get-output-string out))
         (text (if verbose? text (unescape-tildes text)))
         )
    (apply bytes-append
           (add-between (drop-header (regexp-split #rx#"\n" text))
                        #"\n"))))

(define output-filename (make-parameter #f))
(define extracted-dir   (make-parameter #f))

(module+ main
  (define file-to-compile
    (command-line
     #:program "render-racket"
     #:once-each
     [("-o") outfile
      "Output filename (optional)"
      (output-filename outfile)]
     [("-e" "--extracted") extract
      "Extracted directory for images (optional)"
      (extracted-dir extract)]
     #:args (filename)
     filename))
  (define rendered (render file-to-compile))
  (if (output-filename)
      (with-output-to-file (output-filename) #:exists 'replace
        (λ() (display rendered)))
      (display rendered)))

