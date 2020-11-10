#lang racket

(require racket/gui/base)
(require file/convertible)
(require net/base64)
(require stepper/private/xml-snip-helpers)
(require framework)
(require racket/string)
(require racket/class)

(provide render)

(define count 0)
(define (next-count)
  (set! count (+ 1 count))
  count)
(define patches '())
(define (encomment s)
  (define lines (regexp-split #rx"\n" s))
  (define line-lengths (map string-length lines))
  (define max-len (apply max line-lengths))
  (define border (string-append "#| " (make-string max-len #\#) " |#\n"))
  (define clean-lines (map (λ(l len)
                       (string-append l (make-string (- max-len len) #\ )))
                     lines line-lengths))
  (string-append
   border
   (apply string-append
          (add-between
           (map (lambda (s) (string-append "#| " s " |#"))
                clean-lines)
           "\n"))
   "\n" border))

(define (escape-tildes s)
  (string-replace s "~" "~tilde;" #:all? #t))
(define (unescape-tildes s)
  (string-replace s "~tilde;" "~" #:all? #t))
(define (escape-newlines s)
  (string-replace s "\n" "~n" #:all? #t))

(define (add-patch type)
  (λ(contents)
    (define str-contents (if (bytes? contents)
                         (bytes->string/utf-8 contents)
                         contents))
    (define serial (next-count))
    (define escape-contents (escape-newlines (escape-tildes str-contents)))
    (set! patches
          (cons
           (format "~~embed:~a:~a:s~~~a~~embed:~a:e~~"
                   type serial escape-contents serial)
           patches))
    (format "~~embed:~a~~" serial)))

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
         (define cached (hash-ref img-cache contents #f))
         (cond
           [cached ((add-patch "image-file") cached)]
           [else
            (set! cache-count (+ 1 cache-count))
            (define filename (build-path (extracted-dir)
                                         (format "embed~a.png"  cache-count)))
            (display-to-file contents filename #:exists 'replace)
            (hash-set! img-cache contents (path->string filename))
            ((add-patch "image-file") (path->string filename))])]
        [else
         ((add-patch "image-data") (base64-encode contents ""))]))))

(define (display-all snip out #:verbose? [verbose? #t] #:inline-numbers? [inline-numbers? #f])
  (if (not snip)
      out
      (let ((snip-name (send (send snip get-snipclass) get-classname)))
        (cond
          [(is-a? snip string-snip%)
           (display (escape-tildes (send snip get-text 0 (send snip get-count)))
                    out)]
          [(is-a? snip comment-box:snip%)
           (define comment (open-output-bytes))
           (define contents (display-all (send (send snip get-editor)
                                               find-first-snip)
                                         comment
                                         #:verbose? verbose?))
           (display (add-comment (get-output-string comment)) out)]
          [(equal? snip-name
                   "(lib \"number-snip.ss\" \"drscheme\" \"private\")")
           (define val (number->string (send snip get-number)))
           (display (if inline-numbers? val (add-number val)) out)]
          [(or (is-a? snip xml-snip<%>)
               (equal? snip-name "(lib \"xml-snipclass.ss\" \"xml\")")
               (equal? snip-name "(lib \"xml-snipclass.rkt\" \"xml\")")
               (equal? snip-name "(lib \"xml.ss\" \"wxme\")"))
           (define xml (open-output-bytes))
           (define contents (display-all (send (send snip get-editor)
                                               find-first-snip)
                                         xml))
           (display (add-xml (get-output-string xml)) out)]
          [(or (equal? snip-name "(lib \"text-snipclass.ss\" \"xml\")")
               (equal? snip-name "(lib \"text-snipclass.rkt\" \"xml\")")
               (equal? snip-name "(lib \"text.ss\" \"wxme\")"))
           (define text (open-output-bytes))
           (define contents (display-all (send (send snip get-editor)
                                               find-first-snip)
                                         text))
           (display (add-text (get-output-string text)) out)]
          [(equal? snip-name (format "~s" '((lib "collapsed-snipclass.ss" "framework")
                                            (lib "collapsed-snipclass-wxme.ss" "framework"))))
           ;; Display collapsed-snips in collapsed form isn't good for grading:
           ;; it hides code, and could hide badly long lines.  So,
           ;; expand the snip to its hidden pieces, instead.
           (for ([kid (in-list (send snip get-saved-snips))])
             (display-all kid out))]
          ;; [(equal? (send (send snip get-snipclass) get-classname)
          ;;            "wxmedia")
          ;;    (displayln "WXMEDIA")
          ;;    (display-all (send (send snip get-editor) find-first-snip) out)]
          [(or (is-a? snip scheme-snip<%>)
               (equal? snip-name "(lib \"scheme-snipclass.ss\" \"xml\")")
               (equal? snip-name "(lib \"scheme-snipclass.rkt\" \"xml\")")
               (equal? snip-name "(lib \"scheme.ss\" \"wxme\")"))
           (define rkt (open-output-bytes))
           (define contents (display-all (send (send snip get-editor)
                                               find-first-snip)
                                         rkt))
           (if (send snip get-splice?)
               (display (add-splice (get-output-string rkt)) out)
               (display (add-racket (get-output-string rkt)) out))]
          ;; [(is-a? snip editor-snip%)
          ;;    (displayln snip-name (current-error-port))
          ;;    (display-all (send (send snip get-editor) find-first-snip) out
          ;;                 #:verbose? verbose?)]
          [(convertible? snip)
           (define bm (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))
           (define width (box 0))
           (define height (box 0))
           (send snip get-extent bm 0 0 width height)
           (if (or (= (unbox width) 0) (= (unbox height) 0))
               (display (add-empty-image "") out)
               (with-handlers
                   ([exn:fail:contract?
                     (λ(e)
                       (display
                        (add-comment
                         (string-append "Could not render this content;\n"
                                        "please contact a professor."))
                        out))])
                 (display (add-image (convert snip 'png-bytes)) out)))]
          [(syntax? snip)
           (display (syntax->datum snip) out)]
          [else
           (display (format ">?>~a<?<" snip-name) out)
           (display (format ">>>~a<<<" snip) out)]
          )
        (display-all (send snip next) out #:verbose? verbose? #:inline-numbers? inline-numbers?))))
(define (drop-header lines)
  (if (and (cons? lines)
           (regexp-match
            ";; The first three lines of this file were inserted by DrRacket."
            (first lines)))
      (drop lines 3)
      lines))
(define (render file #:verbose? [verbose? #t] #:inline-numbers? [inline-numbers? #f])
  (set! count 0)
  (set! patches '())
  (define dummy (new text%))
  (send dummy load-file file)
  (define first (send dummy find-first-snip))
  (define out (open-output-bytes))
  (define contents (display-all first out #:verbose? verbose? #:inline-numbers? inline-numbers?))
  (when (and verbose? (cons? patches))
    (displayln "~~~~~EMBEDS~~~~~" out))
  (when verbose? (map (λ(p) (displayln p out)) (reverse patches)))
  (define raw-text (get-output-string out))
  (define text (if verbose? raw-text (unescape-tildes raw-text)))
  (apply bytes-append
         (add-between (drop-header (regexp-split #rx#"\n" text))
                      #"\n")))

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

