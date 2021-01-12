#lang racket/base

(require "linter.rkt" "check-spacing.rkt")
(require racket/string
         racket/list racket/set
         racket/cmdline
         racket/path
         racket/class)

(define output-filename (make-parameter #f))
(define total-points (make-parameter 50))
(define line-width (make-parameter 80))

(define messages '())

(define (escape-string message)
  (string-replace
   (string-replace
    (string-replace message "\\" "\\\\")
    "\n" "\\n")
   "\"" "\\\""))
(define (tap #:problem problem #:filename filename #:line line
             #:penalty penalty #:message message)
  (set! messages
        (cons
         (list
          (format "not ok ~a ~a" (+ 1 (length messages)) problem)
          (format "# More information")
          (format "  ---")
          (format "  message: \"~a\"" (escape-string message))
          (format "  filename: \"~a\"" filename)
          (format "  line: ~a" line)
          (format "  category: ~a" problem)
          (format "  severity: Error")
          (format "  weight: ~a" penalty)
          (format "  suppressed: false")
          (format "  ..."))
         messages)))

(define (process filename width)
  (begin
    (displayln filename (current-error-port))
    (define t (load-file filename))
    (define parse-error (parse-errors-text t))
    (if (list? parse-error)
        (tap #:problem "CleanParse"
             #:filename filename
             #:line (second parse-error)
             #:penalty (total-points)
             #:message (format "Syntax error: ~a" (first parse-error)))
        (with-handlers ([exn:fail:read:rethrow?
                         (λ(err)
                           (define msg (exn-message err))
                           (define srclocs (exn:fail:read-srclocs err))
                           (define srcloc (if (cons? srclocs) (first srclocs) srclocs))
                           (define line (srcloc-line srcloc))
                           (tap #:problem "CleanParse"
                                #:filename filename
                                #:line line
                                #:penalty (total-points)
                                #:message (format "Syntax error: ~a" msg)))]
                        [exn:fail:read?
                         (λ(err)
                           (define msg (exn-message err))
                           (define srclocs (exn:fail:read-srclocs err))
                           (define srcloc (if (cons? srclocs) (first srclocs) srclocs))
                           (define line
                             (+ 1 (send t position-line (srcloc-position srcloc))))
                           (tap #:problem "CleanParse"
                                #:filename filename
                                #:line line
                                #:penalty (total-points)
                                #:message (format "Syntax error: ~a" msg)))])
          (for [(line-info (bad-widths-text t #:width width))]
            (let-values (((line-num length contents) (apply values line-info)))
              (tap #:problem "LineLength"
                   #:filename filename
                   #:line line-num
                   #:penalty 1
                   #:message (format
                              (string-append "This line must be no longer than ~a characters.  "
                                             "Please reformat the code.\n")
                              width))))
          (for [(line-info (bad-indentation-text t))]
            (define-values (line before orig-indent correct-indents) (apply values line-info))
            (define correct-indent
              (if (= 1 (set-count correct-indents))
                  (set-first correct-indents)
                  (string-join (map number->string (sort (set->list correct-indents) <))
                               ", "
                               #:before-first "either "
                               #:before-last " or ")))
            (define tab-warning
              (if (string-contains? before "\t")
                  "  (Reminder: you should not have any tab characters in your code!"
                  ""))
            (tap #:problem "Indentation"
                 #:filename filename
                 #:line line
                 #:penalty 1
                 #:message (format
                            (string-append "This line is not properly indented: "
                                           "it should have ~a spaces of indentation."
                                           "~a  Please reformat the code.")
                            correct-indent
                            tab-warning)))
          (for [(line-info (missing-spaces filename))]
            (define-values (message line) (apply values line-info))
            (tap #:problem "Whitespace"
                 #:filename filename
                 #:line line
                 #:penalty 1
                 #:message message))))))
(define (print-output)
  (begin
    (displayln "TAP version 13")
    (displayln (format "1..~a" (length messages)))
    (displayln (format "# Time: ~a" (exact->inexact (/ (current-process-milliseconds) 1000))))
    (displayln (format "# TOTAL POINTS: ~a" (total-points)))
    (displayln (format "# Tests run: ~a, Failures: ~a" (length messages) (length messages)))
    (for-each displayln (flatten (reverse messages)))))
  
(define (process-files start width)
  (begin
    (set! messages '())
    (cond
      [(and (file-exists? start)
            (or (bytes=? (path-get-extension start) #".rkt")
                (bytes=? (path-get-extension start) #".ss")))
       (process start width)]
      [(directory-exists? start)
       (define found #f)
       (for ([p (in-directory start)])
         (when (and (file-exists? p)
                    (or (equal? (path-get-extension p) #".rkt")
                        (equal? (path-get-extension p) #".ss")))
           (set! found #t)
           (process (format "~a" p) width)))
       (if found (void)
           (tap #:problem "NoFiles"
                #:filename start
                #:line 0
                #:penalty (total-points)
                #:message "No files found in this submission"))]
      [else
       (void)])
    (print-output)))

(module+ main

  (define file-to-compile
    (command-line
     #:program "checkstyle"
     #:once-each
     [("-o") outfile
             "Output filename (optional)"
             (output-filename outfile)]
     [("--max-points") maxPoints
                       "Maximum points available (optional)"
                       (total-points (string->number maxPoints))]
     [("--line-width") lineWidth
                       "Maximum allowed line width (optional)"
                       (line-width (string->number lineWidth))]
     #:args (filename)
     filename))



  (if (output-filename)
      (with-output-to-file (output-filename) #:exists 'replace
        (λ () (process-files file-to-compile (line-width))))
      (process-files file-to-compile (line-width))))
