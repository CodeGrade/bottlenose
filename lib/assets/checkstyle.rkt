#lang racket/base

(require "linter.rkt")
(require racket/string
         racket/match
         racket/list
         racket/cmdline)

(define output-filename (make-parameter #f))
(define total-points (make-parameter 50))
(define line-width (make-parameter 80))

(define messages '())

(define (tap #:problem problem #:filename filename #:line line #:penalty penalty #:message message)
  (set! messages
        (cons
         (list
          (format "not ok ~a ~a" (+ 1 (length messages)) problem)
          (format "# More information")
          (format "  ---")
          (format "  message: \"~a\"" (string-replace (string-replace (string-replace message "\\" "\\\\") "\n" "\\n") "\"" "\\\""))
          (format "  filename: \"~a\"" filename)
          (format "  line: ~a" line)
          (format "  category: ~a" problem)
          (format "  severity: Error")
          (format "  weight: ~a" penalty)
          (format "  suppressed: false")
          (format "  ...")
          )
         messages)))

(define (process filename width)
  (begin
    (if (not (correct-parse? filename))
        (tap #:problem "CleanParse"
             #:filename filename
             #:line 0
             #:penalty (total-points)
             #:message "The program would not parse correctly: please revise the code")
        (begin
          (for [(line-info (bad-widths filename #:width width))]
            (match line-info
              [(list line-num length contents)
               (tap #:problem "LineLength"
                    #:filename filename
                    #:line (+ 1 line-num)
                    #:penalty 1
                    #:message (format "This line has length ~a, but must be no longer than ~a characters.  Please reformat the code.\n"
                                      length width))])
            )
          (for [(line-before-after (bad-indentation filename))]
            (match line-before-after
              [(list line before after orig-indent correct-indent)
               (define tab-warning
                 (if (string-contains? before "\t")
                     "  (Reminder: you should not have any tab characters in your code!"
                     ""))
               (tap #:problem "Indentation"
                    #:filename filename
                    #:line (+ 1 line)
                    #:penalty 1
                    #:message (format
                               "This line is not properly indented: it should have ~a spaces of indentation.~a  Please reformat the code."
                               correct-indent
                               tab-warning)
                    )]))
          ))
    (displayln "TAP version 13")
    (displayln (format "1..~a" (length messages)))
    (displayln (format "# Time: ~a" (exact->inexact (/ (current-process-milliseconds) 1000))))
    (displayln (format "# TOTAL POINTS: ~a" (total-points)))
    (displayln (format "# Tests run: ~a, Failures: ~a" (length messages) (length messages)))
    (for-each displayln (flatten (reverse messages)))
    ))
  
(define (process-files start width)
  (cond
   [(file-exists? start)
    (process start width)]
   [(directory-exists? start)
    (for ([p (in-directory start)])
      (when (file-exists? p)
          (process (format "~a" p) width)))]
   [else
    (void)]))


(define file-to-compile
  (command-line
   #:program "render-racket"
   #:once-each
   [("-o") outfile
    "Output filename (optional)"
    (output-filename outfile)]
   [("--max-points") maxPoints
    "Maximum points available (optional)"
    (total-points maxPoints)]
   [("--line-width") lineWidth
    "Maximum allowed line width (optional)"
    (line-width lineWidth)]
   #:args (filename)
   filename))



(if (output-filename)
    (with-output-to-file (output-filename) #:exists 'replace
      (λ() (process-files file-to-compile (line-width))))
    (process-files file-to-compile (line-width)))
