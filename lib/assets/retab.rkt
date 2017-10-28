#lang racket

;; Read in a source file and tabify it according to the following three tabbing styles:
;;
;; 1. Untabbed (as writen directly in the file)
;; 2. Standard DrRacket tabbing excluding the big-bang default.
;;    big-bang indents as:
;;      (big-bang a
;;                b)
;; 3. Standard DrRacket tabbing including the big-bang default.
;;    big-bang indents as:
;;      (big-bang a
;;        b)
;;
;; (-> racket:text%? (values string? string? string?))
;;
;; WARNING!!! This function likely has effects based on the framework library. It should NOT touch
;; your filesystem. However, this module should not be instantiated alongside
;; other DrRacket preferences.
;;
(provide tabify-text)

(require framework/preferences)

(define (tabify-text t)
  (parameterize* ([preferences:low-level-put-preferences
                   (λ _ (void))]
                  [preferences:low-level-get-preference
                   (λ _ #f)])
    (define untabbed (send t get-text))
    (define tabbed
      (let ()
        (match-define (list table rx1 rx2 rx3 rx4)
          (preferences:get 'framework:tabify))
        (hash-remove! table 'big-bang)
        (preferences:set 'framework:tabify
                         (list table rx1 rx2 rx3 rx4))
        (send t tabify-all)
        (send t get-text)))
    (define lambda-tabbed
      (let ()
        (match-define (list table rx1 rx2 rx3 rx4)
          (preferences:get 'framework:tabify))
        (hash-set! table 'big-bang 'lambda)
        (preferences:set 'framework:tabify
                         (list table rx1 rx2 rx3 rx4))
        (send t tabify-all)
        (send t get-text)))
    (values untabbed tabbed lambda-tabbed)))

#| TODO, turn these examples into proper tests
(module+ test
  (tabify-file "big.rkt")
  (tabify-file "retab.rkt"))
|#
