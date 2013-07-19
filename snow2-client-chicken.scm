#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

;; #!/usr/local/bin/csi -script

(import chicken scheme)
(use http-client)
(use srfi-69)
(use inclub)

(inclub "snow2-client-common.scm")


(define (report-error format-string . args)
  (apply format (cons (current-error-port) (cons format-string args)))
  #f)


(define (program-and-command-line)
  ;; argv holds the script name, but if this is run as an interpreter,
  ;; it starts with /usr/local/bin/csi -script ...
  ;; (command-line-arguments) doesn't include the script name.

  ;; search through (argv) until we find (command-line-arguments),
  ;; and assume that the string in (argv) before the start of
  ;; (command-line-arguments) must be the program name.
  (let loop ((cl (argv))
             (program-name (car (argv))))
    (cond ((null? cl)
           (cons program-name (command-line-arguments)))
          ((equal? cl (command-line-arguments))
           (cons program-name (command-line-arguments)))
          (else
           (loop (cdr cl) (car cl))))))





(define (untar filename)
  (let ((tar-pid (process-run (format #f "tar xf '~A'" filename))))
    (process-wait tar-pid)))


(main-program)
