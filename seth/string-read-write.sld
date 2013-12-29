;; -*- scheme -*-

(define-library (seth string-read-write)
  (export read-from-string write-to-string)
  (cond-expand
   (chibi (import (scheme base) (scheme read) (scheme write)))
   (chicken (import (chicken)))
   (gauche (import (scheme base) (scheme read) (scheme write))))
  (begin

    (define (read-from-string s)
      (read (open-input-string s)))

    (define (write-to-string obj)
      (let ((s (open-output-string)))
        (write obj s)
        (let ((result (get-output-string s)))
          (close-output-port s)
          result)))

    ;; (cond-expand (chicken (register-feature! 'seth.string-read-write)))

    ))
