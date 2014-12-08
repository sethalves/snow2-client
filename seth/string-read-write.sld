;; -*- scheme -*-

(define-library (seth string-read-write)
  (export read-from-string
          write-to-string
          display-to-string
          ->string
          with-output-to-string
          call-with-output-string
          call-with-input-string
          )
  (import (scheme base) (scheme read) (scheme write))

  (cond-expand
   (chibi
    (import (only (chibi) call-with-output-string)
            (only (chibi show base) write-to-string)
            ))
   (chicken
    (import (ports)))
   (foment)
   (gauche)
   (sagittarius
    (import (sagittarius io))))

  (begin

    (define (read-from-string s)
      (read (open-input-string s)))

    (cond-expand
     (chibi)
     (else
      (define (write-to-string obj)
        (let ((s (open-output-string)))
          (write obj s)
          (let ((result (get-output-string s)))
            (close-output-port s)
            result)))))

    (define (display-to-string obj)
      (let ((s (open-output-string)))
        (display obj s)
        (let ((result (get-output-string s)))
          (close-output-port s)
          result)))

    (define ->string display-to-string)

    (cond-expand
     ((or chibi foment kawa)
      (define (call-with-output-string func)
        (let ((out-port (open-output-string)))
          (func out-port)
          (get-output-string out-port)))

      (define (call-with-input-string str proc)
        (proc (open-input-string str))))
     (else))

    (cond-expand
     (chicken)
     ((or foment gauche)
      (define (with-output-to-string thunk)
        (let ((s (open-output-string)))
          (let ((save-output-port (current-output-port)))
            (current-output-port s)
            (thunk)
            (current-output-port save-output-port)
            (let ((result (get-output-string s)))
              (close-output-port s)
              result)))))
     (sagittarius)
     (else
      (define (with-output-to-string thunk)
        (call-with-output-string
         (lambda (out)
           (let ((old-out (current-output-port)))
             (current-output-port out)
             (thunk)
             (current-output-port old-out)))))))

    ))
