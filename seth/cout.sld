;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (cout a "blah" 'foo b c (current-error-port))
;;
;;


(define-library (seth cout)
  (export cout)
  (import (scheme base)
          (scheme write)
          (srfi 1))
  (begin

    (define (cout . items-maybe-port)

      ;; (define (cout-hash hsh port)
      ;;   (display "[" port)
      ;;   (for-each
      ;;    (lambda (key)
      ;;      (cout "(" key ":" port)
      ;;      (cout (hash-table-ref hsh key) port)
      ;;      (display ")" port))
      ;;    (hash-table-keys hsh))
      ;;   (display "]" port))

      (define (cout-pair tree outp)
        (cout "(" outp)
        (let loop ((childs tree))
          (cond ((null? childs)
                 (cout ")" outp))
                (else
                 (cout (car childs) outp)
                 (cond ((pair? (cdr childs))
                        (cout " " outp)
                        (loop (cdr childs)))
                       ((null? (cdr childs))
                        (cout ")" outp))
                       (else
                        (cout " . " outp)
                        (cout (cdr childs) outp)
                        (cout ")" outp)))))))

      (let ((port (cond ((pair? items-maybe-port)
                         (let ((llast (last items-maybe-port)))
                           (if (output-port? llast)
                               llast
                               (current-output-port))))
                        (else (current-output-port)))))
        (for-each (lambda (x) (cond
                               ((output-port? x) #t)
                               ;; ((hash-table? x) (cout-hash x port))
                               ((pair? x) (cout-pair x port))
                               (else (display x port))))
                  items-maybe-port)))
    ))
