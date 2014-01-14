
(define-library (seth tar)
  (export extract)
  (import (scheme base))
  (cond-expand
   (chibi (import (chibi io) (chibi process)))
   (chicken (import (chicken) (posix)))
   (gauche (import (gauche process)))
   (sagittarius (import (prefix (sagittarius process) process-))))
  (begin
    (cond-expand

     (chicken
      (define (extract filename)
        ;; XXX check filename for '
        (let* ((cmd (string-append "tar xf '" filename "'"))
               ;; (cmd (format #f "tar xf '~A'" filename))
               (tar-pid (process-run cmd)))
          (let-values (((pid normal-exit exit-status)
                        (process-wait tar-pid)))
            normal-exit))))

     (chibi
      (define (extract filename)
        (let ((fork-result (fork)))
          (cond ((= fork-result 0)
                 ;; child
                 (execute "tar" (list "tar" "xf" filename)))
                (else
                 (waitpid fork-result 0))))
        #t))

     (gauche
      (define (extract filename)
        (run-process (list "tar" "xf" filename) :wait #t)
        ))

     (sagittarius
      (define (extract filename)
        (process-run "tar" "xf" filename)
        ))

     )))
