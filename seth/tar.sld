
(define-library (seth tar)
  (export extract)
  (cond-expand
   (chibi
    (import (scheme base) (chibi io) (chibi process)))
   (chicken
    (import (scheme base) (chicken) (posix))
    )
   (gauche
    (import (scheme base))))
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
     )))
