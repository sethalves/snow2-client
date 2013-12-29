(define-library (seth temporary-file)
  (export temporary-file)
  (cond-expand
   (chibi
    (import (scheme base) (scheme file)
            (chibi io) (chibi process)
            (seth srfi-27-random)))
   (chicken
    (import (scheme base) (chicken) (posix)))
   (gauche
    (import (scheme base))))
  (begin
    (cond-expand
     (chicken
      (define (temporary-file)
        ;; hm http://wiki.call-cc.org/man/4/Unit%20files
        ;; has create-temporary-file
        (let-values (((fd temp-path)
                      (file-mkstemp
                       (string-append "/tmp/tmp-"
                                      (number->string (current-process-id))
                                      ".XXXXXX"))))
          (values
           (open-output-file* fd)
           temp-path))))
     (chibi
      (define (temporary-file)
        (let ((temp-path
               (string-append "/tmp/tmp-"
                              (number->string (current-process-id))
                              "."
                              (number->string
                               (+ 100000 (random-integer 899999))))))
          (values (open-output-file temp-path)
                  temp-path))))
     (gauche
      (define (temporary-file)
        (let ((template
               (string-append
                (sys-tmpdir) "tmp-" (current-process-id) ".XXXXXX")))
          (file-mkstemp template))))
     )))
