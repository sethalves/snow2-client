(define-library (seth temporary-file)
  (export temporary-file)
  (import (scheme base))
  (cond-expand
   (chibi
    (import (scheme file)
            (chibi io) (chibi process)
            (srfi 27)))
   (chicken
    (import (posix)))
   (gauche (import (scheme file) (file util) (srfi 27)))
   (sagittarius (import (scheme file)
                        (util file)
                        (only (rnrs)
                              transcoded-port
                              make-transcoder
                              latin-1-codec
                              eol-style)
                        (srfi 27)
                        ))
   )
  (begin
    (cond-expand
     (chicken
      (define (temporary-file . maybe-binary)
        ;; hm http://wiki.call-cc.org/man/4/Unit%20files
        ;; has create-temporary-file
        (let-values (((fd temp-path)
                      (file-mkstemp
                       (string-append "/tmp/tmp-"
                                      (number->string (current-process-id))
                                      ".XXXXXX"))))
          (values (open-output-file* fd) temp-path))))
     (chibi
      (define (temporary-file . maybe-binary)
        (let ((opener (if (and (pair? maybe-binary) (car maybe-binary))
                          open-binary-output-file
                          open-output-file)))
          (let ((temp-path
                 (string-append "/tmp/tmp-"
                                (number->string (current-process-id))
                                "."
                                (number->string
                                 (+ 100000 (random-integer 899999))))))
            (values (opener temp-path) temp-path)))))

     (sagittarius
      (define (temporary-file . maybe-binary)
        (let ((bin (and (pair? maybe-binary) (car maybe-binary))))
          ;; (make-temporary-file (string-append (temporary-directory) "/"))
          (let ((temp-path
                 (string-append (temporary-directory)
                                "/tmp-"
                                "sagittarius"
                                "."
                                (number->string
                                 (+ 100000 (random-integer 899999))))))
            (values (if bin
                        (open-binary-output-file temp-path)
                        (open-output-file temp-path))
                        ;; (opener temp-path
                        ;;     ;; :transcoder #f
                        ;;     :transcoder (make-transcoder
                        ;;                  (latin-1-codec)
                        ;;                  (eol-style none)))
                    temp-path))))
      )

     (gauche
      (define (temporary-file . maybe-binary)
        (let ((opener (if (and (pair? maybe-binary) (car maybe-binary))
                          open-binary-output-file
                          open-output-file)))
          (let ((temp-path
                 (string-append (temporary-directory)
                                "/tmp-"
                                ;; (number->string (sys-getpid))
                                "gauche"
                                "."
                                (number->string
                                 (+ 100000 (random-integer 899999))))))
            (values (opener temp-path) temp-path)))))

     )))
