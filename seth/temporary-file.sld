(define-library (seth temporary-file)
  (export temporary-file)
  (import (scheme base)
          (scheme process-context))
  (import (srfi 27))
  (cond-expand
   (chibi
    (import (scheme file)
            (chibi io)
            (chibi process)))
   (chicken
    (import (posix)))
   (foment
    (import (scheme file)))
   (gauche
    (import (scheme file)
            (file util)))
   (sagittarius
    (import (scheme file)
            (util file))))
  (begin

    (random-source-randomize! default-random-source)

    (define (env-tmp)
      (let ((tmp (get-environment-variable "TMP")))
        (if (and tmp (not (equal? tmp "")))
            tmp
            "/tmp")))


    (cond-expand
     (chicken
      (define (temporary-file . maybe-binary)
        ;; hm http://wiki.call-cc.org/man/4/Unit%20files
        ;; has create-temporary-file
        (let-values (((fd temp-path)
                      (file-mkstemp
                       (string-append (env-tmp) "/tmp-"
                                      (number->string (current-process-id))
                                      ".XXXXXX"))))
          (values (open-output-file* fd) temp-path))))
     (chibi
      (define (temporary-file . maybe-binary)
        (let ((opener (if (and (pair? maybe-binary) (car maybe-binary))
                          open-binary-output-file
                          open-output-file)))
          (let ((temp-path
                 (string-append (env-tmp) "/tmp-"
                                (number->string (current-process-id))
                                "."
                                (number->string
                                 (+ 100000 (random-integer 899999))))))
            (values (opener temp-path) temp-path)))))

     (foment
      (define (temporary-file . maybe-binary)
        (let ((bin (and (pair? maybe-binary) (car maybe-binary))))
          (let ((temp-path
                 (string-append (env-tmp)
                                "/tmp"
                                "."
                                (number->string
                                 (+ 100000 (random-integer 899999))))))
            (values (if bin
                        (open-binary-output-file temp-path)
                        (open-output-file temp-path))
                    temp-path)))))

     (sagittarius
      (define (temporary-file . maybe-binary)
        (let ((bin (and (pair? maybe-binary) (car maybe-binary))))
          ;; (make-temporary-file (string-append (temporary-directory) "/"))
          (let ((temp-path
                 (string-append (temporary-directory)
                                "/tmp"
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
