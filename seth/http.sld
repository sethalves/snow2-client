(define-library (seth http)
  (export call-with-request-body download-file)
  (cond-expand
   (chibi
    (import (scheme base) (chibi io) (chibi process))
    (import (scheme file))
    (import (chibi net http)))
   (chicken
    (import (scheme base) (chicken) (extras) (posix))
    (import (http-client))
    )
   (gauche
    (import (scheme base))
    (import (rfc uri) (rfc http))
    ))
  (begin
    (cond-expand

     (chicken
      (define (call-with-request-body url consumer)
        (with-input-from-request
         url #f (lambda () (consumer (current-input-port)))))

      (define (download-file url write-port)
        (call-with-request-body
         url
         (lambda (inp)
           (let ((data (read-string #f inp)))
             (write-string data #f write-port)
             (close-output-port write-port)
             #t)))))

     (chibi
      (define (call-with-request-body url consumer)
        (call-with-input-url url consumer))

      (define (download-file url write-port)
        (call-with-input-url
         url
         (lambda (inp)
           (let loop ()
             (let ((data (read-u8 inp)))
               (cond ((eof-object? data)
                      (close-output-port write-port)
                      #t)
                     (else
                      (write-u8 data write-port)
                      (loop)))))))))

     (gauche
      ;; http://practical-scheme.net/gauche/man/gauche-refe_149.html

      (define (call-with-request-body url consumer)

        (let-values (((scheme user-info hostname port-number
                              path-part query-part fragment-part)
                      (uri-parse url)))
          (let-values (((status-code headers body)
                        (http-get hostname path-part)))
            (consumer (open-input-string body))))))
     )))
