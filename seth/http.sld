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
    (import (scheme base))))
  (begin
    (cond-expand

     (chicken
      (define (call-with-request-body url consumer)
        (with-input-from-request
         url #f (lambda () (consumer (current-input-port)))))

      (define (download-file url local-filename)
        (call-with-request-body
         url
         (lambda (inp)
           (let ((outp (open-output-file local-filename))
                 (data (read-string #f inp)))
             (write-string data #f outp)
             (close-output-port outp)
             #t)))))

     (chibi
      (define (call-with-request-body url consumer)
        (call-with-input-url url consumer))

      (define (download-file url local-filename)
        (call-with-input-url
         url
         (lambda (inp)
           (let ((outp (open-output-file local-filename)))
             (let loop ()
               (let ((data (read-u8 inp)))
                 (cond ((eof-object? data)
                        (close-output-port outp)
                        #t)
                       (else
                        (write-u8 data outp)
                        (loop))))))))))

     (gauche
      (define (call-with-request-body url consumer)
        #f)))

    ))
