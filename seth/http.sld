(define-library (seth http)
  (export call-with-request-body download-file)
  (import (scheme base))
  (cond-expand
   (chibi
    (import (chibi io) (chibi process) (scheme file) (chibi net http))
    (import (seth srfi-13-strings)))
   (chicken (import (chicken) (extras) (posix) (http-client)))
   (gauche (import (rfc uri) (rfc http) (seth port-extras)))
   (sagittarius
    (import (scheme write) (rfc uri) (srfi 1) (srfi 13) (srfi 14))
    (import (seth port-extras) (seth bytevector) (seth network-socket)))
   )
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
            (consumer (open-input-string body)))))


      (define (download-file url write-port)
        (call-with-request-body
         url
         (lambda (inp)
           (let ((data (read-all-chars inp)))
             (write-string data write-port)
             (close-output-port write-port)
             #t))))
      )

     (sagittarius
      ;; http://ktakashi.github.io/sagittarius-ref.html#rfc.uri

      (define (http:header-as-integer headers name default)
        (let* ((header (assq name headers)))
          (if header
              (let ((value (if (pair? (cdr header))
                               (cadr header)
                               (cdr header))))
                (string->number value))
              default)))


      (define (http:header-as-string headers name default)
        (let* ((header (assq name headers)))
          (if header
              (if (pair? (cdr header))
                  (cadr header)
                  (cdr header))
              default)))


      (define (http:join-continued-lines lines)
        (let loop ((joined (list))
                   (unjoined lines))
          (cond ((null? unjoined) (reverse joined))
                ((null? joined) (loop (list (car unjoined))
                                      (cdr unjoined)))
                ((or (eqv? (string-ref (car unjoined) 0) #\space)
                     (eqv? (string-ref (car unjoined) 0) #\tab))
                 (let ((continued (string-append
                                   (string-trim-both (car joined))
                                   (string-trim-both (car unjoined)))))
                   (loop (cons continued (cdr joined))
                         (cdr unjoined))))
                (else
                 (loop (cons (car unjoined) joined)
                       (cdr unjoined))))))


      (define (http:string->header header-string)
        (let ((colon-position (string-contains header-string (string #\:))))
          (if (eq? colon-position #f) #f
              (let ((name (substring header-string 0 colon-position))
                    (value (substring header-string (+ colon-position 1)
                                      (string-length header-string))))
                (list (string->symbol (string-downcase (string-trim-both name)))
                      (string-trim-both value))))))


      (define (http:string->headers headers-string)
        (let ((lines (filter (lambda (x) (> (string-length x) 0))
                             (string-tokenize
                              headers-string
                              (char-set-complement
                               (char-set #\newline #\return))))))
          (map http:string->header (http:join-continued-lines lines))))


      (define (read-headers read-port)
        (let loop ((prev-0 #f)
                   (prev-1 #f)
                   (prev-2 #f)
                   (character (read-u8 read-port))
                   (headers (list)))
          (cond ((eof-object? character) #f)
                ((and (eqv? character 10)
                      (eqv? prev-0 13)
                      (eqv? prev-1 10)
                      (eqv? prev-2 13))
                 (list->string
                  (reverse (cons (integer->char character) headers))))
                (else
                 (loop character prev-0 prev-1 (read-u8 read-port)
                       (cons (integer->char character) headers))))))


      (define (http-get hostname path-part)
        (let* ((sock (open-network-client `((host ,hostname) (port 80))))
               (write-port (socket:outbound-write-port sock))
               (read-port (socket:inbound-read-port sock)))

          (display "GET " write-port)
          (display path-part write-port)
          (display " HTTP/1.1\r\n" write-port)
          (display "Host: " write-port)
          (display hostname write-port)
          (display "\r\n\r\n" write-port)

          (let* ((headers-string (read-headers read-port))
                 (headers (http:string->headers headers-string))
                 (content-length
                  (http:header-as-integer headers 'content-length 0)))
            (let ((body (latin-1->string (read-n-u8 content-length read-port))))
              (values 200 "" body)))))


      (define (call-with-request-body url consumer)
        (let-values (((scheme user-info hostname port-number
                              path-part query-part fragment-part)
                      (uri-parse url)))
          (let-values (((status-code headers body)
                        (http-get hostname path-part)))
            (consumer (open-input-string body)))))


      (define (download-file url write-port)
        (call-with-request-body
         url
         (lambda (inp)
           (let* ((data-s (read-all-chars inp))
                  (data-bv (string->latin-1 data-s))
                  )

             ;; (write-bytevector data-bv write-port)
             (let loop ((i 0))
               (cond ((= i (bytevector-length data-bv)) #t)
                     (else
                      (let ((b (bytevector-u8-ref data-bv i)))
                        (write-u8 b write-port)
                        (loop (+ i 1))))))

             (close-output-port write-port)
             #t))))

      ))))
