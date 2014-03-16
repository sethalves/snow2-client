(define-library (seth http)
  (export http
          call-with-request-body
          download-file
          http-header-as-integer
          http-header-as-string)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme char))
  (cond-expand
   (chibi
    (import (chibi io)
            (chibi process)
            (scheme file)
            (chibi net http)))
   (chicken (import (chicken)
                    (ports) ;; for make-input-port
                    (extras) (posix)
                    (http-client)
                    (uri-generic)
                    (intarweb)))
   (gauche (import (rfc uri)
                   (rfc http)
                   (snow gauche-extio-utils)
                   ))
   (sagittarius
    (import (scheme write)
            ;; (rfc uri)
            (match)
            (srfi 1)
            (srfi 13)
            (srfi 14)))
   )
  (import (snow snowlib)
          (snow bytevector)
          (snow binio)
          (snow srfi-29-format)
          (snow srfi-13-strings)
          (snow extio)
          (seth mime)
          (seth string-read-write)
          (seth uri)
          (seth port-extras)
          (seth network-socket)
          )
  (begin



    (define (http-header-as-integer headers name default)
      (cond ((assq-ref headers name default)
             => (lambda (v)
                  (cond ((number? v) v)
                        ((string? v) (string->number v))
                        (else (snow-error "http-header-as-integer error")))))
            (else #f)))


    (define (http-header-as-string headers name default)
      (cond ((assq-ref headers name default) => ->string)
            (else #f)))


    (define (uri->path-string uri)
      (uri->string
       (update-uri uri 'scheme #f 'authority #f 'fragment #f)))


    (define (make-dechunked-input-port port)
      (let ((chunk-length 0)
            (saw-eof #f))

        (define (update-chunk-length)
          (cond ((> chunk-length 0) #t)
                (saw-eof #t)
                (else
                 (let ((chunk-length-str (read-latin-1-line port)))
                   (cond ((eof-object? chunk-length-str)
                          (set! chunk-length 0)
                          (set! saw-eof #t))
                         (else
                          (set! chunk-length
                                (string->number chunk-length-str 16))
                          (if (= chunk-length 0)
                              (set! saw-eof #t))))))))

        (define (chunked-read-char)
          (update-chunk-length)
          (cond (saw-eof (eof-object))
                (else
                 (set! chunk-length (- chunk-length 1))
                 (let ((c (read-latin-1-char port)))
                   (if (= 0 chunk-length)
                       (read-latin-1-line port))
                   (update-chunk-length)
                   c))))

        (define (chunked-read-u8)
          (let ((c (chunked-read-char)))
            (cond ((eof-object? c) c)
                  (else (char->integer c)))))

        (define (chunked-char-ready?)
          (update-chunk-length)
          (not saw-eof))

        (define (chunked-peek-char)
          (update-chunk-length)
          (cond (saw-eof (eof-object))
                (else
                 (set! chunk-length (- chunk-length 1))
                 (peek-latin-1-char port))))

        (cond-expand

         (chibi
          (make-custom-input-port
           (lambda (str start end)
             (cond (saw-eof (eof-object))
                   (else
                    (let ((c (chunked-read-char)))
                      (cond ((eof-object? c) c)
                            (else
                             (string-set! str start c)
                             1))))))))

         (chicken
          (make-input-port
           chunked-read-char ; read-char
           chunked-char-ready? ; char-ready?
           (lambda () #t) ; close
           chunked-peek-char)) ; peek-char

         (gauche
          (make-virutal-input-port
           :getb chunked-read-u8
           :getc chunked-read-char
           :ready chunked-char-ready?
           :close (lambda () #t)))

         (else
          (let loop ((chars '()))
            (let ((c (chunked-read-char)))
              (cond ((eof-object? c)
                     (open-input-string (list->string (reverse chars))))
                    (else
                     (loop (cons c chars))))))
          ))))


    (define (read-status-line read-port)
      ;; "HTTP/1.1 200 OK"
      (let* ((first-line (read-latin-1-line read-port))
             (parts (string-tokenize first-line)))
        (cond ((< (length parts) 3) #f)
              ((not (string-prefix-ci? "http/" (car parts))) #f)
              (else (string->number (cadr parts))))))


    (define (http verb uri writer reader . maybe-user-headers+finalizer)
      ;;
      ;; verb should be a symbol like 'GET
      ;; uri should be a string or a uri record
      ;; writer can be a string or input-port or #f
      ;; reader can be #f or a output-port or (reader input-port headers)
      ;; headers can be #f or an alist or (headers headers-alist)
      ;; --
      ;; if reader is #f return value is
      ;;     (values status-code headers response-body)
      ;; if reader is a procedure, it will be called like this:
      ;;    (reader status-code response-headers response-body-port)
      ;;    and return value is that of reader.
      ;;
      ;; if writer is a port or procedure and content-length isn't
      ;; among the user-headers, chunked encoding will be used in
      ;; the request.
      ;;
      ;; if optional headers are passed, they should be an alist
      ;; with lowercase symbols for keys.
      ;;
      ;; the optional header-finalizer procedure should accept an
      ;; alist of headers (which shouldn't be modified) and return
      ;; an alist of headers.
      ;;
      (define (get-outbound-port-and-length headers)
        (let ((user-content-length
               (http-header-as-integer headers 'content-length #f)))
          ;; XXX should check content-encoding here, in case
          ;; the writer has multi-byte characters
          (cond

           ;; writer is #f
           ((not writer) (values #f 0))

           ;; writer is a string
           ((string? writer)
            (if (and user-content-length
                     (not (= user-content-length
                             (string-length writer))))
                (snow-error "http -- writer string length mismatch"))
            (values (open-input-string writer)
                    (string-length writer)))

           ;; writer is a port
           ((input-port? writer)
            (values writer user-content-length))

           ;; something unexpected
           (else
            (snow-error "http -- invalid writer")))))


      (define (send-body src-port dst-port content-length)
        ;; XXX content-encoding?
        (cond ((not content-length)
               ;; XXX chunked encoding
               (snow-error "http -- request chunked encoding, write me!"))
              (else
               (let loop ((sent 0))
                 (cond ((= sent content-length) #t)
                       (else
                        (let* ((n-to-read (- content-length sent))
                               (n-to-read (if (> n-to-read 1024)
                                              1024
                                              n-to-read))
                               (data (read-latin-1-string n-to-read src-port)))
                          (if (eof-object? data)
                              (snow-error
                               "http -- not enough request body data"))
                          (write-string data dst-port)
                          (loop (+ sent n-to-read)))))))))


      ;; figure out usable versions of all the arguments.  if it's a
      ;; string, parse the url.  supply defaults for unspecified
      ;; optional arguments.
      (let* ((verb-str (string-upcase (->string verb)))
             (uri (cond ((uri-reference? uri) uri)
                        ((string? uri) (uri-reference uri))
                        (else
                         (snow-error "http -- invalid uri" uri))))
             ;; set up network connection
             (hostname (uri-host uri))
             (port (or (uri-port uri)
                       (cond ((eq? (uri-scheme uri) 'http) 80)
                             ((eq? (uri-scheme uri) 'https) 443)
                             (else (snow-error "http -- don't know port")))))
             (sock (open-network-client `((host ,hostname) (port ,port))))
             (write-port (socket:outbound-write-port sock))
             (read-port (socket:inbound-read-port sock))
             ;; path and headers
             (path-part (uri->path-string uri))
             (user-headers (if (pair? maybe-user-headers+finalizer)
                               (car maybe-user-headers+finalizer)
                               '()))
             (maybe-user-headers+finalizer
              (if (pair? maybe-user-headers+finalizer)
                  (cdr maybe-user-headers+finalizer) '()))
             (header-finalizer (if (pair? maybe-user-headers+finalizer)
                                   (car maybe-user-headers+finalizer)
                                   (lambda (x) x))))
        ;;
        ;; make request
        ;;
        (let-values (((writer-port content-length)
                      (get-outbound-port-and-length user-headers)))
          ;; finish putting together request headers
          (let* ((host-headers (assq-set user-headers 'host hostname))
                 (host-cl-headers
                  (if content-length
                      (assq-set host-headers 'content-length content-length)
                      host-headers))
                 (final-headers (header-finalizer host-cl-headers)))

            (display
             (with-output-to-string
               (lambda ()
                 ;; send request and headers
                 (display (format "~a ~a HTTP/1.1\r\n" verb-str path-part))
                 (mime-write-headers final-headers (current-output-port))
                 (display "\r\n")))
             write-port)
            ;; send body
            (send-body writer-port write-port content-length)
            (snow-force-output write-port)))

        ;;
        ;; read response
        ;;
        (let* ((status-code (read-status-line read-port))
               (headers (mime-headers->list read-port))
               (content-length
                (http-header-as-integer headers 'content-length #f))
               (transfer-encoding
                (http-header-as-string headers 'transfer-encoding #f))
               (body-port
                (cond (content-length
                       (make-delimited-input-port read-port content-length))
                      ((equal? transfer-encoding "chunked")
                       (make-dechunked-input-port read-port))
                      (else
                       (snow-raise "http -- no content length or chunked")))))
          (cond ((not reader)
                 (let* ((response-body
                         (if content-length
                             (read-latin-1-string content-length body-port)
                             (read-all-latin-1-chars body-port)))
                        (expect-eof (read-latin-1-char body-port)))
                   (cond ((not (eof-object? expect-eof))
                          (snow-raise "http -- extra data in response body"))
                         ((and content-length
                               (< (string-length response-body) content-length))
                          (snow-raise "http -- response body too short"))
                         (else
                          (values status-code headers response-body)))))
                ((procedure? reader)
                 (reader status-code headers body-port))
                ((port? reader)
                 (snow-error "http -- write this!"))
                (else
                 (snow-error "http -- unexpected reader type"))))))


    ;;
    ;; some wrappers around http clients that come with
    ;; the various schemes.
    ;;


    (cond-expand

     ;; (chibi
     ;;  (define (call-with-request-body url consumer)
     ;;    (call-with-input-url url consumer)))

     (chicken
      (define (call-with-request-body url reader)
        (call-with-input-request url #f reader)))

     (gauche
      ;; http://practical-scheme.net/gauche/man/gauche-refe_149.html
      (define (call-with-request-body url consumer)

        (let-values (((scheme user-info hostname port-number
                              path-part query-part fragment-part)
                      (uri-parse url)))
          (let-values (((status-code headers body)
                        (http-get
                         (if port-number
                             (string-append hostname ":"
                                            (number->string port-number))
                             hostname)
                         path-part)))
            (consumer (open-input-string body))))))

     (else
      (define (call-with-request-body url consumer)
        (http 'GET url #f
              (lambda (status-code headers response-body-port)
                (consumer response-body-port))))))


    (define (download-file url write-port)
      (http 'GET
            url #f
            (lambda (status-code headers response-body-port)
              (let loop ()
                (let ((c (read-latin-1-char response-body-port)))
                  (cond ((eof-object? c)
                         (close-output-port write-port)
                         #t)
                        ((> (char->integer c) 255)
                         (display "OOPS\n")
                         (snow-error "download-file OOPS"))
                        (else
                         (write-latin-1-char c write-port)
                         (loop))))))))

    ))
