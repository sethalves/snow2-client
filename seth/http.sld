(define-library (seth http)
  (export http
          log-http-to-stderr
          response-status-class
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
                    (only (ports) make-input-port)
                    (extras) (posix)
                    (only (http-client) call-with-input-request)
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
            (srfi 14)
            (only (rnrs) make-custom-binary-input-port))))
  (import (snow bytevector)
          (snow binio)
          (srfi 29)
          (srfi 13)
          (snow extio)
          (seth mime)
          (seth string-read-write)
          (seth uri)
          (seth port-extras)
          (seth network-socket)
          )
  (begin

    ;; http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html

    (define log-http-to-stderr (make-parameter #f))


    (define (response-status-class code)
      ;; inspired by chicken's intarweb's response-class
      (- code (modulo code 100)))


    (define (http-header-as-integer headers name default)
      (cond ((assq-ref headers name default)
             => (lambda (v)
                  (cond ((number? v) v)
                        ((string? v) (string->number v))
                        (else (error "http-header-as-integer error" v)))))
            (else default)))


    (define (http-header-as-string headers name default)
      (cond ((assq-ref headers name default) => ->string)
            (else default)))


    (define (make-dechunked-input-port port)
      (let ((chunk-length 0)
            (saw-eof #f))

        (define (update-chunk-length)
          (cond ((> chunk-length 0) #t)
                (saw-eof #t)
                (else
                 (let ((chunk-length-str (read-latin-1-line port)))
                   ;; (cond ((log-http-to-stderr)
                   ;;        (display chunk-length-str (current-error-port))))
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
                 (let ((c (read-char port)))
                   (set! chunk-length (- chunk-length 1))
                   (cond ((= 0 chunk-length)
                          (read-latin-1-line port)))
                   (update-chunk-length)
                   c))))


        (define (chunked-read-u8)
          (update-chunk-length)
          (cond (saw-eof (eof-object))
                (else
                 (let ((c (read-u8 port)))
                   (set! chunk-length (- chunk-length 1))
                   (cond ((= 0 chunk-length)
                          (read-latin-1-line port)))
                   (update-chunk-length)
                   c))))

        ;; (define (chunked-read-u8)
        ;;   (let ((c (chunked-read-char)))
        ;;     (cond ((eof-object? c) c)
        ;;           (else (char->integer c)))))

        (define (chunked-char-ready?)
          (update-chunk-length)
          (not saw-eof))

        (define (chunked-peek-char)
          (update-chunk-length)
          (cond (saw-eof (eof-object))
                (else
                 ;; (set! chunk-length (- chunk-length 1))
                 (peek-latin-1-char port))))

        (cond-expand

         (chibi
          (make-custom-binary-input-port
           (lambda (str start end)
             (cond (saw-eof 0)
                   (else
                    (let ((c (chunked-read-u8)))
                      (cond ((eof-object? c) c)
                            (else
                             ;; (string-set! str start c)
                             (bytevector-u8-set! str start c)
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

         (sagittarius
          (make-custom-binary-input-port
           "http chunked port"
           (lambda (str start count)
             (cond (saw-eof 0)
                   (else
                    (let ((c (chunked-read-u8)))
                      (cond ((eof-object? c) 0)
                            (else
                             (bytevector-u8-set! str start c)
                             1))))))
           #f ;; get-position
           #f ;; set-position!
           #f ;; close
           #f ;; ready
           ))

         (else
          (let loop ((chars '()))
            (let ((c (chunked-read-u8)))
              (cond ((eof-object? c)
                     (open-input-bytevector (u8-list->bytevector
                                             (reverse chars))))
                    (else
                     (loop (cons c chars))))))
          ))))


    (define (read-status-line read-port)
      ;; "HTTP/1.1 200 OK"
      (let* ((first-line (read-latin-1-line read-port))
             (parts (string-tokenize first-line)))
        (cond ((log-http-to-stderr)
               (display first-line (current-error-port))
               (newline (current-error-port))))
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
          (cond

           ;; writer is #f
           ((not writer) (values #f 0))

           ;; writer is a string
           ((string? writer)
            (let ((writer (string->utf8 writer)))
              (if (and user-content-length
                       (not (= user-content-length
                               (bytevector-length writer))))
                  (error "http -- writer string length mismatch"))
              (values (open-input-bytevector writer)
                      (bytevector-length writer))))

           ((bytevector? writer)
            (if (and user-content-length
                     (not (= user-content-length
                             (bytevector-length writer))))
                (error "http -- writer bytevector length mismatch"))
            (values (open-input-bytevector writer)
                    (bytevector-length writer)))

           ;; writer is a binary port
           ((and (input-port? writer) (binary-port? writer))
            (values writer user-content-length))

           ;; writer is a textual port
           ((and (input-port? writer))
            (values (textual-port->binary-port writer)
                    user-content-length))

           ;; something unexpected
           (else
            (error "http -- invalid writer")))))


      (define (send-body src-port dst-port content-length)
        ;; XXX content-encoding?
        (cond ((not content-length)
               ;; XXX chunked encoding
               (error "http -- request chunked encoding, write me!"))
              (else
               (let loop ((sent 0))
                 (cond ((= sent content-length) #t)
                       (else
                        (let* ((n-to-read (- content-length sent))
                               (n-to-read (if (> n-to-read 1024)
                                              1024
                                              n-to-read))
                               (data (read-bytevector n-to-read src-port)))
                          (if (eof-object? data)
                              (error
                               "http -- not enough request body data"))
                          (write-bytevector data dst-port)
                          (loop (+ sent n-to-read)))))))))


      ;; figure out usable versions of all the arguments.  if it's a
      ;; string, parse the url.  supply defaults for unspecified
      ;; optional arguments.
      (let* ((verb-str (string-upcase (->string verb)))
             (uri (cond ((uri-reference? uri) uri)
                        ((string? uri) (uri-reference uri))
                        (else
                         (error "http -- invalid uri" uri))))
             ;; set up network connection
             (hostname (uri-host uri))
             (port (or (uri-port uri)
                       (cond ((eq? (uri-scheme uri) 'http) 80)
                             ((eq? (uri-scheme uri) 'https) 443)
                             (else (error "http -- don't know port")))))
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

            (cond ((log-http-to-stderr)
                   (display
                    (format "~a ~a HTTP/1.1\r\n" verb-str path-part)
                    (current-error-port))
                   (mime-write-headers final-headers (current-error-port))
                   (display "\r\n" (current-error-port))))

            (guard
             (exn (#t (error "http error during request" exn)))
             ;; send request and headers
             (display
              ;; try to avoid sending 3 separate header packets
              (with-output-to-string
                (lambda ()
                  (display (format "~a ~a HTTP/1.1\r\n" verb-str path-part))
                  (mime-write-headers final-headers (current-output-port))
                  (display "\r\n")))
              write-port))

            (guard
             (exn (#t (error "http error during send body" exn)))
             ;; send body
             (send-body writer-port write-port content-length)
             (snow-force-output write-port))))

        ;;
        ;; read response
        ;;

        (guard
         (exn (#t (error "http error during read response" exn)))
         (let* ((status-code (read-status-line read-port))
                (headers (mime-headers->list read-port))
                (content-length
                 (http-header-as-integer headers 'content-length #f))
                (transfer-encoding
                 (http-header-as-string headers 'transfer-encoding #f))
                (body-port
                 (cond ((or (equal? verb-str "HEAD")
                            (= status-code 204)) ;; No Content
                        (open-input-bytevector (make-bytevector 0)))
                       (content-length
                        (make-delimited-input-port read-port content-length))
                       ((equal? transfer-encoding "chunked")
                        (make-dechunked-input-port read-port))
                       (else
                        (error "http -- no content length or chunked")))))

           (cond ((log-http-to-stderr)
                  (mime-write-headers headers (current-error-port))))

           (cond ((not reader)
                  (let* ((response-body
                          (if content-length
                              (read-bytevector content-length body-port)
                              (read-all-u8 body-port)))
                         (expect-eof (read-u8 body-port)))
                    (cond ((eof-object? response-body)
                           (error "http -- empty body"))
                          ((not (eof-object? expect-eof))
                           (error "http -- extra data in response body"))
                          ((and content-length
                                (< (bytevector-length response-body)
                                   content-length))
                           (error "http -- response body too short"))
                          (else
                           (values status-code headers response-body)))))
                 ((procedure? reader)
                  (guard
                   (exn (#t (error "http error calling reader" exn)))
                   (reader status-code headers body-port)))
                 ((port? reader)
                  (error "http -- write this!"))
                 (else
                  (error "http -- unexpected reader type")))))))


    ;;
    ;; some wrappers around http clients that come with
    ;; the various schemes.
    ;;


    (cond-expand

     (chibi
      (define (call-with-request-body url consumer)
        (call-with-input-url url consumer)))

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
              (let ((buffer (make-bytevector 1024)))
                (let loop ()
                  (let ((got (read-bytevector! buffer response-body-port)))
                    (cond ((eof-object? got)
                           (close-output-port write-port)
                           #t)
                          (else
                           (write-bytevector buffer write-port 0 got)
                           (loop)))))))))

    ))
