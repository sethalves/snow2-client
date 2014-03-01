(define-library (seth http)
  (export call-with-request-body download-file)
  (import (scheme base))
  (import (snow snowlib))
  (cond-expand
   (chibi
    (import (chibi io) (chibi process) (scheme file) (chibi net http))
    (import (snow srfi-13-strings)))
   (chicken (import (chicken)
                    (ports) ;; for make-input-port
                    (extras) (posix)
                    (http-client)
                    (uri-common)
                    (intarweb)))
   (gauche (import (rfc uri) (rfc http) (seth port-extras)))
   (sagittarius
    (import (scheme write) (rfc uri) (srfi 1) (srfi 13) (srfi 14))
    (import (only (rnrs)
                  transcoded-port
                  make-transcoder
                  ;; utf-8-codec
                  latin-1-codec
                  eol-style)
            (only (sagittarius) format))
    (import (seth port-extras)
            (snow bytevector)
            (seth network-socket)))
   )
  (import (seth mime))
  (begin

    ;; (define (http:header-as-integer headers name default)
    ;;   (cond ((assq-ref headers name default) => string->number)
    ;;         (else #f)))


    ;; (define http:header-as-string assq-ref)


    ;; (define (http verb uri writer reader user-headers)
    ;;   ;;
    ;;   ;; verb should be a symbol like 'GET
    ;;   ;; uri should be a string
    ;;   ;; writer can be a string or input-port or (writer output-port) or #f
    ;;   ;; reader can be #f or a output-port or (reader input-port headers)
    ;;   ;; headers can be #f or an alist or (headers headers-alist)
    ;;   ;; --
    ;;   ;; if reader is #f or a port, return value is response body.
    ;;   ;; if reader is a procedure, return value is that of reader
    ;;   ;;
    ;;   ;; if writer is a port or procedure and content-length isn't
    ;;   ;; among the user-headers, chunked encoding will be used in
    ;;   ;; the request.
    ;;   ;;

    ;;   (let* ((sock (open-network-client `((host ,hostname) (port 80))))
    ;;          (write-port (socket:outbound-write-port sock))
    ;;          (read-port (bin->textual (socket:inbound-read-port sock))))

    ;;     (format write-port "GET ~A HTTP/1.1\r\n" path-part)
    ;;     (mime-write-headers `((host ,hostname)) write-port)
    ;;     (display "\r\n" write-port)

    ;;     (let* ((first-line (read-line read-port))
    ;;            (headers (mime-headers->list read-port))
    ;;            (content-length
    ;;             (http:header-as-integer headers 'content-length 0)))
    ;;       (let ((body (read-n content-length read-port)))
    ;;         (values 200 "" body)))))




    (cond-expand

     (chicken
      ;; this code is adapted from chicken's http-client.scm

      (define (make-delimited-input-port port len)
        (if (not len)
            port ;; no need to delimit anything
            (let ((pos 0))
              (make-input-port
               (lambda ()                     ; read-char
                 (if (= pos len)
                     (eof-object)
                     (let ((char (read-char port)))
                       (set! pos (add1 pos))
                       char)))
               (lambda ()                     ; char-ready?
                 (or (= pos len) (char-ready? port)))
               (lambda ()                     ; close
                 (close-input-port port))
               (lambda ()                     ; peek-char
                 (if (= pos len)
                     (eof-object)
                     (peek-char port)))
               (lambda (p bytes buf off)      ; read-string!
                 (let* ((bytes (min bytes (- len pos)))
                        (bytes-read (read-string! bytes buf port off)))
                   (set! pos (+ pos bytes-read))
                   bytes-read))
               (lambda (p limit)              ; read-line
                 (if (= pos len)
                     (eof-object)
                     (let* ((bytes-left (- len pos))
                            (limit (min (or limit bytes-left) bytes-left))
                            (line (read-line port limit)))
                       (unless (eof-object? line)
                               (set! pos (+ pos (string-length line))))
                       line)))))))

      (define discard-remaining-data!
        (let ((buf (make-string 1024))) ; Doesn't matter, discarded anyway
          (lambda (response port)
            ;; If header not available or no response object
            ;; passed, this reads until EOF
            (let loop ((len (and response
                                 (header-value
                                  'content-length
                                  (response-headers response)))))
              (if len
                  (when (> len 0)
                        (loop (- len (read-string! len buf port))))
                  (when (> (read-string! (string-length buf) buf port) 0)
                        (loop #f)))))))


      ;; (define (call-with-request-body url reader)
      ;;   (call-with-input-request url #f consumer))

      (define (call-with-request-body url reader)
        (let* ((uri (uri-reference url))
               (req (make-request uri: uri method: 'GET))
               ;; http://wiki.call-cc.org/eggref/4/intarweb#headers
               (headers (headers `((x-test "test header"))
                                 (request-headers req))))
          (update-request req headers: headers)
          (call-with-response
           req
           ;; send data to webserver
           (lambda x (void))
           ;; receive data from webserver
           (lambda (response)
             (let* ((rheaders (response-headers response))
                    (port (make-delimited-input-port
                           (response-port response)
                           (header-value 'content-length rheaders)))
                    (body?
                     ((response-has-message-body-for-request?) response req)))
               (if (= 200 (response-class response))
                   ;; 200 response
                   (let ((result (and body? reader (reader port))))
                     (when body? (discard-remaining-data! #f port))
                     (close-input-port port)
                     result)
                   ;; we got an http error response
                   (let ((err-body (read-string #f port)))
                     (close-input-port port)
                     (snow-raise
                      (make-snow-cond
                       (case (response-class response)
                         ((400) '(http-error http-client-error))
                         ((500) '(http-error http-server-error))
                         (else '(http-error)))
                       (vector
                        (response-code response)
                        (response-reason response)
                        (headers->list rheaders)
                        err-body))))))))))




      (define (download-file url write-port)
        (call-with-request-body
         url
         (lambda (inp)
           (let ((data (read-string #f inp)))
             (write-string data #f write-port)
             (close-output-port write-port)
             #t)))))

     (chibi
      ;; (define (call-with-request-body url consumer)
      ;;   (call-with-input-url url consumer))

      (define (call-with-request-body url consumer)
        (let* ((headers '())
               (p (http-get url headers))
               (res (consumer p)))
          (close-input-port p)
          res))

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
           (let ((data (read-all-u8 inp)))
             (write-bytevector data write-port)
             (close-output-port write-port)
             #t))))
      )

     (sagittarius
      ;; http://ktakashi.github.io/sagittarius-ref.html#rfc.uri

      (define (http:header-as-integer headers name default)
        (cond ((assq-ref headers name default) => string->number)
              (else #f)))

      (define http:header-as-string assq-ref)

      (define (bin->textual port)
        (transcoded-port port (make-transcoder
                               (latin-1-codec)
                               (eol-style none))))

      (define (http-get hostname path-part)
        (let* ((sock (open-network-client `((host ,hostname) (port 80))))
               (write-port (socket:outbound-write-port sock))
               (read-port (bin->textual (socket:inbound-read-port sock))))

          (display (format "GET ~A HTTP/1.1\r\n" path-part) write-port)
          (mime-write-headers `((host . ,hostname)) write-port)
          (display "\r\n" write-port)

          (let* ((first-line (read-line read-port))
                 (headers (mime-headers->list read-port))
                 (content-length
                  (http:header-as-integer headers 'content-length 0)))

            (let ((body (read-n content-length read-port)))
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

      ))


    ))
