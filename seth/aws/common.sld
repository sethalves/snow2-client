(define-library (seth aws common)
  (export read-credentials
          make-credentials
          credentials?
          credentials-access-key-id
          credentials-secret-access-key
          make-path
          perform-aws-request
          uri->bucket
          get-credentials-for-s3-bucket
          )

  (import (scheme base)
          (scheme char)
          (scheme write)
          (scheme file)
          (srfi 1)
          (snow bytevector)
          (snow srfi-13-strings)
          (snow srfi-95-sort)
          (snow srfi-29-format)
          (snow srfi-19-time)
          (snow extio)
          ;; (seth xml ssax)
          ;; (seth xml sxpath)
          (seth http)
          (seth crypt hmac-sha-1)
          ;; (seth port-extras)
          (seth uri)
          (prefix (seth base64) base64-)
          )

  (cond-expand
   (chibi (import (chibi char-set)))
   (chicken (import (srfi 14)))
   (gauche (import (srfi 14)))
   (sagittarius (import (srfi 14)))
   (else))


  (begin

    ;; http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html


    (define-record-type <credentials>
      (make-credentials access-key-id secret-access-key)
      credentials?
      (access-key-id credentials-access-key-id)
      (secret-access-key credentials-secret-access-key))

    (define (read-credentials filename)
      ;; XXX make a better parser
      (let* ((creds-hndl (open-input-file filename))
             (creds-line0 (read-line creds-hndl))
             (creds-line1 (read-line creds-hndl)))
        (close-input-port creds-hndl)
        (make-credentials
         (substring creds-line0 15 (string-length creds-line0))
         (substring creds-line1 13 (string-length creds-line1)))))

    (define (get-credentials-for-s3-bucket bucket)
      ;; XXX look at environment variables
      (read-credentials (string-append "/etc/aws/s3-" bucket)))


    (define (make-path key)
      (cons '/
            (cond ((not key) '())
                  ((string? key)
                   (string-tokenize key char-set:uri-unreserved))
                  ((and (pair? key) (eq? (car key) '/)) (cdr key))
                  (else key))))


    (define (sig-date date) (date->string date "~a, ~d ~b ~Y ~T GMT"))


    ;; http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html

    ;; GET /photos/puppy.jpg HTTP/1.1
    ;; Host: johnsmith.s3.amazonaws.com
    ;; Date: Mon, 26 Mar 2007 19:37:58 +0000
    ;; Authorization: AWS AKIAIOSFODNN7EXAMPLE:frJIUN8DYpKDtOLCwo//yllqDzg=


    (define (make-aws-authorization credentials
                                    verb
                                    resource
                                    date
                                    amz-headers
                                    content-md5
                                    content-type)
      (let* ((can-amz-headers
              (sort (map (lambda (header)
                           `(,(string-downcase (car header)) . ,(cdr header)))
                         amz-headers)
                    (lambda (v1 v2)
                      (string<? (car v1) (car v2)))))
             (can-string
              (string-append
               (string-upcase verb) "\n"
               (if content-md5 content-md5 "") "\n"
               (if content-type content-type "") "\n"
               (if date date "") "\n"
               (fold (lambda (e o)
                       (string-append o (format "~a:~a~%" (car e) (cdr e))))
                     ""
                     can-amz-headers)
               resource)))

        (latin-1->string
         (base64-encode
          (hmac-sha-1 (credentials-secret-access-key credentials)
                      can-string)))))


    (define (perform-aws-request credentials
                                 uri
                                 resource
                                 body
                                 verb
                                 no-auth
                                 content-type
                                 content-length
                                 amz-headers)

      (define (aws-auth-header now)
        (let ((value (string-append
                      "AWS " (credentials-access-key-id credentials) ":"
                      (make-aws-authorization
                       credentials
                       verb ;; verb
                       resource
                       (sig-date now) ;; date
                       amz-headers ;; amz-headers
                       #f ;; content-md5
                       content-type))))
          `(authorization . ,value)))

      (let* ((now (current-date 0))
             ;; (amz-headers (if acl `((x-amz-acl . ,acl)) '()))
             (headers `((date . ,(date->string now "~a, ~d ~b ~Y ~T GMT"))
                        ,@amz-headers
                        (content-type . ,(string->symbol content-type))
                        (content-length . ,content-length)
                        ))
             )

        ;; (newline (current-error-port))
        ;; (display "verb=" (current-error-port))
        ;; (write verb (current-error-port))
        ;; (newline (current-error-port))
        ;; (display "uri=" (current-error-port))
        ;; (write (uri->string uri) (current-error-port))
        ;; (newline (current-error-port))
        ;; (display "resource=" (current-error-port))
        ;; (write resource (current-error-port))
        ;; (newline (current-error-port))
        ;; (display "now=" (current-error-port))
        ;; (write now (current-error-port))
        ;; (newline (current-error-port))
        ;; (display "headers=" (current-error-port))
        ;; (write headers (current-error-port))
        ;; (newline (current-error-port))
        ;; (display "body=" (current-error-port))
        ;; (write body (current-error-port))
        ;; (newline (current-error-port))


        (http verb uri body
              (lambda (status-code headers body-port) ;; reader-thunk
                ;; (newline)
                ;; (display "response status-code=")
                ;; (write status-code)
                ;; (newline)
                ;; (display "response headers=")
                ;; (write headers)
                ;; (newline)
                ;; (display "response body=")
                ;; (write (utf8->string (read-all-u8 body-port)))
                ;; (newline)
                (let ((status-class (response-status-class status-code)))
                  (values status-code headers body-port)))
              headers
              (lambda (headers)
                (cond (no-auth headers)
                      (else
                       ;(let ((value (aws-auth-header now)))
                       ;  (cons `(authorization . ,value) headers))
                       (cons (aws-auth-header now) headers)
                       )))
              )))


    (define (uri->bucket uri)
      (let* ((uri (if (uri? uri) uri (uri-reference uri)))
             (host (uri-host uri))
             (cs (char-set-complement (string->char-set ".")))
             (host-parts (string-tokenize host cs))
             )
        (cond ((not (= (length host-parts) 4)) #f)
              ((not (equal? (list-ref host-parts 2) "amazonaws")) #f)
              ((not (equal? (list-ref host-parts 3) "com")) #f)
              (else (car host-parts)))))

    ))
