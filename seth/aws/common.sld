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
          (scheme process-context)
          (srfi 1)
          (snow bytevector)
          (except (srfi 13)
                  string-copy string-map string-for-each
                  string-fill! string-copy! string->list
                  string-upcase string-downcase)
          (srfi 95)
          (srfi 29)
          (srfi 19)
          (snow extio)
          (seth http)
          (seth crypt hmac-sha-1)
          (seth uri)
          (prefix (seth base64) base64-)
          )

  (cond-expand
   (chibi (import (chibi char-set)))
   (else (import (srfi 14))))
  (begin

    ;; http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html


    (define-record-type <credentials>
      (make-credentials access-key-id secret-access-key)
      credentials?
      (access-key-id credentials-access-key-id)
      (secret-access-key credentials-secret-access-key))

    (define (read-credentials filename)
      ;;
      ;; aws credentials in a file usually look like:
      ;;
      ;; AWSAccessKeyId=AKXXXXXXXXXXXXXXXXXX
      ;; AWSSecretKey=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      ;;
      ;; or
      ;;
      ;; aws_access_key_id = AKXXXXXXXXXXXXXXXXXX
      ;; aws_secret_access_key = XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      ;;
      (let ((creds-hndl (open-input-file filename)))
        (let loop ((access-key-id #f)
                   (secret-access-key #f))
          ;; read each line
          (let ((line (read-line creds-hndl)))
            (cond ((eof-object? line)
                   (close-input-port creds-hndl)
                   (if (and access-key-id secret-access-key)
                       (make-credentials access-key-id secret-access-key)
                       #f))
                  (else
                   ;; search the line for and = or :
                   (let ((position-of-delimiter (or (string-index line #\=)
                                                    (string-index line #\:))))
                     (if (not position-of-delimiter)
                         ;; if neither = nor : is found, just ignore the line
                         (loop access-key-id secret-access-key)
                         (let ((secret-or-id
                                ;; chop off anything before : or = and trim
                                (string-trim-both
                                 (substring line
                                            (+ position-of-delimiter 1)
                                            (string-length line))
                                 char-whitespace?)))
                           ;; access-key-id is shorter than secret
                           (if (> (string-length secret-or-id) 26)
                               (loop access-key-id secret-or-id)
                               (loop secret-or-id secret-access-key)))))))))))


    (define (get-credentials-for-s3-bucket bucket)
      ;;
      ;; http://boto.readthedocs.org/en/latest/boto_config_tut.html
      ;; Some options, such as credentials, can also be read from environment
      ;; variables (e.g. AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY,
      ;; AWS_SECURITY_TOKEN and AWS_PROFILE)
      ;;
      ;; http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-started.html
      ;; 1. Environment Variables – AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY.
      ;;
      (let ((env-key (get-environment-variable "AWS_ACCESS_KEY_ID"))
            (env-secret (get-environment-variable "AWS_SECRET_ACCESS_KEY"))
            (cred-file (get-environment-variable "AWS_CREDENTIAL_FILE")))
        (cond ((and env-key env-secret)
               (make-credentials env-key env-secret))
              ((and cred-file (read-credentials cred-file))
               => (lambda (creds) creds))
              ((read-credentials (string-append "/etc/aws/s3-" bucket))
               => (lambda (creds) creds))
              (else #f))))


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
             (->str (lambda (x) (if (string? x) x (symbol->string x))))
             (can-string
              (string-append
               (string-upcase verb) "\n"
               (if content-md5 content-md5 "") "\n"
               (if content-type content-type "") "\n"
               (if date date "") "\n"
               (fold (lambda (e o)
                       ;; (string-append o (format "~a:~a~%" (car e) (cdr e)))
                       (string-append (->str o)
                                      (->str (car e)) ":" (->str (cdr e))
                                      "\n"))
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
      (let* ((uri (if (uri-reference? uri) uri (uri-reference uri)))
             (host (uri-host uri))
             (cs (char-set-complement (string->char-set ".")))
             (host-parts (if host (string-tokenize host cs) '()))
             )
        (cond ((not (= (length host-parts) 4)) #f)
              ((not (equal? (list-ref host-parts 2) "amazonaws")) #f)
              ((not (equal? (list-ref host-parts 3) "com")) #f)
              (else (car host-parts)))))

    ))
