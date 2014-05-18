(define-library (seth aws s3)
  (export
   list-buckets
   list-objects
   bucket-exists?
   create-bucket!
   delete-bucket!
   get-object
   get-object-md5
   put-object!
   delete-object!
   )
  (import (scheme base)
          (scheme write)
          (snow snowlib)
          (snow bytevector)
          (snow srfi-13-strings)
          (snow extio)
          (seth uri)
          (seth port-extras)
          (seth xml ssax)
          (seth xml sxpath)
          (only (seth http) response-status-class)
          (seth aws common)
          )
  (begin

    ;; http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html
    ;; http://docs.aws.amazon.com/AmazonS3/latest/API/RESTServiceGET.html

    (define s3-authority "s3.amazonaws.com")
    (define s3-namespace "http://s3.amazonaws.com/doc/2006-03-01/")


    (define (s3-error who p)
      (let ((data (read-all-u8 p)))
        (error who (utf8->string data))))


    (define (make-s3-uri bucket key)
      (make-uri
       'scheme 'http
       'host (if bucket (string-append bucket "." s3-authority) s3-authority)
       'port 80
       'path (make-path key)))


    (define (make-s3-resource bucket path)
      (string-append "/"
                     (if bucket (string-append bucket "/") "")
                     ;; (if path path "")
                     (cond ((not path) "")
                           ((string-prefix? "/" path)
                            (substring path 1 (string-length path)))
                           (else path))
                     ))


    (define (bucket-exists? credentials bucket)
      (let-values
          (((status-code headers data)
            (perform-aws-request
             credentials ;; credentials
             (make-s3-uri bucket #f)
             (make-s3-resource bucket #f)
             "" ;; body
             "HEAD" ;; verb
             #f ;; no-auth
             "application/x-www-form-urlencoded" ;; content-type
             0 ;; content-length
             '() ;; amz-headers
             )))
        (= (response-status-class status-code) 200)))


    (define (create-bucket! credentials bucket)
      (let-values
          (((status-code headers data)
            (perform-aws-request
             credentials ;; credentials
             (make-s3-uri bucket #f)
             (make-s3-resource bucket #f)
             "" ;; body
             "PUT" ;; verb
             #f ;; no-auth
             "application/x-www-form-urlencoded" ;; content-type
             0 ;; content-length
             '() ;; amz-headers
             )))
        (= (response-status-class status-code) 200)))


    (define (delete-bucket! credentials bucket)
      (let-values
          (((status-code headers data)
            (perform-aws-request
             credentials ;; credentials
             (make-s3-uri bucket #f)
             (make-s3-resource bucket #f)
             "" ;; body
             "DELETE" ;; verb
             #f ;; no-auth
             "application/x-www-form-urlencoded" ;; content-type
             0 ;; content-length
             '() ;; amz-headers
             )))
        (= (response-status-class status-code) 200)))


    (define (list-buckets credentials)
      (let-values
          (((status-code headers data)
            (perform-aws-request
             credentials ;; credentials
             (make-s3-uri #f #f)
             (make-s3-resource #f #f)
             "" ;; body
             "GET" ;; verb
             #f ;; no-auth
             "application/x-www-form-urlencoded" ;; content-type
             0 ;; content-length
             '() ;; amz-headers
             )))
        (cond ((= (response-status-class status-code) 200)

               ((sxpath
                 '(x:ListAllMyBucketsResult x:Buckets x:Bucket x:Name *text*))
                (ssax:xml->sxml
                 (binary-port->textual-port data)
                 `((x . ,s3-namespace))))

               )
              (else (s3-error "list-buckets" data)
                    #f))))


    (define (list-objects credentials bucket)
      (let-values
          (((status-code headers data)
            (perform-aws-request
             credentials
             (make-s3-uri bucket #f)
             (make-s3-resource bucket #f)
             "" ;; body
             "GET" ;; verb
             #f ;; no-auth
             "application/x-www-form-urlencoded" ;; content-type
             0 ;; content-length
             '() ;; amz-headers
             )))
        (cond ((= (response-status-class status-code) 200)

               ((sxpath
                 '(x:ListBucketResult x:Contents x:Key *text*))
                (ssax:xml->sxml
                 (binary-port->textual-port data)
                 `((x . ,s3-namespace))))

               )
              (else (s3-error "list-objects" data)
                    #f))))


    (define (get-object credentials bucket key)
      (let-values
          (((status-code headers data)
            (perform-aws-request
             credentials
             (make-s3-uri bucket key)
             (make-s3-resource bucket key)
             "" ;; body
             "GET" ;; verb
             #f ;; no-auth
             "application/x-www-form-urlencoded" ;; content-type
             0 ;; content-length
             '() ;; amz-headers
             )))
        (cond ((= (response-status-class status-code) 200) (read-all-u8 data))
              (else
               ;; (s3-error "get-object" data)
               #f))))


    (define (get-object-md5 credentials bucket key)
      (let-values
          (((status-code headers data)
            (perform-aws-request
             credentials
             (make-s3-uri bucket key)
             (make-s3-resource bucket key)
             "" ;; body
             "HEAD" ;; verb
             #f ;; no-auth
             "application/x-www-form-urlencoded" ;; content-type
             0 ;; content-length
             '() ;; amz-headers
             )))
        (cond ((= (response-status-class status-code) 200)
               (read-all-u8 data)
               ;; (etag . "\"900150983cd24fb0d6963f7d28e17f72\"")
               (hex-string->bytes
                (string-trim-both
                 (cdr (assq 'etag headers))
                 #\")))
              (else
               ;; (s3-error "get-object-md5" data)
               #f))))


    (define (get-body-and-size body content-length)
      (cond (content-length (values body content-length))
            ((bytevector? body) (values body (bytevector-length body)))
            ((string? body)
             (let ((data (string->utf8 body)))
               (values data (bytevector-length data))))
            ((and (input-port? body) (binary-port? body))
             (let ((data (read-all-u8 body)))
               (values data (bytevector-length data))))
            ((input-port? body)
             (let ((data (string->utf8 (read-all-chars body))))
               (values data (bytevector-length data))))
            (else
             (error "aws s3 put-object! unusable body type" body))))


    (define (put-object! credentials bucket key body .
                         maybe-length+type+acl)
      (let ((content-length (if (> (length maybe-length+type+acl) 0)
                                (list-ref maybe-length+type+acl 0)
                                #f))
            (content-type (if (> (length maybe-length+type+acl) 1)
                              (list-ref maybe-length+type+acl 1)
                              "application/octet-stream"))
            (acl (if (> (length maybe-length+type+acl) 2)
                     (list-ref maybe-length+type+acl 2)
                     #f)))
        (let*-values
            (((body content-length) (get-body-and-size body content-length))
             ((status-code headers data)
              (perform-aws-request
               credentials
               (make-s3-uri bucket key)
               (make-s3-resource bucket key)
               body ;; body
               "PUT" ;; verb
               #f ;; no-auth
               content-type ;; content-type
               content-length ;; content-length
               (if acl `(("x-amz-acl" . ,acl)) '()) ;; amz-headers
               )))
          (cond ((= (response-status-class status-code) 200)
                 (read-all-u8 data))
                (else (s3-error "put-object!" data)
                      #f)))))


    (define (delete-object! credentials bucket key)
      (let-values
          (((status-code headers data)
            (perform-aws-request
             credentials
             (make-s3-uri bucket key)
             (make-s3-resource bucket key)
             "" ;; body
             "DELETE" ;; verb
             #f ;; no-auth
             "application/x-www-form-urlencoded" ;; content-type
             0 ;; content-length
             '() ;; amz-headers
             )))
        (cond ((= (response-status-class status-code) 200) (read-all-u8 data))
              (else (s3-error "delete-object!" data)
                    #f))))

    ))
