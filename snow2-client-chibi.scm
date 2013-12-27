#! /usr/bin/env chibi-scheme

(import (scheme process-context))
(import (scheme base))
(import (scheme read))
(import (scheme write))
(import (scheme file))
(import (srfi 1))
(import (srfi 69))
(import (chibi filesystem))
(import (chibi net http))
(import (chibi process))
(import (prefix (seth tar) tar-) (seth snow2-utils))


(define (read-from-string s)
  (read (open-input-string s)))


(define (decide-local-package-filename url)
  (string-append "/tmp/snow2-"
                 (number->string (current-process-id))
                 ".tgz"))


(define (download-file url local-filename)
  ;; (display "downloading: ")
  ;; (display url)
  ;; (display " to ")
  ;; (display local-filename)
  ;; (newline)
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
                  (loop)))))))))


(define (get-repository repository-url)
  ;; (with-input-from-request repository-url #f read-repository)
  (call-with-input-url repository-url read-repository))


(define (snow2-install repository library-name)
  (let ((package (find-package-with-library repository library-name)))
    (cond ((not package)
           (error "didn't find a package with library: ~S\n"
                  library-name))
          (else
           (let* ((libraries (snow2-package-libraries package))
                  (urls (gather-depends repository libraries)))

             (for-each
              (lambda (url)
                (display "installing ")
                (display url)
                (newline)
                (let ((local-package-filename
                       (decide-local-package-filename url)))
                  (download-file url local-package-filename)
                  (tar-extract local-package-filename)
                  (delete-file local-package-filename)))
              urls))))))



(define (snow2-uninstall repository library-name)
  #f)



(define (usage pargs)
  (display (car pargs))
  (display " ")
  (display "<operation> '(library name)'")
  (newline)
  (display "  <operation> can be \"install\" or \"uninstall\"")
  (newline))


(define (main-program)
  (let* ((repository-url
          "http://snow2.s3-website-us-east-1.amazonaws.com/")
         (repository (get-repository repository-url))
         (pargs (command-line)))
    (cond ((not (= (length pargs) 3))
           (usage pargs))
          (else
           (let ((operation (list-ref pargs 1))
                 (library-name (read-from-string (list-ref pargs 2))))
             (cond ((equal? operation "install")
                    (snow2-install repository library-name))
                   ((equal? operation "uninstall")
                    (snow2-uninstall repository library-name))
                   ))))))


(main-program)
