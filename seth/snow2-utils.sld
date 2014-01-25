(define-library (seth snow2-utils)
  (export get-repository
          read-repository
          find-package-with-library
          snow2-package-libraries
          gather-depends
          install
          uninstall
          client
          main-program
          )
  (import (scheme base) (scheme read))
  (cond-expand
   (chibi (import (scheme write) (chibi filesystem)))
   (chicken (import (scheme read) (scheme write)
                    (scheme file) (srfi 1)
                    (scheme process-context)))
   (gauche (import (scheme write)
                   (scheme file)
                   (srfi 1)
                   (scheme process-context)))
   (sagittarius (import (scheme file) (scheme write) (srfi 1))))
  (import (seth srfi-69-hash-tables))
  (import (snow filesys) (snow binio) (snow genport) (snow zlib) (snow tar))
  (import (prefix (seth http) http-))
  (import (seth temporary-file))
  (import (seth string-read-write))
  (import (seth srfi-37-argument-processor))
  (begin

    (define-record-type <snow2-repository>
      (make-snow2-repository packages)
      snow2-repository?
      (packages snow2-repository-packages set-snow2-repository-packages!))

    (define-record-type <snow2-package>
      (make-snow2-package name url libraries)
      snow2-package?
      (name snow2-package-name set-snow2-package-name!)
      (url snow2-package-url set-snow2-package-url!)
      (libraries snow2-package-libraries set-snow2-package-libraries!))


    (define-record-type <snow2-library>
      (make-snow2-library name path depends)
      snow2-library?
      (name snow2-library-name set-snow2-library-name!)
      (path snow2-library-path set-snow2-library-path!)
      (depends snow2-library-depends set-snow2-library-depends!))

    (define (get-tag child)
      ;; extract the tag from an element that is assumed to be shaped like:
      ;; '(tag ...)
      (cond ((not (list? child))
             (error "not a list: ~A" child))
            ((null? child)
             (error "list is empty."))
            (else
             (car child))))


    (define (get-children-by-type obj child-type)
      ;; return any child sexps that is a list starting with child-type
      (filter (lambda (child)
                (eq? (get-tag child) child-type))
              (cdr obj)))


    (define (get-child-by-type obj child-type default)
      ;; find a non-optional child with the given tag.  the tag
      ;; is expected to be unique among the children.
      (let ((childs (get-children-by-type obj child-type)))
        (cond ((null? childs)
               (if default
                   default
                   (error "~A has no ~A\n" (get-tag obj) child-type)))
              ((> (length childs) 1)
               (error "~A has more than one ~A\n." obj child-type))
              (else
               (car childs)))))


    (define (get-string-by-type obj child-type default)
      ;; return the string from a child with the form
      ;; '(child-type "...")
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type default)))
        (cond ((and (not child) default) default)
              ((and (null? child) default) default)
              ((not child) #f)
              ((null? child) #f)
              ((not (= (length child) 2))
               (error "~A has malformed ~A: ~A\n"
                      (get-tag obj) child-type child))
              (else
               (let ((result (cadr child)))
                 (cond ((not (string? result))
                        (error
                         "value of ~A in ~A isn't a string\n"
                         child-type (get-tag obj)))
                       (else
                        result)))))))



    (define (get-list-by-type obj child-type default)
      ;; return the list from a child with the form
      ;; '(child-type (x y z))
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type default)))
        (cond ((and (not child) default) default)
              ((and (null? child) default) default)
              ((not child) #f)
              ((null? child) #f)
              ((not (= (length child) 2))
               (error "~A has malformed ~A: ~A\n"
                      (get-tag obj) child-type child))
              (else
               (let ((result (cadr child)))
                 (cond ((not (list? result))
                        (error
                         "value of ~A in ~A isn't a list: ~A\n"
                         child-type (get-tag obj) result))
                       (else
                        result)))))))


    (define (get-args-by-type obj child-type default)
      ;; return the list '(x y z) from a child with the form
      ;; '(child-type x y z)
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type default)))
        (cond ((and (not child) default) default)
              ((and (null? child) default) default)
              ((not child) #f)
              ((null? child) #f)
              (else
               (cdr child)))))


    (define (depend-from-sexp depend-sexp)
      depend-sexp)


    (define (library-from-sexp library-sexp)
      ;; convert an s-exp into a library record
      (let ((name (get-list-by-type library-sexp 'name #f))
            (path (get-string-by-type library-sexp 'path #f))
            (depends-sexps (get-args-by-type library-sexp 'depends '())))
        (cond ((not name) #f)
              ((not path) #f)
              (else
               (make-snow2-library name path
                                   (map depend-from-sexp depends-sexps))))))


    (define (package-from-sexp package-sexp)
      ;; convert a s-exp into a package record
      (let ((url (get-string-by-type package-sexp 'url #f))
            (name (get-list-by-type package-sexp 'name '()))
            (library-sexps (get-children-by-type package-sexp 'library)))
        (cond ((not url) #f)
              ((not name) #f)
              (else
               (let ((libraries (map library-from-sexp library-sexps)))
                 (make-snow2-package name url libraries))))))


    (define (repository-from-sexp repository-sexp)
      ;; convert an s-exp into a repository record
      (cond ((not (list? repository-sexp))
             (error "repository definition isn't a list."))
            ((null? repository-sexp)
             (error "repository is empty."))
            ((not (eq? (car repository-sexp) 'repository))
             (error "this doesn't look like a repository."))
            (else
             (let* ((package-sexps
                     (get-children-by-type repository-sexp 'package))
                    (packages (map package-from-sexp package-sexps)))
               (make-snow2-repository packages)))))


    (define (read-repository in-port)
      ;; read an s-exp from (current-input-port) and convert it to
      ;; a repository record
      (let* ((repository-sexp (read in-port))
             (repository (repository-from-sexp repository-sexp)))
        repository))


    (define (package-contains-library? package library-name)
      ;; return #t if a package contains any libraries with the given name
      (let loop ((libraries (snow2-package-libraries package)))
        (cond ((null? libraries) #f)
              (else
               (let ((library (car libraries)))
                 (if (equal? (snow2-library-name library) library-name)
                     #t
                     (loop (cdr libraries))))))))


    (define (find-package-with-library repositories library-name)
      ;; find the last package that contains a library with the given name
      (let r-loop ((repositories repositories)
                   (candidate-packages '()))
        (cond
         ((null? repositories)
          (cond ((null? candidate-packages)
                 (error "couldn't find library" library-name)
                 #f)
                ;; XXX rather than just taking the last one,
                ;; select one based on version requirements, etc
                (else (car candidate-packages))))
         (else
          (let loop ((packages (snow2-repository-packages (car repositories)))
                     (candidate-packages candidate-packages))
            (cond ((null? packages)
                   (r-loop (cdr repositories) candidate-packages))
                  (else
                   (let ((package (car packages)))
                     (loop (cdr packages)
                           (if (package-contains-library? package library-name)
                               (cons package candidate-packages)
                               candidate-packages))))))))))


    (define (library-from-name repositories library-name)
      (let ((package (find-package-with-library repositories library-name)))
        (cond ((not package)
               (error
                "can't find package that contains ~S\n" library-name)
               #f)
              (else
               (let loop ((libraries (snow2-package-libraries package)))
                 (cond ((null? libraries) #f)
                       ((equal? library-name
                                (snow2-library-name (car libraries)))
                        (car libraries))
                       (else (loop (cdr libraries)))))))))


    (define (gather-depends repositories libraries)
      (let ((lib-name-ht (make-hash-table))
            (package-url-ht (make-hash-table)))
        (for-each
         (lambda (library)

           (let* ((lib-name (snow2-library-name library))
                  (package (find-package-with-library repositories lib-name)))
             (hash-table-set! lib-name-ht lib-name #t)
             (hash-table-set! package-url-ht (snow2-package-url package) #t))

           (for-each
            (lambda (depend)
              (let* ((package (find-package-with-library repositories depend))
                     (libs (snow2-package-libraries package)))
                (hash-table-set! package-url-ht (snow2-package-url package) #t)
                (for-each
                 (lambda (lib)
                   (hash-table-set! lib-name-ht (snow2-library-name lib) #t))
                 libs)))
            (snow2-library-depends library)))
         libraries)

        (let* ((result-names (hash-table-keys lib-name-ht))
               (result (map (lambda (library-name)
                              (library-from-name repositories library-name))
                            result-names)))
          (cond ((= (length result) (length libraries))
                 (hash-table-keys package-url-ht))
                (else
                 (gather-depends repositories result))))))


    (define (get-repository repository-url)
      (http-call-with-request-body repository-url read-repository))

    (define (write-tar-recs-to-disk tar-recs)
      (let loop ((tar-recs tar-recs))
        (cond ((null? tar-recs) #t)
              (else
               (let ((t (car tar-recs)))
                 (cond
                  ((eq? (tar-rec-type t) 'directory)
                   (snow-create-directory-recursive
                    (tar-rec-name t)))
                  ((eq? (tar-rec-type t) 'regular)
                   (cond ((or (snow-file-symbolic-link? (tar-rec-name t))
                              (snow-file-directory? (tar-rec-name t)))
                          (display "not overwriting " (current-error-port))
                          (display (tar-rec-name t) (current-error-port))
                          (newline (current-error-port)))
                         (else
                          (let ((hndl (binio-open-output-file
                                       (tar-rec-name t))))
                            (binio-write-subu8vector
                             (tar-rec-content t) 0
                             (bytevector-length (tar-rec-content t)) hndl)))))
                  (else
                   (error "unexpected file type in tar file")))
                 (loop (cdr tar-recs)))))))


    (define (install repositories library-name)

      (let ((package (find-package-with-library repositories library-name)))
        (cond ((not package)
               (error "didn't find a package with library: ~S\n"
                      library-name))
              (else
               (let* ((libraries (snow2-package-libraries package))
                      (urls (gather-depends repositories libraries)))

                 (for-each
                  (lambda (url)
                    (display "installing ")
                    (display url)
                    (newline)

                    (let-values (((write-port local-package-filename)
                                  (temporary-file)))
                      (http-download-file url write-port)
                      (let* ((bin-port (binio-open-input-file
                                        local-package-filename))
                             (zipped-p
                              (genport-native-input-port->genport bin-port))
                             (unzipped-p (gunzip-genport zipped-p))
                             (tar-recs (tar-unpack-genport unzipped-p)))
                        (genport-close-input-port unzipped-p)
                        (write-tar-recs-to-disk tar-recs))
                      (delete-file local-package-filename)))
                  urls))))))

    (define (uninstall repositories library-name)
      #f)


    (define (client repository-urls operation library-name)
      (let ((repositories (map get-repository repository-urls)))
        (cond ((equal? operation "install")
               (install repositories library-name))
              ((equal? operation "uninstall")
               (uninstall repositories library-name))
              )))


    (define options
      (list
       (option '(#\r "repo") #t #f
               (lambda (option name arg operation repos libs verbose)
                 (values operation (cons arg repos) libs verbose)))

       (option '(#\v "verbose") #f #f
               (lambda (option name arg operation repos libs verbose)
                 (values operation repos libs #t)))

       (option '(#\h "help") #f #f
               (lambda (option name arg operation repos libs verbose)
                 (usage "")))
       ))


    (define (usage msg)
      (let ((pargs (command-line)))
        (display msg (current-error-port))
        (display (car pargs) (current-error-port))
        (display " " (current-error-port))
        (display "[arguments] <operation> '(library name)' ...\n"
                 (current-error-port))
        (display "  <operation> can be \"install\" or \"uninstall\"\n"
                 (current-error-port))
        (display "  -r --repo <url>      " (current-error-port))
        (display "Prepend to list of snow2 repositories.\n"
                 (current-error-port))
        (display "  -v --verbose         " (current-error-port))
        (display "Print more.\n" (current-error-port))
        (display "  -h --help            " (current-error-port))
        (display "Print usage message.\n" (current-error-port))
        (exit 1)))


    (define (main-program)
      (let-values
          (((operation repository-urls libs verbose)
            (args-fold
             (cdr (command-line))
             options
             ;; unrecognized
             (lambda (option name arg . seeds)
               ;; (error "Unrecognized option:" name)
               (usage (string-append "Unrecognized option:"
                                     (if (string? name) name (string name))
                                     "\n\n")))
             ;; operand (arguments that don't start with a hyphen)
             (lambda (operand operation repos libs verbose)
               (if operation
                   (values operation repos (cons operand libs) verbose)
                   (values operand repos libs verbose)))
             #f ;; initial value of operation
             ;; initial value of repos
             '("http://snow2.s3-website-us-east-1.amazonaws.com/"
               "http://snow-repository.s3-website-us-east-1.amazonaws.com/")
             '() ;; initial value of libs
             #f ;; initial value of verbose
             )))
        (cond ((not operation) (usage ""))
              ((not (member operation '("install" "uninstall")))
               (usage (string-append "Unknown operation: " operation "\n\n")))
              (else
               (for-each
                (lambda (library-name-argument)
                  (let ((library-name (read-from-string library-name-argument)))
                    (client repository-urls operation library-name)))
                libs)))))
    ))
