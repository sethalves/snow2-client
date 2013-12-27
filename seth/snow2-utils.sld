(define-library (seth snow2-utils)
  (export read-repository
          find-package-with-library
          package-libraries
          gather-depends)
  (cond-expand
   (chibi
    (import (scheme base) (scheme read) (srfi 1) (srfi 69)))
   (chicken
    (import (scheme base) (chicken) (posix) (srfi 1) (srfi 69)))
   (gauche
    (import (scheme base))))
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
      (libraries package-libraries set-package-libraries!))


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


    (define (read-repository)
      ;; read an s-exp from (current-input-port) and convert it to
      ;; a repository record
      (let* ((repository-sexp (read))
             (repository (repository-from-sexp repository-sexp)))
        repository))


    (define (package-contains-library? package library-name)
      ;; return #t if a package contains any libraries with the given name
      (let loop ((libraries (package-libraries package)))
        (cond ((null? libraries) #f)
              (else
               (let ((library (car libraries)))
                 (if (equal? (snow2-library-name library) library-name)
                     #t
                     (loop (cdr libraries))))))))


    (define (find-package-with-library repository library-name)
      ;; find the last package that contains a library with the given name
      (let loop ((packages (snow2-repository-packages repository))
                 (candidate-packages '()))
        (cond ((null? packages)
               (cond ((null? candidate-packages) #f)
                     ;; XXX rather than just taking the last one,
                     ;; select one based on version requirements, etc
                     (else (car candidate-packages))))
              (else
               (let ((package (car packages)))
                 (loop (cdr packages)
                       (if (package-contains-library? package library-name)
                           (cons package candidate-packages)
                           candidate-packages)))))))


    (define (library-from-name repository library-name)
      (let ((package (find-package-with-library repository library-name)))
        (cond ((not package)
               (error
                "can't find package that contains ~S\n" library-name)
               #f)
              (else
               (let loop ((libraries (package-libraries package)))
                 (cond ((null? libraries) #f)
                       ((equal? library-name
                                (snow2-library-name (car libraries)))
                        (car libraries))
                       (else (loop (cdr libraries)))))))))


    (define (gather-depends repository libraries)
      (let ((lib-name-ht (make-hash-table))
            (package-url-ht (make-hash-table)))
        (for-each
         (lambda (library)

           (let* ((lib-name (snow2-library-name library))
                  (package (find-package-with-library repository lib-name)))
             (hash-table-set! lib-name-ht lib-name #t)
             (hash-table-set! package-url-ht (snow2-package-url package) #t))

           (for-each
            (lambda (depend)
              (let* ((package (find-package-with-library repository depend))
                     (libs (package-libraries package)))
                (hash-table-set! package-url-ht (snow2-package-url package) #t)
                (for-each
                 (lambda (lib)
                   (hash-table-set! lib-name-ht (snow2-library-name lib) #t))
                 libs)))
            (snow2-library-depends library)))
         libraries)

        (let* ((result-names (hash-table-keys lib-name-ht))
               (result (map (lambda (library-name)
                              (library-from-name repository library-name))
                            result-names)))
          (cond ((= (length result) (length libraries))
                 (hash-table-keys package-url-ht))
                (else
                 (gather-depends repository result))))))

    ))
