(define-library (seth snow2 types)
  (export make-snow2-repository
          snow2-repository?
          snow2-repository-name set-snow2-repository-name!
          snow2-repository-siblings set-snow2-repository-siblings!
          snow2-repository-packages set-snow2-repository-packages!
          snow2-repository-local set-snow2-repository-local!
          snow2-repository-url set-snow2-repository-url!
          snow2-repository-dirty set-snow2-repository-dirty!
          snow2-repository-force
          snow2-repository-local-or-url

          make-snow2-sibling
          snow2-sibling?
          snow2-sibling-name set-snow2-sibling-name!
          snow2-sibling-url set-snow2-sibling-url!
          snow2-sibling-trust set-snow2-sibling-trust!

          make-snow2-package
          snow2-package?
          snow2-package-name set-snow2-package-name!
          snow2-package-version set-snow2-package-version!
          snow2-package-url set-snow2-package-url!
          snow2-package-libraries set-snow2-package-libraries!
          snow2-package-repository set-snow2-package-repository!
          snow2-package-size set-snow2-package-size!
          snow2-package-checksum set-snow2-package-checksum!
          snow2-package-dirty set-snow2-package-dirty!

          make-snow2-library
          snow2-library?
          snow2-library-name set-snow2-library-name!
          snow2-library-path set-snow2-library-path!
          snow2-library-depends set-snow2-library-depends!
          snow2-library-version set-snow2-library-version!
          snow2-library-homepage set-snow2-library-homepage!
          snow2-library-manual set-snow2-library-manual!
          snow2-library-maintainers set-snow2-library-maintainers!
          snow2-library-authors set-snow2-library-authors!
          snow2-library-description set-snow2-library-description!
          snow2-library-license set-snow2-library-license!
          snow2-library-package set-snow2-library-package!
          snow2-library-use-for set-snow2-library-use-for!

          get-children-by-type
          get-child-by-type
          get-string-by-type
          get-number-by-type
          get-list-by-type
          get-args-by-type
          get-multi-args-by-type

          snow2-libraries-equal?
          snow2-packages-equal?
          snow2-package-get-readable-name

          snow2-trace
          )


  (import (scheme base)
          (scheme write)
          (scheme lazy))
  (cond-expand
   (chibi (import (only (srfi 1) filter make-list any fold last)))
   (else (import (srfi 1))))

  (import (except (srfi 13)
                  string-copy string-map string-for-each
                  string-fill! string-copy! string->list
                  string-upcase string-downcase)
          (seth uri)
          (seth string-read-write))

  (begin

    ;; repositories might be promises.
    (define-record-type <snow2-repository>
      (make-snow2-repository name siblings packages local url dirty)
      snow2-repository~?
      (name snow2-repository-name~ set-snow2-repository-name~!)
      (siblings snow2-repository-siblings~ set-snow2-repository-siblings~!)
      (packages snow2-repository-packages~ set-snow2-repository-packages~!)
      (local snow2-repository-local~ set-snow2-repository-local~!)
      (url snow2-repository-url~ set-snow2-repository-url~!)
      (dirty snow2-repository-dirty~ set-snow2-repository-dirty~!))


    (define (snow2-repository-force repo)
      (let ((frepo (if (promise? repo) (force repo) repo)))
        frepo))

    (define (snow2-repository? repo)
      (snow2-repository~? (snow2-repository-force repo)))
    (define (snow2-repository-name repo)
      (snow2-repository-name~ (snow2-repository-force repo)))
    (define (set-snow2-repository-name! repo v)
      (set-snow2-repository-name~! (snow2-repository-force repo) v))
    (define (snow2-repository-siblings repo)
      (snow2-repository-siblings~ (snow2-repository-force repo)))
    (define (set-snow2-repository-siblings! repo v)
      (set-snow2-repository-siblings~! (snow2-repository-force repo) v))
    (define (snow2-repository-packages repo)
      (snow2-repository-packages~ (snow2-repository-force repo)))
    (define (set-snow2-repository-packages! repo v)
      (set-snow2-repository-packages~! (snow2-repository-force repo) v))
    (define (snow2-repository-local repo)
      (snow2-repository-local~ (snow2-repository-force repo)))
    (define (set-snow2-repository-local! repo v)
      (set-snow2-repository-local~! (snow2-repository-force repo) v))
    (define (snow2-repository-url repo)
      (snow2-repository-url~ (snow2-repository-force repo)))
    (define (set-snow2-repository-url! repo v)
      (set-snow2-repository-url~! (snow2-repository-force repo) v))
    (define (snow2-repository-dirty repo)
      (snow2-repository-dirty~ (snow2-repository-force repo)))
    (define (set-snow2-repository-dirty! repo v)
      (set-snow2-repository-dirty~! (snow2-repository-force repo) v))

    (define (snow2-repository-local-or-url repo)
      (or (snow2-repository-local repo) (snow2-repository-url repo)))


    (define-record-type <snow2-sibling>
      (make-snow2-sibling name url trust)
      snow2-sibling?
      (name snow2-sibling-name set-snow2-sibling-name!)
      (url snow2-sibling-url set-snow2-sibling-url!)
      (trust snow2-sibling-trust set-snow2-sibling-trust!))


    (define-record-type <snow2-package>
      (make-snow2-package name version url libraries repo size checksum dirty)
      snow2-package?
      (name snow2-package-name set-snow2-package-name!)
      (version snow2-package-version set-snow2-package-version!)
      (url snow2-package-url set-snow2-package-url!)
      (libraries snow2-package-libraries set-snow2-package-libraries!)
      (repo snow2-package-repository set-snow2-package-repository!)
      (size snow2-package-size set-snow2-package-size!)
      (checksum snow2-package-checksum set-snow2-package-checksum!)
      (dirty snow2-package-dirty set-snow2-package-dirty!))


    (define-record-type <snow2-library>
      (make-snow2-library name path depends version homepage manual
                          maintainers authors description license
                          package use-for)
      snow2-library?
      (name snow2-library-name set-snow2-library-name!)
      (path snow2-library-path set-snow2-library-path!)
      (depends snow2-library-depends set-snow2-library-depends!)
      (version snow2-library-version set-snow2-library-version!)
      (homepage snow2-library-homepage set-snow2-library-homepage!)
      (manual snow2-library-manual set-snow2-library-manual!)
      (maintainers snow2-library-maintainers set-snow2-library-maintainers!)
      (authors snow2-library-authors set-snow2-library-authors!)
      (description snow2-library-description set-snow2-library-description!)
      (license snow2-library-license set-snow2-library-license!)
      (package snow2-library-package set-snow2-library-package!)
      (use-for snow2-library-use-for set-snow2-library-use-for!))


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
      ;; return any child sexps that are lists starting with child-type
      (filter (lambda (child)
                (eq? (get-tag child) child-type))
              (cdr obj)))


    (define (get-child-by-type obj child-type . default)
      ;; find a child with the given tag.  the tag
      ;; is expected to be unique among the children.
      ;; if default is #f, a missing child will raise an error.
      (let ((childs (get-children-by-type obj child-type)))
        (cond ((null? childs)
               (if (pair? default)
                   (car default)
                   (error "~A has no ~A\n" (get-tag obj) child-type)))
              ((> (length childs) 1)
               (error "~A has more than one ~A\n." obj child-type))
              (else
               (car childs)))))


    (define (get-string-by-type obj child-type default)
      ;; return the string from a child with the form
      ;; '(child-type "...")
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type #f)))
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


    (define (get-number-by-type obj child-type default)
      ;; return the number from a child with the form
      ;; '(child-type 1.0)
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type #f)))
        (cond ((and (not child) default) default)
              ((and (null? child) default) default)
              ((not child) #f)
              ((null? child) #f)
              ((not (= (length child) 2))
               (error "~A has malformed ~A: ~A\n"
                      (get-tag obj) child-type child))
              (else
               (let ((result (cadr child)))
                 (cond ((not (number? result))
                        (error
                         "value of ~A in ~A isn't a number\n"
                         child-type (get-tag obj)))
                       (else
                        result)))))))



    (define (get-list-by-type obj child-type default)
      ;; return the list from a child with the form
      ;; '(child-type (x y z))
      ;; if no such child is found and default isn't #f, return default
      (let ((child (get-child-by-type obj child-type #f)))
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

    (define (get-multi-args-by-type obj child-type default)
      ;; return the appended list '(a b c x y z) from children with the form
      ;; '(child-type a b c)
      ;; '(child-type x y z)
      ;; if no such children are found and default isn't #f, return default
      (let ((childs (get-children-by-type obj child-type)))
        (cond ((and (null? childs) default) default)
              (else
               (fold append '() (map cdr childs))))))


    (define (snow2-libraries-equal? a b)
      (and ;; (equal? (snow2-library-name a) (snow2-library-name b))
           (equal? (snow2-library-path a) (snow2-library-path b))
           ;; (equal? (snow2-library-depends a) (snow2-library-depends b))
           (equal? (snow2-library-version a) (snow2-library-version b))
           (equal? (snow2-library-homepage a) (snow2-library-homepage b))
           (equal? (snow2-library-manual a) (snow2-library-manual b))
           (equal? (snow2-library-maintainers a) (snow2-library-maintainers b))
           (equal? (snow2-library-authors a) (snow2-library-authors b))
           (equal? (snow2-library-description a) (snow2-library-description b))
           (equal? (snow2-library-license a) (snow2-library-license b))
           (equal? (snow2-library-use-for a) (snow2-library-use-for b))))


    (define (snow2-library-lists-equal? a b)
      (cond ((and (null? a) (null? b)) #t)
            ((null? a) #f)
            ((null? b) #f)
            ((not (snow2-libraries-equal? (car a) (car b))) #f)
            (else (snow2-library-lists-equal? (cdr a) (cdr b)))))


    (define (snow2-packages-equal? a b)
      (and (equal? (snow2-package-name a) (snow2-package-name b))
           (equal? (snow2-package-version a) (snow2-package-version b))
           (uri-equal? (snow2-package-url a) (snow2-package-url b))
           ;; (equal? (snow2-package-libraries a) (snow2-package-libraries b))
           (snow2-library-lists-equal? (snow2-package-libraries a)
                                       (snow2-package-libraries b))
           ))

    (define (snow2-package-get-readable-name package)
      (let* ((name (snow2-package-name package))
             (url (snow2-package-url package)))
        (cond ((and name (string? name) (not (equal? name "")))
               name)
              ((and name (list? name) (not (equal? name '())))
               (string-join (map ->string name) "-"))
              ((and url (pair? (uri-path url)))
               (let ((tgz-name (last (uri-path url))))
                 (if (string-suffix? ".tgz" tgz-name)
                     (substring tgz-name 0
                                (- (string-length tgz-name) 4))
                     tgz-name)))
              (else "unknown"))))



    ;; (define-syntax snow2-trace
    ;;   (syntax-rules ()
    ;;     ((_ what) (begin
    ;;                 (write what)
    ;;                 (newline)))))

    (define-syntax snow2-trace
      (syntax-rules ()
        ((_ what) #t)))

    ))
