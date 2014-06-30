;;;============================================================================

;;; File: "filesys.scm", Time-stamp: <2007-09-01 22:23:06 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to access the file system.


(define-library (snow filesys)
  (export snow-directory-files
          snow-file-directory?
          snow-file-regular?
          snow-file-symbolic-link?
          snow-file-size
          snow-file-mtime
          snow-rename-file
          snow-create-directory
          snow-delete-directory
          snow-filename-extension
          snow-filename-strip-extension
          snow-filename-directory
          snow-filename-strip-directory
          snow-filename-strip-trailing-directory-separator
          snow-make-filename
          snow-combine-filename-parts
          snow-split-filename
          snow-filename-relative?
          ;; snow-make-temp-filename
          snow-directory-subfiles
          snow-directory-tree-walk
          snow-create-directory-recursive
          snow-create-symbolic-link

          current-directory
          change-directory
          )
  (import (scheme base)
          (scheme write)
          (scheme file)
          ;; (snow bytevector)
          ;; (snow random)
          (srfi 13))
  (cond-expand
   (chibi
    ;; http://synthcode.com/scheme/chibi/lib/chibi/filesystem.html
    (import (only (srfi 1) filter)
            (chibi filesystem)
            (chibi char-set)
            (chibi char-set ascii)))
   (chicken
    ;; (import (posix-extras))
    ;; (import (directory-utils))
    (import (chicken)
            (posix)
            (srfi 14)))
   (foment)
   (gauche
    ;; (import (only (gauche) symlink))
    (import (snow gauche-filesys-utils)
            (gauche fileutil) (file util)
            (srfi 14)))
   (sagittarius
    ;; http://ktakashi.github.io/sagittarius-ref.html#G1146
    (import (sagittarius)
            (srfi 1)
            (util file)
            (srfi 14)))
   (else))

  (begin


;;;============================================================================

;;; System dependencies.


    (cond-expand
     (chibi
      (define (snow-directory-files dir)
        (filter (lambda (ent)
                  (not (or (equal? ent ".")
                           (equal? ent ".."))))
                (directory-files dir))))
     (chicken
      (define (snow-directory-files dir)
        (directory dir #t)))
     (foment)
     (gauche
      (define (snow-directory-files dir)
        (let loop ((filenames (directory-list dir))
                   (result (list)))
          (cond ((null? filenames) (reverse result))
                ((equal? (car filenames) ".") (loop (cdr filenames) result))
                ((equal? (car filenames) "..") (loop (cdr filenames) result))
                (else (loop (cdr filenames) (cons (car filenames) result)))))))
     (sagittarius
      (define (snow-directory-files dir)
        (filter (lambda (ent)
                  (not (or (equal? ent ".")
                           (equal? ent ".."))))
                (read-directory dir)))))


    (define (snow-file-directory? filename)
      (cond-expand
       ((or chibi sagittarius)
        (file-directory? filename))
       (chicken
        (directory? filename))
       (foment)
       (gauche
        (eq? (file-type filename) 'directory))))


    (define (snow-file-regular? filename)
      (cond-expand
       ((or chibi sagittarius)
        (file-regular? filename))
       (chicken
        (regular-file? filename))
       (foment)
       (gauche
        (eq? (file-type filename) 'regular))))


    (define (snow-file-symbolic-link? filename)
      (cond-expand
       (chibi
        (file-link? filename))
       (chicken
        (symbolic-link? filename))
       (foment)
       (gauche
        (eq? (file-type filename :follow-link? #f) 'symlink))
       (sagittarius
        (file-symbolic-link? filename))))


    (define (snow-rename-file orig-filename new-filename)
      (cond-expand
       ((or chibi chicken sagittarius)
        (rename-file orig-filename new-filename))
       (foment)
       (gauche
        (move-file orig-filename new-filename))))


    (define (snow-create-directory dir)
      (cond-expand
       (chibi
        (create-directory* dir))
       ((or chicken sagittarius)
        (create-directory dir))
       (foment)
       (gauche
        (make-directory* dir))))


    (define (snow-delete-directory dir)
      (cond-expand
       ((or chibi chicken sagittarius)
        (delete-directory dir))
       (foment)
       (gauche
        (remove-directory* dir))))


    (define (snow-create-symbolic-link filename linkname)
      (cond-expand
       (chibi
        (or (symbolic-link-file filename linkname)
            (error "snow-create-symbolic-link failed" filename linkname)))
       ((or chicken sagittarius)
        (create-symbolic-link filename linkname))
       (foment)
       (gauche
        (sys-symlink filename linkname))))


    (cond-expand
     ((or chibi chicken foment sagittarius))
     (gauche
      (define current-directory sys-getcwd)))


    (cond-expand
     ((or chibi chicken foment))
     (sagittarius
      (define change-directory current-directory))
     (gauche
      (define change-directory sys-chdir)))


    (cond-expand
     ((or chibi chicken foment gauche)
      (define snow-file-size file-size))
     (sagittarius
      (define snow-file-size file-size-in-bytes)))


    (define (snow-file-mtime filename)
      (cond-expand
       (chibi
        (exact (floor (+ 1262271600 (file-modification-time filename)))))
       (chicken
        (exact (floor (vector-ref (file-stat filename) 8))))
       (foment)
       (gauche
        (exact (floor (file-mtime filename))))
       (sagittarius
        (exact (floor (file-stat-mtime filename))))))



;;;----------------------------------------------------------------------------

    (define (extension-separator) #\.)
    (define (directory-separator) #\/)

    (define (snow-filename-extension-index filename)
      (let ((end (string-length filename)))
        (let loop ((i (- end 1)))
          (if (< i 0)
              end
              (let ((c (string-ref filename i)))
                (cond ((char=? c (extension-separator))
                       i)
                      ((char=? c (directory-separator))
                       end)
                      (else
                       (loop (- i 1)))))))))

    (define (snow-filename-extension filename)
      (substring filename
                 (snow-filename-extension-index filename)
                 (string-length filename)))

    (define (snow-filename-strip-extension filename)
      (substring filename
                 0
                 (snow-filename-extension-index filename)))

    (define (snow-filename-directory-index filename)
      (let ((end (string-length filename)))
        (let loop ((i (- end 1)))
          (if (< i 0)
              0
              (let ((c (string-ref filename i)))
                (cond ((char=? c (directory-separator))
                       (+ i 1))
                      (else
                       (loop (- i 1)))))))))

    (define (snow-filename-directory filename)
      (substring filename
                 0
                 (snow-filename-directory-index filename)))

    (define (snow-filename-strip-directory filename)
      (substring filename
                 (snow-filename-directory-index filename)
                 (string-length filename)))

    (define (snow-filename-strip-trailing-directory-separator filename)
      (let ((end (string-length filename)))
        (if (and (< 0 end)
                 (char=? (string-ref filename (- end 1)) (directory-separator)))
            (substring filename 0 (- end 1))
            filename)))

    (define (snow-make-filename part1 . parts)
      (let loop ((filename part1) (lst parts))
        (cond ((and (pair? lst) (> (string-length (car lst)) 0))
               (loop (string-append filename
                                    (string (directory-separator))
                                    (car lst))
                     (cdr lst)))
              ((pair? lst)
               (loop filename (cdr lst)))
              (else
               filename))))


    (define (snow-combine-filename-parts parts)
      (let loop (
;;                  (result "")
;;                  (parts parts)
                 (result (if (and (pair? parts) (eq? (car parts) '/)) "/" ""))
                 (parts (if (and (pair? parts) (eq? (car parts) '/))
                            (cdr parts) parts))
                 )
        (cond ((null? parts) result)
              ((null? (cdr parts))
               (loop (string-append result (car parts))
                     (cdr parts)))
              (else
               (loop (string-append result
                                    (car parts)
                                    (string (directory-separator)))
                     (cdr parts))))))


    (define (snow-split-filename filename)
      (let loop ((ret (list))
                 (this-part "")
                 (str filename))
        (cond
         ((not str) #f)
         ((= (string-length str) 0)
          (reverse (cons this-part ret)))
         ((eqv? (string-ref str 0) (directory-separator))
          (loop (cons this-part ret) ""
                (substring str 1 (string-length str))))
         (else
          (loop ret
                ;; (string-tack this-part (string-ref str 0))
                (string-append this-part (string (string-ref str 0)))
                (substring str 1 (string-length str)))))))


    (define (snow-filename-relative? filename)
      (not (string-prefix? (string (directory-separator)) filename)))

    ;; (define (snow-make-temp-filename)
    ;;   (let loop ()
    ;;     (let ((filename (snow-u8vector->hex-string (make-random-u8vector 6))))
    ;;       (if (file-exists? filename)
    ;;           (loop)
    ;;           filename))))

    ;; (define* (snow-directory-subfiles filename (types '(regular directory)))


    (define (snow-directory-tree-walk
             filename
             recurse-into-directory?
             consumer)

      (define (list-file file-path-parts)
        (let ((filename (snow-combine-filename-parts file-path-parts)))
          (cond ((snow-file-directory? filename)
                 (if (recurse-into-directory? file-path-parts)
                     (list-dir file-path-parts)))
                (else
                 (consumer file-path-parts)))))

      (define (list-dir dir-path-parts)
        (let loop ((lst (snow-directory-files
                         (snow-combine-filename-parts
                          dir-path-parts))))
          (if (pair? lst)
              (let ((name (car lst)))
                (list-file (append dir-path-parts (list name)))
                (loop (cdr lst))))))

      (list-file (snow-split-filename filename)))



    (define (snow-directory-subfiles filename . maybe-types)
      (let ((types (if (null? maybe-types)
                       '(regular directory)
                       (car maybe-types))))

        (define (list-file filename rev-files)
          (let* ((t
                  (if (snow-file-directory? filename)
                      'directory
                      'regular))
                 (rf
                  (if (memq t types)
                      (cons filename rev-files)
                      rev-files)))
            (if (eq? t 'directory)
                (list-dir filename rf)
                rf)))

        (define (list-dir dir rev-files)
          (let loop ((lst (snow-directory-files dir))
                     (rev-files rev-files))
            (if (pair? lst)
                (let* ((name
                        (car lst))
                       (filename
                        (snow-make-filename dir name)))
                  (loop (cdr lst)
                        (list-file filename rev-files)))
                rev-files)))

        (reverse (list-file filename '()))))

    (define (snow-create-directory-recursive dir)
      (let ((d (snow-filename-strip-trailing-directory-separator dir)))
        (if (not (string=? d dir))
            (snow-create-directory-recursive d)
            (if (not (file-exists? dir))
                (let ((p (snow-filename-directory dir)))
                  (if (not (string=? p dir))
                      (begin
                        (snow-create-directory-recursive p)
                        (snow-create-directory dir))))))))


     ))


;;;============================================================================


