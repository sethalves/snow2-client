(define-library (srfi 29)
  (export format)
  (import (scheme base)
          (scheme char)
          (scheme write))
  (cond-expand
   (chibi
    (import (chibi io)))
   (else))
  (begin


    ;; Basic Format Strings
    ;; http://srfi.schemers.org/srfi-28/srfi-28.html

    ;; Localization (with format)
    ;; http://srfi.schemers.org/srfi-29/srfi-29.html

    ;; Intermediate Format Strings
    ;; http://srfi.schemers.org/srfi-48/srfi-48.html

    ;; slib has format
    ;; http://people.csail.mit.edu/jaffer/slib/Format-Specification.html

    ;; CHICKEN fmt egg
    ;; http://wiki.call-cc.org/eggref/4/fmt


    (cond-expand
     ((or gauche sagittarius chicken))
     ((or chibi foment kawa)

      ;; The association list in which bundles will be stored
      (define *localization-bundles* '())

      ;; The current-language and current-country functions provided
      ;; here must be rewritten for each Scheme system to default to the
      ;; actual locale of the session
      (define current-language
        (let ((current-language-value 'en))
          (lambda args
            (if (null? args)
                current-language-value
                (set! current-language-value (car args))))))

      (define current-country
        (let ((current-country-value 'us))
          (lambda args
            (if (null? args)
                current-country-value
                (set! current-country-value (car args))))))

      ;; The load-bundle! and store-bundle! both return #f in this
      ;; reference implementation.  A compliant implementation need
      ;; not rewrite these procedures.
      ;; (define load-bundle!
      ;;   (lambda (bundle-specifier)
      ;;     #f))

      ;; (define store-bundle!
      ;;   (lambda (bundle-specifier)
      ;;     #f))

      ;; Declare a bundle of templates with a given bundle specifier
      ;; (define declare-bundle!
      ;;   (letrec ((remove-old-bundle
      ;;             (lambda (specifier bundle)
      ;;               (cond ((null? bundle) '())
      ;;                     ((equal? (caar bundle) specifier)
      ;;                      (cdr bundle))
      ;;                     (else (cons (car bundle)
      ;;                                 (remove-old-bundle specifier
      ;;                                                    (cdr bundle))))))))
      ;;     (lambda (bundle-specifier bundle-assoc-list)
      ;;       (set! *localization-bundles*
      ;;             (cons (cons bundle-specifier bundle-assoc-list)
      ;;                   (remove-old-bundle bundle-specifier
      ;;                                      *localization-bundles*))))))

      ;;Retrieve a localized template given its package name and a template name
      (define localized-template
        (letrec ((rdc
                  (lambda (ls)
                    (if (null? (cdr ls))
                        '()
                        (cons (car ls) (rdc (cdr ls))))))
                 (find-bundle
                  (lambda (specifier template-name)
                    (cond ((assoc specifier *localization-bundles*) =>
                           (lambda (bundle) bundle))
                          ((null? specifier) #f)
                          (else (find-bundle (rdc specifier)
                                             template-name))))))
          (lambda (package-name template-name)
            (let loop ((specifier (cons package-name
                                        (list (current-language)
                                              (current-country)))))
              (and (not (null? specifier))
                   (let ((bundle (find-bundle specifier template-name)))
                     (and bundle
                          (cond ((assq template-name bundle) => cdr)
                                ((null? (cdr specifier)) #f)
                                (else (loop (rdc specifier))))))))))))
     (else))

    (cond-expand
     ((or gauche sagittarius))
     ((or chibi chicken foment kawa)


;;An SRFI-28 and SRFI-29 compliant version of format.  It requires
;;SRFI-23 for error reporting.
(define format
  (lambda (format-string . objects)
    (let ((buffer (open-output-string)))
      (let loop ((format-list (string->list format-string))
                 (objects objects)
                 (object-override #f))
        (cond ((null? format-list) (get-output-string buffer))
              ((char=? (car format-list) #\~)
               (cond ((null? (cdr format-list))
                      (error 'format "Incomplete escape sequence"))
                     ((char-numeric? (cadr format-list))
                      (let posloop ((fl (cddr format-list))
                                    (pos (string->number
                                          (string (cadr format-list)))))
                        (cond ((null? fl)
                               (error 'format "Incomplete escape sequence"))
                              ((and (eq? (car fl) '#\@)
                                    (null? (cdr fl)))
                                    (error 'format "Incomplete escape sequence"))
                              ((and (eq? (car fl) '#\@)
                                    (eq? (cadr fl) '#\*))
                               (loop (cddr fl) objects (list-ref objects pos)))
                              (else
                                (posloop (cdr fl)
                                         (+ (* 10 pos)
                                            (string->number
                                             (string (car fl)))))))))
                     (else
                       (case (cadr format-list)
                         ((#\a)
                          (cond (object-override
                                 (begin
                                   (display object-override buffer)
                                   (loop (cddr format-list) objects #f)))
                                ((null? objects)
                                 (error 'format "No value for escape sequence"))
                                (else
                                  (begin
                                    (display (car objects) buffer)
                                    (loop (cddr format-list)
                                          (cdr objects) #f)))))
                         ((#\s)
                          (cond (object-override
                                 (begin
                                   (display object-override buffer)
                                   (loop (cddr format-list) objects #f)))
                                ((null? objects)
                                 (error 'format "No value for escape sequence"))
                                (else
                                  (begin
                                    (write (car objects) buffer)
                                    (loop (cddr format-list)
                                          (cdr objects) #f)))))
                         ((#\%)
                          (if object-override
                              (error 'format "Escape sequence following positional override does not require a value"))
                          (display #\newline buffer)
                          (loop (cddr format-list) objects #f))
                        ((#\~)
                          (if object-override
                              (error 'format "Escape sequence following positional override does not require a value"))
                          (display #\~ buffer)
                          (loop (cddr format-list) objects #f))
                         (else
                           (error 'format "Unrecognized escape sequence"))))))
              (else (display (car format-list) buffer)
                    (loop (cdr format-list) objects #f)))))))


;; ;; adapted from srfi-48 -- allow first parameter to be port or boolean
;; (define (format . args)
;;   (cond
;;    ((null? args)
;;     (error "FORMAT: required format-string argument is missing"))
;;    ((string? (car args))
;;     (apply format~ args))
;;    ((< (length args) 2)
;;     (error (format #f "FORMAT: too few arguments ~s" (cons 'format args))))
;;    ((eq? (car args) #f)
;;     (apply format~ (cdr args)))
;;    (else
;;     (let* ((output-port (car args))
;;            (args (cdr args))
;;            (port (cond ((output-port? output-port) output-port)
;;                        ((eq? output-port #t) (current-output-port))
;;                        (else (error
;;                               (format #f "FORMAT: bad output-port argument: ~s"
;;                                       output-port))))))
;;       (display (apply format~ args) port)
;;       #t))))

))))
