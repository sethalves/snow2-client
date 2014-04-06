;;;============================================================================

;;; File: "snowlib.scm", Time-stamp: <2007-09-03 12:21:22 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Runtime library for Scheme Now! package system.


(define-library (snow snowlib)
  (export snow-cleanup
          snow-cleanup-handler-push!
          snow-raise
          snow-error
          snow-display-error
          snow-expect
          snow-with-exception-catcher
          snow-keyword?
          snow-keyword->string
          snow-string->keyword
          ;; snow-record?
          ;; snow-vector?
          ;; snow-record-rtd
          ;; snow-indirect-instance-of?
          ;; snow-direct-instance-of?
          ;; snow-field-ref-error
          ;; snow-field-set!-error
          snow-get-undefined
          snow-req-key
          snow-opt-key
          snow-process-keys
          make-snow-condition
          snow-condition?
          make-type-check-condition
          type-check-condition?
          type-check-condition-type-expected
          make-snow-error-condition
          snow-error-condition?
          snow-error-condition-msg
          snow-error-condition-args

          make-snow-cond
          snow-cond?
          snow-cond-type
          set-snow-cond-type!
          snow-cond-fields
          set-snow-cond-fields!


          )
  (import (scheme base) (scheme write))
  (cond-expand
   (chibi)
   (chicken (import (only (chicken) handle-exceptions)))
   (gauche)
   (sagittarius))
  (begin

;;;============================================================================

;;; System dependencies.

    (cond-expand

     (else

      (define-record-type <snow-cond>
        (make-snow-cond type fields)
        snow-cond?
        (type snow-cond-type set-snow-cond-type!)
        (fields snow-cond-fields set-snow-cond-fields!)
        )


      (define (make-snow-condition)
        (make-snow-cond '()
                        '#()))

      (define (snow-condition? obj)
        (snow-cond? obj))

      (define (make-type-check-condition type-expected)
        (make-snow-condition '(type-check-condition)
                             (vector type-expected)))

      (define (type-check-condition? obj)
        (and (snow-condition? obj)
             (memq 'type-check-condition (snow-cond-type obj))))

      (define (type-check-condition-type-expected cnd)
        (vector-ref (snow-cond-fields cnd) 0))

      (define (make-snow-error-condition msg args)
        (make-snow-cond '(error-condition)
                        (vector msg args)))

      (define (snow-error-condition? obj)
        (and (snow-cond? obj)
             (memq 'error-condition (snow-cond-type obj))))

      (define (snow-error-condition-msg cnd)
        (vector-ref (snow-cond-fields cnd) 0))

      (define (snow-error-condition-args cnd)
        (vector-ref (snow-cond-fields cnd) 1))))

    (cond-expand

     (bigloo

      (define (snow-raise exc)
        (raise exc))

      (define (snow-with-exception-catcher catcher thunk)
        (with-handler catcher (thunk))))

     (chez

      (define exception-error-msg
        (string-append "uncaught" " exception"))

      (define (snow-raise exc)
        (error 'snow-raise exception-error-msg exc))

      (define (snow-with-exception-catcher catcher thunk)
        ((call-with-current-continuation
          (lambda (unwind-and-call)
            (lambda ()
              (parameterize ((error-handler
                              (lambda (who msg . args)
                                (let ((exc
                                       (if (and (eq? who 'snow-raise)
                                                (eq? msg exception-error-msg)
                                                (pair? args))
                                           (car args)
                                           (cons who (cons msg args)))))
                                  (unwind-and-call (lambda ()
                                                     (catcher exc)))))))
                            (call-with-values
                                thunk
                              (lambda results
                                (unwind-and-call
                                 (lambda () (apply values results))))))))))))

     (chibi
      (define (snow-raise exc)
        (error exc))
      (define (snow-with-exception-catcher catcher thunk)
        (guard (condition (else (catcher condition)))
               (thunk))))

     (chicken

      (define (snow-raise exc)
        ;; (signal exc)
        (error exc))

      (define (snow-with-exception-catcher catcher thunk)
        (handle-exceptions exn (catcher exn) (thunk)))

      ;; (define (snow-with-exception-catcher catcher thunk)
      ;;   ((call-with-current-continuation
      ;;     (lambda (unwind-and-call)
      ;;       (lambda ()
      ;;         (with-exception-handler
      ;;          (lambda (exc)
      ;;            (unwind-and-call
      ;;             (lambda ()
      ;;               (catcher
      ;;                ((condition-property-accessor 'exn 'message) exc)))))
      ;;          (lambda ()
      ;;            (call-with-values
      ;;                thunk
      ;;              (lambda results
      ;;                (unwind-and-call
      ;;                 (lambda () (apply values results))))))))))))
      )

     (guile

      (define (snow-raise exc)
        (throw 'raise exc))

      (define (snow-with-exception-catcher catcher thunk)
        (catch #t thunk (lambda (key . exc)
                          (catcher (if (pair? exc) (car exc) key))))))

     (kawa

      (define-simple-class <snow-exception> (<java.lang.Throwable>)
        (snow-exc init-keyword: snow-exc:))

      (define (snow-raise exc)
        (primitive-throw (make <snow-exception> snow-exc: exc)))

      (define (snow-with-exception-catcher-aux catcher thunk)
        (try-catch (thunk)
                   (e <java.lang.Throwable>
                      (catcher
                       (if (<snow-exception>:instance? e)
                           e:snow-exc
                           e)))))

      (define (snow-with-exception-catcher catcher thunk)
        (snow-with-exception-catcher-aux catcher thunk)))

     (larceny

      (define* (snow-raise exc)
        ((error-handler) 'snow-raise-errcode exc))

      (define* (snow-with-exception-catcher catcher thunk)
        ((call-with-current-continuation
          (lambda (unwind-and-call)
            (lambda ()
              (let ((orig-error-handler (error-handler)))
                (call-with-error-handler
                 (lambda (code . args)
                   '(begin (display `(snow-with-exception-catcher..error-handler ,code ,@args))
                           (newline))
                   (case code
                     ((snow-raise-errcode)
                      (unwind-and-call (lambda () (catcher (car args)))))
                     (else
                      (apply orig-error-handler code args))))
                 (lambda ()
                   (call-with-values thunk
                     (lambda results
                       (unwind-and-call
                        (lambda () (apply values results))))))))))))))

     (mit

      (define _snow:condition-type
        (make-condition-type 'snow-exception condition-type:error '(exc)
                             (lambda (condition port)
                               condition
                               (write-string "Snow exception." port))))

      (define (snow-raise exc)
        (call-with-current-continuation
         (lambda (k)
           (signal-condition
            (make-condition
             _snow:condition-type
             k
             '()
             (list 'exc exc))))))

      (define (snow-with-exception-catcher catcher thunk)
        ((call-with-current-continuation
          (lambda (unwind-and-call)
            (lambda ()
              (bind-condition-handler
               '()
               (lambda (condition)
                 (let ((exc
                        (if (eq? (condition/type condition) _snow:condition-type)
                            (access-condition condition 'exc)
                            condition)))
                   (unwind-and-call (lambda () (catcher exc)))))
               (lambda ()
                 (let ((results (thunk)))
                   (unwind-and-call
                    (lambda () results)))))))))))

     (mzscheme

      (define (snow-raise exc)
        (raise exc))

      (define (snow-with-exception-catcher catcher thunk)
        (with-handlers
         (((lambda (exc) #t)
           (lambda (exc) (catcher exc))))
         (thunk))))

     (scheme48

      (define-condition-type 'snow-except '())

      (define (snow-raise exc)
        (signal 'snow-except exc))

      (define (snow-with-exception-catcher catcher thunk)
        ((call-with-current-continuation
          (lambda (unwind-and-call)
            (lambda ()
              (with-handler
               (lambda (c propagate)
                 (unwind-and-call
                  (lambda ()
                    (catcher (if (and (eq? (condition-type c) 'snow-except)
                                      (pair? (condition-stuff c)))
                                 (car (condition-stuff c))
                                 c)))))
               (lambda ()
                 (call-with-values
                     thunk
                   (lambda results
                     (unwind-and-call
                      (lambda () (apply values results)))))))))))))

     (scm

      (define *current-exception-handler*
        (lambda (exc)
          (display "uncaught exception ")
          (write exc)
          (newline)
          (exit 1)))

      (define (snow-raise exc)
        (*current-exception-handler* exc))

      (define (snow-with-exception-catcher catcher thunk)
        ((call-with-current-continuation
          (lambda (unwind-and-call)
            (lambda ()
              (fluid-let ((*current-exception-handler*
                           (lambda (exc)
                             (unwind-and-call (lambda () (catcher exc))))))
                (call-with-values
                    thunk
                  (lambda results
                    (unwind-and-call
                     (lambda () (apply values results))))))))))))

     (scsh

      (define (snow-raise exc)
        (error 'snow-except exc))

      (define (snow-with-exception-catcher catcher thunk)
        ((call-with-current-continuation
          (lambda (unwind-and-call)
            (lambda ()
              (with-handler
               (lambda (exc next)
                 (unwind-and-call
                  (lambda ()
                    (catcher
                     (if (and (pair? exc)
                              (eq? (car exc) 'error)
                              (pair? (cdr exc))
                              (eq? (cadr exc) 'snow-except)
                              (pair? (cddr exc)))
                         (caddr exc)
                         exc)))))
               (lambda ()
                 (call-with-values
                     thunk
                   (lambda results
                     (unwind-and-call
                      (lambda () (apply values results)))))))))))))

     (sisc

      (define _snow:exception-tag (list 'snow-exception))

      (define (snow-raise exc)
        (throw (make-error (list _snow:exception-tag exc))))

      (define (snow-with-exception-catcher catcher thunk)
        (with-failure-continuation
         (lambda (error-record error-k)
           (let ((msg (error-message error-record)))
             (if (and (pair? msg)
                      (eq? (car msg) _snow:exception-tag))
                 (catcher (cadr msg))
                 (catcher error-record))))
         thunk)))

     (else

      (define (snow-raise exc)
        (raise exc))

      (define (snow-with-exception-catcher catcher thunk)
        ((call-with-current-continuation
          (lambda (unwind-and-call)
            (lambda ()
              (with-exception-handler
               (lambda (exc)
                 (unwind-and-call (lambda () (catcher exc))))
               (lambda ()
                 (call-with-values
                     thunk
                   (lambda results
                     (unwind-and-call
                      (lambda () (apply values results))))))))))))))

    ;; Record operations.

    ;; (define (snow-record? obj) ;; deprecated
    ;;   #f)

    ;; (cond-expand

    ;;  ((or bigloo
    ;;       chez
    ;;       chicken
    ;;       gambit
    ;;       gauche
    ;;       guile
    ;;       kawa
    ;;       mzscheme
    ;;       scheme48
    ;;       ;;      scm
    ;;       scsh
    ;;       stklos)

    ;;   (define (snow-vector? obj)
    ;;     (vector? obj))

    ;;   (define (snow-record-rtd rec) #f)
    ;;   (define (snow-indirect-instance-of? obj rtd) #f)
    ;;   (define (snow-direct-instance-of? obj rtd) #f)
    ;;   (define (snow-field-ref-error obj i rtd) #f)
    ;;   (define (snow-field-set!-error obj val i rtd) #f))

    ;;  (else

    ;;   (define (snow-vector? obj)
    ;;     (and (vector? obj)
    ;;          (not (snow-indirect-instance-of? obj (snow-record)))))

    ;;   (define (snow-record-rtd rec)
    ;;     (vector-ref rec 0))

    ;;   (define-macro (rtd-size) 2) ;; size of a record type descriptor

    ;;   (define (snow-indirect-instance-of? obj rtd)
    ;;     (and (vector? obj)
    ;;          (>= (vector-length obj) 1)
    ;;          (let loop ((x (vector-ref obj 0))) ;; get record's rtd
    ;;            (and (vector? x)
    ;;                 (= (vector-length x) (rtd-size))
    ;;                 (or (eq? (vector-ref x 1) ;; compare uid fields
    ;;                          (vector-ref rtd 1))
    ;;                     (loop (vector-ref x 0)))))))

    ;;   (define (snow-direct-instance-of? obj rtd)
    ;;     (and (vector? obj)
    ;;          (>= (vector-length obj) 1)
    ;;          (let ((x (vector-ref obj 0))) ;; get record's rtd
    ;;            (and (vector? x)
    ;;                 (= (vector-length x) (rtd-size))
    ;;                 (eq? (vector-ref x 1) ;; compare uid fields
    ;;                      (vector-ref rtd 1))))))

    ;;   (define (snow-field-ref-error obj i rtd)
    ;;     (snow-raise (make-type-check-condition rtd)))

    ;;   (define (snow-field-set!-error obj val i rtd)
    ;;     (snow-raise (make-type-check-condition rtd)))))

    ;; Keywords.

    (cond-expand

     ((or bigloo
          ;; chicken ;; can't yet use keyword macros from r7rs
          gambit
          kawa
          mzscheme
          sagittarius)

      (define (snow-keyword? obj)
        (keyword? obj))

      (define (snow-keyword->string k)
        (keyword->string k))

      (define (snow-string->keyword s)
        (string->keyword s)))

     ((or chez
          chibi
          chicken ;; can't yet use keyword macros from r7rs
          larceny
          mit
          rscheme
          scheme48
          scm
          scsh
          sisc)

      (define (snow-keyword? obj)
        (and (symbol? obj)
             (let* ((s (symbol->string obj))
                    (n (string-length s)))
               (and (>= n 1)
                    (char=? (string-ref s (- n 1)) #\:))))) ;; add a colon

      (define (snow-keyword->string k)
        (let* ((s (symbol->string k))
               (n (string-length s)))
          (substring s 0 (- n 1)))) ;; remove the colon

      (define (snow-string->keyword s)
        (string->symbol (string-append s ":"))))

     ((or gauche
          stklos)

      (define (snow-keyword? obj)
        (keyword? obj))

      (define (snow-keyword->string k)
        (keyword->string k))

      (define (snow-string->keyword s)
        (make-keyword s)))

     (guile

      (define (snow-keyword? obj)
        (keyword? obj))

      (define (snow-string->keyword s)
        (symbol->keyword (string->symbol s)))

      (define (snow-keyword->string k)
        (symbol->string (keyword->symbol k)))))

;;;----------------------------------------------------------------------------

    (define (snow-cleanup)
      ;; (_snow:cleanup)
      #f ;; XXX
      )

    (define (snow-cleanup-handler-push! thunk)
      ;; (_snow:cleanup-handler-push! thunk)
      #f ;; XXX
      )

;;;----------------------------------------------------------------------------

    (define (snow-error msg . args)

      ;; (display "*** SNOW ERROR -- ")
      ;; (display msg)
      ;; (for-each (lambda (x) (display " ") (write x)) args)
      ;; (newline)

      (snow-raise (make-snow-error-condition msg args)))


    (define (snow-display-error err)
      (cond
       ((snow-error-condition? err)
        (display "Error (snow-error-condition) -- " (current-error-port))
        (display (snow-error-condition-msg err) (current-error-port))
        (newline (current-error-port)))
       ((snow-condition? err)
        (display "Error (snow-condition) -- " (current-error-port))
        (display (snow-cond-fields err) (current-error-port))
        (newline (current-error-port)))
       (else
        (display "Error -- " (current-error-port))
        (write err (current-error-port))
        (newline (current-error-port))))
      err)

;;;----------------------------------------------------------------------------

    (define snow-expect-nb-tests 0)
    (define snow-expect-failed-tests '())

    (snow-cleanup-handler-push!
     (lambda ()
       (if (> snow-expect-nb-tests 0)
           (let ((nb-failed (length snow-expect-failed-tests)))
             (display "*** SNOW TESTS: ")
             (if (> nb-failed 0)
                 (begin
                   (display "failed ")
                   (display nb-failed)
                   (if (> nb-failed 1)
                       (display " tests")
                       (display " test"))
                   (display " out of "))
                 (display "passed all "))
             (display snow-expect-nb-tests)
             (if (> snow-expect-nb-tests 1)
                 (display " tests.")
                 (display " test."))
             (newline)
             (for-each
              (lambda (msg)
                (display "*** ")
                (write msg)
                (newline))
              (reverse snow-expect-failed-tests))))))

    (define (snow-expect expr thunk)

      (define (fail msg)
        (set! snow-expect-failed-tests
              (cons msg snow-expect-failed-tests)))

      (set! snow-expect-nb-tests (+ 1 snow-expect-nb-tests))

      (snow-with-exception-catcher
       (lambda (exc)
         (fail (list expr '=> 'raised exc))
         #f)
       (lambda ()
         (let ((result (thunk)))
           (if (not result)
               (fail (list expr '=> result)))
           result))))

;;;----------------------------------------------------------------------------

;;; These procedures are needed at run time.  They are called from the
;;; code produced by the macros defined in base/snow.scm .

    ;; Perfect hash-tables with keyword keys.

    (define (perfect-hash-table-lookup table key)

      (define (hash-keyword key n)
        (let ((str (snow-keyword->string key)))
          (let loop ((h 0) (i 0))
            (if (< i (string-length str))
                (loop (modulo (+ (* h 65536) (char->integer (string-ref str i)))
                              n)
                      (+ i 1))
                h))))

      (let* ((n (quotient (vector-length table) 2))
             (x (* 2 (hash-keyword key n))))
        (and (eq? (vector-ref table x) key)
             (vector-ref table (+ x 1)))))

    ;; Handling of named parameters.

    (define snow-undefined (list 'undefined))

    (define (snow-get-undefined)
      snow-undefined)

    (define (snow-req-key key-values i)
      (let ((val (vector-ref key-values i)))
        (if (eq? val (snow-get-undefined))
            (snow-error "a required named parameter was not provided")
            val)))

    (define (snow-opt-key key-values i default)
      (let ((val (vector-ref key-values i)))
        (if (eq? val (snow-get-undefined))
            (default)
            val)))

    (define (snow-process-keys args key-hash-table key-values)
      (let loop ((args args))
        (if (null? args)
            args
            (let ((k (car args)))
              (if (not (snow-keyword? k))
                  args
                  (let ((i (perfect-hash-table-lookup key-hash-table k)))
                    (if (not i)
                        (snow-error
                         "unknown parameter keyword"
                         k)
                        (if (null? (cdr args))
                            (snow-error
                             "a value was expected after keyword"
                             k)
                            (begin
                              (if (eq? (vector-ref key-values i)
                                       (snow-get-undefined))
                                  (vector-set! key-values i (cadr args))
                                  (snow-error
                                   "duplicate parameter"
                                   k))
                              (loop (cddr args)))))))))))

;;;============================================================================

    ))