(define-library (snow extio)
  (export snow-read-string
          snow-force-output
          snow-pretty-print
          make-delimited-input-port
          binary-port->textual-port
          textual-port->binary-port
          ;; read-line
          snow-port-position
          snow-set-port-position!
          snow-set-port-position-from-current!
          snow-set-port-position-from-end!
          )
  (import (scheme base)
          (scheme write))
  (cond-expand
   (chibi (import (chibi io)))
   (chicken (import (only (chicken) flush-output pretty-print)
                    (only (posix) file-position set-file-position!
                          seek/cur seek/end)
                    (ports)))
   (gauche (import (snow gauche-extio-utils)))
   (sagittarius (import
                 (only (rnrs)
                       port-position
                       set-port-position!
                       make-custom-binary-input-port
                       make-custom-textual-input-port
                       transcoded-port
                       make-transcoder
                       utf-8-codec
                       latin-1-codec
                       eol-style)))
   (foment (import (only (foment base) port-position set-port-position!)))
   )
  (import (snow bytevector)
          (srfi 60)
          (srfi 13)
          )
  (begin

    (cond-expand

     ;; snow-read-string is like r7rs read-string except that
     ;; if the length is #f, it will read until eof (like CHICKEN's).

     ((or chicken)
      (define snow-read-string read-string))

     ((or chibi foment gauche sagittarius)
      (define (read-string-until-eof port)
        (let loop ((strings '()))
          (let ((s (read-string 4000 port)))
            (cond ((eof-object? s)
                   (string-concatenate-reverse strings))
                  (else
                   (loop (cons s strings)))))))

      (define (snow-read-string len port)
        (if (not len)
            (read-string-until-eof port)
            (read-string len port))))
     )


    (define (snow-force-output . maybe-port)
      (let ((port (if (null? maybe-port) (current-output-port)
                      (car maybe-port))))
        (flush-output-port port)))


    ;; (cond-expand

    ;;  ((or bigloo
    ;;       chibi
    ;;       larceny
    ;;       chez
    ;;       sagittarius
    ;;       sisc
    ;;       stklos)

    ;;   (define (snow-force-output . maybe-port)
    ;;     (let ((port (if (null? maybe-port) (current-output-port)
    ;;                     (car maybe-port))))
    ;;       (flush-output-port port))))

    ;;  ((or gambit
    ;;       guile
    ;;       kawa
    ;;       scheme48
    ;;       scm
    ;;       scsh)

    ;;   (define (snow-force-output . maybe-port)
    ;;     (let ((port (if (null? maybe-port) (current-output-port)
    ;;                     (car maybe-port))))
    ;;       (force-output port))))

    ;;  ((or chicken)

    ;;   (define (snow-force-output . maybe-port)
    ;;     (let ((port (if (null? maybe-port) (current-output-port)
    ;;                     (car maybe-port))))
    ;;       (flush-output port))))

    ;;  (gauche
    ;;   ;; See gauche-extio-utils.sld
    ;;   ;; this was done to get access to flush
    ;;   ;; (define (snow-force-output . maybe-port)
    ;;   ;;   (let ((port (if (null? maybe-port) (current-output-port)
    ;;   ;;                   (car maybe-port))))
    ;;   ;;     (flush port)))
    ;;   )
    ;;  )




    ;; trimmed down version of:
      ;;; Pretty print:
    ;;
    ;; Copyright (c) 1991, Marc Feeley
    ;; Author: Marc Feeley (feeley@iro.umontreal.ca)
    ;; Distribution restrictions: none
    ;;
    ;; Modified by felix for use with CHICKEN
    ;;

    ;; (define pretty-print-width (make-parameter 79))
    (define (pretty-print-width) 79)

    (define (reverse-string-append l) ;; from CHICKEN data-structures.scm
      (define (rev-string-append l i)
        (if (pair? l)
            (let* ((str (car l))
                   (len (string-length str))
                   (result (rev-string-append (cdr l) (+ i len))))
              (let loop ((j 0) (k (- (- (string-length result) i) len)))
                (if (< j len)
                    (begin
                      (string-set! result k (string-ref str j))
                      (loop (+ j 1) (+ k 1)))
                    result)))
            (make-string i)))
      (rev-string-append l 0))

    (define generic-write
      (lambda (obj display? width output)

        (define (read-macro? l)
          (define (length1? l) (and (pair? l) (null? (cdr l))))
          (let ((head (car l)) (tail (cdr l)))
            (case head
              ((quote quasiquote unquote unquote-splicing) (length1? tail))
              (else                                        #f))))

        (define (read-macro-body l)
          (cadr l))

        (define (read-macro-prefix l)
          (let ((head (car l)) (tail (cdr l)))
            (case head
              ((quote)            "'")
              ((quasiquote)       "`")
              ((unquote)          ",")
              ((unquote-splicing) ",@"))))

        (define (out str col)
          (and col (output str) (+ col (string-length str))))

        (define (wr obj col)

          (define (wr-expr expr col)
            (wr-lst expr col))

          (define (wr-lst l col)
            (if (pair? l)
                (let loop ((l (cdr l))
                           (col (and col (wr (car l) (out "(" col)))))
                  (cond ((not col) col)
                        ((pair? l)
                         (loop (cdr l) (wr (car l) (out " " col))))
                        ((null? l) (out ")" col))
                        (else      (out ")" (wr l (out " . " col))))))
                (out "()" col)))

          (cond ((pair? obj)        (wr-expr obj col))
                ((null? obj)        (wr-lst obj col))
                ((eof-object? obj)  (out "#!eof" col))
                ((vector? obj)      (wr-lst (vector->list obj) (out "#" col)))
                ((boolean? obj)     (out (if obj "#t" "#f") col))
                ((number? obj)      (out (number->string obj) col))
                ((or (symbol? obj)
                     (string? obj)
                     (char? obj))
                 (let ((s (open-output-string)))
                   (write obj s)
                   (out (get-output-string s) col)))
                (else (out "#<unprintable object>" col)) ) )

        (define (pp obj col)

          (define (spaces n col)
            (if (> n 0)
                (if (> n 7)
                    (spaces (- n 8) (out "        " col))
                    (out (substring "        " 0 n) col))
                col))

          (define (indent to col)
            (and col
                 (if (< to col)
                     (and (out (make-string 1 #\newline) col) (spaces to 0))
                     (spaces (- to col) col))))

          (define (pr obj col extra pp-pair)
            (if (or (pair? obj) (vector? obj))
                ;; may have to split on multiple lines
                (let ((result '())
                      (left (max (+ (- (- width col) extra) 1)
                                 max-expr-width)))
                  (generic-write obj display? #f
                                 (lambda (str)
                                   (set! result (cons str result))
                                   (set! left (- left (string-length str)))
                                   (> left 0)))
                  (if (> left 0) ;; all can be printed on one line
                      (out (reverse-string-append result) col)
                      (if (pair? obj)
                          (pp-pair obj col extra)
                          (pp-list (vector->list obj)
                                   (out "#" col) extra pp-expr))))
                (wr obj col)))

          (define (pp-expr expr col extra)
            (if (read-macro? expr)
                (pr (read-macro-body expr)
                    (out (read-macro-prefix expr) col)
                    extra
                    pp-expr)
                (let ((head (car expr)))
                  (if (symbol? head)
                      (let ((proc (style head)))
                        (if proc
                            (proc expr col extra)
                            (if (> (string-length (symbol->string head))
                                   max-call-head-width)
                                (pp-general expr col extra #f #f #f pp-expr)
                                (pp-call expr col extra pp-expr))))
                      (pp-list expr col extra pp-expr)))))

          ;; (head item1
          ;;       item2
          ;;       item3)
          (define (pp-call expr col extra pp-item)
            (let ((col* (wr (car expr) (out "(" col))))
              (and col
                   (pp-down (cdr expr) col* (+ col* 1) extra pp-item))))

          ;; (item1
          ;;  item2
          ;;  item3)
          (define (pp-list l col extra pp-item)
            (let ((col (out "(" col)))
              (pp-down l col col extra pp-item)))

          (define (pp-down l col1 col2 extra pp-item)
            (let loop ((l l) (col col1))
              (and col
                   (cond ((pair? l)
                          (let ((rest (cdr l)))
                            (let ((extra (if (null? rest) (+ extra 1) 0)))
                              (loop rest
                                    (pr (car l)
                                        (indent col2 col)
                                        extra pp-item)))))
                         ((null? l)
                          (out ")" col))
                         (else
                          (out ")"
                               (pr l
                                   (indent col2 (out "." (indent col2 col)))
                                   (+ extra 1)
                                   pp-item)))))))

          (define (pp-general expr col extra named? pp-1 pp-2 pp-3)

            (define (tail1 rest col1 col2 col3)
              (if (and pp-1 (pair? rest))
                  (let* ((val1 (car rest))
                         (rest (cdr rest))
                         (extra (if (null? rest) (+ extra 1) 0)))
                    (tail2 rest col1
                           (pr val1 (indent col3 col2) extra pp-1)
                           col3))
                  (tail2 rest col1 col2 col3)))

            (define (tail2 rest col1 col2 col3)
              (if (and pp-2 (pair? rest))
                  (let* ((val1 (car rest))
                         (rest (cdr rest))
                         (extra (if (null? rest) (+ extra 1) 0)))
                    (tail3 rest col1
                           (pr val1 (indent col3 col2) extra pp-2)))
                  (tail3 rest col1 col2)))

            (define (tail3 rest col1 col2)
              (pp-down rest col2 col1 extra pp-3))

            (let* ((head (car expr))
                   (rest (cdr expr))
                   (col* (wr head (out "(" col))))
              (if (and named? (pair? rest))
                  (let* ((name (car rest))
                         (rest (cdr rest))
                         (col** (wr name (out " " col*))))
                    (tail1 rest (+ col indent-general) col** (+ col** 1)))
                  (tail1 rest (+ col indent-general) col* (+ col* 1)))))

          (define (pp-expr-list l col extra)
            (pp-list l col extra pp-expr))

          (define (pp-lambda expr col extra)
            (pp-general expr col extra #f pp-expr-list #f pp-expr))

          (define (pp-if expr col extra)
            (pp-general expr col extra #f pp-expr #f pp-expr))

          (define (pp-cond expr col extra)
            (pp-call expr col extra pp-expr-list))

          (define (pp-case expr col extra)
            (pp-general expr col extra #f pp-expr #f pp-expr-list))

          (define (pp-and expr col extra)
            (pp-call expr col extra pp-expr))

          (define (pp-let expr col extra)
            (let* ((rest (cdr expr))
                   (named? (and (pair? rest) (symbol? (car rest)))))
              (pp-general expr col extra named? pp-expr-list #f pp-expr)))

          (define (pp-begin expr col extra)
            (pp-general expr col extra #f #f #f pp-expr))

          (define (pp-do expr col extra)
            (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

          ;; define formatting style (change these to suit your style)

          (define indent-general 2)

          (define max-call-head-width 5)

          (define max-expr-width 50)

          (define (style head)
            (case head
              ((lambda let* letrec letrec* define) pp-lambda)
              ((if set!)                   pp-if)
              ((cond)                      pp-cond)
              ((case)                      pp-case)
              ((and or)                    pp-and)
              ((let)                       pp-let)
              ((begin)                     pp-begin)
              ((do)                        pp-do)
              (else                        #f)))

          (pr obj col 0 pp-expr))

        (if width
            (out (make-string 1 #\newline) (pp obj 0))
            (wr obj 0))))

    ;; (pretty-print obj port) pretty prints 'obj' on 'port'.  The current
    ;; output port is used if 'port' is not specified.

    (define (snow-pretty-print obj . opt)
      (let ((port (if (pair? opt) (car opt) (current-output-port))))
        (generic-write obj #f
                       (pretty-print-width)
                       (lambda (s) (display s port) #t))))



    ;;
    ;; break off some number of characters from a port and then
    ;; generate an eof.  the original port will be positioned
    ;; just after len characters.  reading from original port
    ;; before the delimited port will mess it all up.
    ;;
    (cond-expand


     (chicken
      (define (make-delimited-input-port port len)
        (let ((index 0)
              (saw-eof #f))
          (make-input-port
           (lambda () ; read-char
             (cond ((= index len) (eof-object))
                   (saw-eof (eof-object))
                   (else
                    (let ((c (read-char port)))
                      (cond ((eof-object? c) (set! saw-eof #t))
                            (else (set! index (+ index 1))))
                      c))))
           (lambda () ; char-ready?
             (cond ((= index len) #f)
                   (saw-eof #f)
                   (else (char-ready? port))))
           (lambda () #t) ; close
           (lambda () ; peek-char
             (if (= index len)
                 (eof-object)
                 (peek-char port)))))))

     (chibi
      (define (make-delimited-textual-input-port port len)
        (let ((index 0))
          (make-custom-input-port
           (lambda (str start end)
             (cond ((= index len) 0)
                   (else
                    (let ((c (read-char port)))
                      (cond ((eof-object? c) c)
                            (else (set! index (+ index 1))
                                  (string-set! str start c)
                                  ;; XXX ?
                                  (bytevector-length
                                   (string->utf8 (string c))))))))))))


      (define (make-delimited-binary-input-port port len)
        (let ((index 0))
          (make-custom-binary-input-port
           (lambda (bv start end)
             (cond ((= index len) 0)
                   (else
                    (let* ((plan-to-read (- len index))
                           (plan-to-read (if (< (- end start) plan-to-read)
                                             (- end start) plan-to-read))
                           (did-read
                            (read-bytevector!
                             bv port start (+ start plan-to-read))))
                      (cond ((eof-object? did-read) 0)
                            (else
                             (set! index (+ index did-read))
                             did-read)))))))))


      (define (make-delimited-input-port port len)
        (cond ((binary-port? port)
               (make-delimited-binary-input-port port len))
              (else
               (make-delimited-textual-input-port port len)))))

     (gauche
      (define (make-delimited-input-port port len)
        (let ((index 0)
              (saw-eof #f))
          (make-virutal-input-port
           :getb (lambda ()
                   (cond ((= index len) (eof-object))
                         (saw-eof (eof-object))
                         (else
                          (let ((c (read-u8 port)))
                            (cond ((eof-object? c)
                                   (set! saw-eof #t)
                                   c)
                                  (else
                                   (set! index (+ index 1))
                                   c))))))
           :getc (lambda ()
                   (cond ((= index len) (eof-object))
                         (saw-eof (eof-object))
                         (else
                          (let ((c (read-u8 port)))
                            (cond ((eof-object? c)
                                   (set! saw-eof #t)
                                   c)
                                  (else
                                   (set! index (+ index 1))
                                   (integer->char c)))))))
           :ready (lambda (t-for-char-f-for-byte)
                    (if t-for-char-f-for-byte
                        (char-ready? port)
                        (u8-ready? port)))
           :close (lambda () #t)
           ))))


     (sagittarius
      (define (make-delimited-textual-input-port port len)
        (let ((index 0))
          (make-custom-textual-input-port
           (string-append "delmited textual port " (number->string len))
           (lambda (str start count)
             (cond ((= index len) 0)
                   (else
                    (let ((c (read-char port)))
                      (cond ((eof-object? c) c)
                            (else (set! index (+ index 1))
                                  (string-set! str start c)
                                  1))))))
           #f ;; get-position
           #f ;; set-position!
           #f ;; close
           (lambda () (char-ready? port)) ;; ready
           )))


      (define (make-delimited-binary-input-port port len)
        (let ((index 0))
          (make-custom-binary-input-port
           (string-append "delmited binary port " (number->string len))
           (lambda (bv start count)
             (cond ((= index len) 0)
                   (else
                    (let* ((plan-to-read (- len index))
                           (plan-to-read
                            (if (< count plan-to-read) count plan-to-read))
                           (did-read
                            (read-bytevector! bv port start
                                              (+ start plan-to-read))))
                      (cond ((eof-object? did-read) 0)
                            (else
                             (set! index (+ index did-read))
                             did-read))))))
           #f ;; get-position
           #f ;; set-position!
           #f ;; close
           (lambda () (u8-ready? port)) ;; ready
           )))

      (define (make-delimited-input-port port len)
        (cond ((binary-port? port)
               (make-delimited-binary-input-port port len))
              (else
               (make-delimited-textual-input-port port len)))))

     (else
      ;; for schemes with no procedural ports
      (define (make-delimited-binary-input-port port len)
        (let loop ((i 0)
                   (segments '()))
          (define (return-result)
            (open-input-bytevector
             (reverse-bytevector-list->bytevector segments)))
          (if (= i len)
              (return-result)
              (let* ((to-read (if (> (+ i 1024) len) (- len i) 1024))
                     (seg (read-bytevector to-read port)))
                (cond ((eof-object? seg)
                       (return-result))
                      (else
                       (loop (+ i (bytevector-length seg))
                             (cons seg segments))))))))


      (define (make-delimited-textual-input-port port len)
        (let loop ((i 0)
                   (segments '()))
          (define (return-result)
            ;; (open-input-string (apply string-append (reverse segments)))
            (open-input-string (string-concatenate-reverse segments)))
          (if (= i len)
              (return-result)
              (let* ((to-read (if (> (+ i 25) len) (- len i) 25))
                     (seg (read-string to-read port)))
                (cond ((eof-object? seg) (return-result))
                      (else
                       (loop (+ i (string-length seg))
                             (cons seg segments))))))))


      (define (make-delimited-input-port port len)
        (cond ((binary-port? port)
               (make-delimited-binary-input-port port len))
              (else
               (make-delimited-textual-input-port port len))))

      ))

    ;; (cond-expand
    ;;  (sagittarius
    ;;   ;; Sagittarius' read-line doesn't accept an upper length.
    ;;   ;; These are from chibi.
    ;;   (define (%read-line n in)
    ;;     (let ((out (open-output-string)))
    ;;       (let lp ((i 0))
    ;;         (let ((ch (peek-char in)))
    ;;           (cond
    ;;            ((eof-object? ch)
    ;;             (let ((res (get-output-string out)))
    ;;               (and (not (equal? res "")) res)))
    ;;            ((eqv? ch #\newline)
    ;;             (read-char in)
    ;;             (get-output-string out))
    ;;            ((eqv? ch #\return)
    ;;             (read-char in)
    ;;             (if (eqv? #\newline (peek-char in))
    ;;                 (read-char in))
    ;;             (get-output-string out))
    ;;            ((and n (>= i n))
    ;;             (get-output-string out))
    ;;            (else
    ;;             (write-char (read-char in) out)
    ;;             (lp (+ i 1))))))))

    ;;   (define (read-line . o)
    ;;     (let ((in (if (pair? o) (car o) (current-input-port)))
    ;;           (n (if (and (pair? o) (pair? (cdr o))) (car (cdr o)) #f)))
    ;;       (let ((res (%read-line n in)))
    ;;         (if (not res)
    ;;             (eof-object)
    ;;             (let ((len (string-length res)))
    ;;               (cond
    ;;                ((and (> len 0) (eqv? #\newline (string-ref res (- len 1))))
    ;;                 (if (and (> len 1)
    ;;                          (eqv? #\return (string-ref res (- len 2))))
    ;;                     (substring res 0 (- len 2))
    ;;                     (substring res 0 (- len 1))))
    ;;                ((and (> len 0) (eqv? #\return (string-ref res (- len 1))))
    ;;                 (substring res 0 (- len 1)))
    ;;                (else
    ;;                 res))))))))
    ;;  (else))



    (define (utf8->char bv i len)
      ;; http://en.wikipedia.org/wiki/Utf8
      (define (check-following-bytes n)
        (do ((x 0 (+ x 1)))
            ((= x n))
          (if (not (= (bitwise-and (bytevector-u8-ref bv (+ i x 1)) #xc0) #x80))
              (error "invalid utf8" bv i len))))
      (if (= i len)
          (values #f 0)
          (let ((b0 (bytevector-u8-ref bv i))
                (available (- len i)))
            (cond
             ;; 0xxxxxxx
             ((= (bitwise-and b0 #x80) #x00) (values (integer->char b0) 1))
             ((not (= (bitwise-and b0 #xc0) #xc0))
              (error "invalid utf8" bv i len))
             ((= (bitwise-and b0 #xfe) #xfe)
              (error "invalid utf8" bv i len))
             ;; 110xxxxx 10xxxxxx
             ((and (> available 1) (= (bitwise-and b0 #xe0) #xc0))
              (check-following-bytes 1)
              (values
               (integer->char
                (bitwise-ior
                 (arithmetic-shift (bitwise-and b0 #x1f) 6)
                 (bitwise-and (bytevector-u8-ref bv (+ i 1)) #x3f)))
               2))
             ;; 1110xxxx 10xxxxxx 10xxxxxx
             ((and (> available 2) (= (bitwise-and b0 #xf0) #xe0))
              (check-following-bytes 2)
              (values
               (integer->char
                (bitwise-ior
                 (arithmetic-shift (bitwise-and b0 #x0f) 12)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 1)) #x3f) 6)
                 (bitwise-and (bytevector-u8-ref bv (+ i 2)) #x3f)))
               3))
             ;; 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
             ((and (> available 3) (= (bitwise-and b0 #xf8) #xf0))
              (check-following-bytes 3)
              (values
               (integer->char
                (bitwise-ior
                 (arithmetic-shift (bitwise-and b0 #x07) 18)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 1)) #x3f) 12)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 2)) #x3f) 6)
                 (bitwise-and (bytevector-u8-ref bv (+ i 3)) #x3f)))
               4))
             ;; 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
             ((and (> available 4) (= (bitwise-and b0 #xfc) #xf8))
              (check-following-bytes 4)
              (values
               (integer->char
                (bitwise-ior
                 (arithmetic-shift (bitwise-and b0 #x03) 24)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 1)) #x3f) 18)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 2)) #x3f) 12)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 3)) #x3f) 6)
                 (bitwise-and (bytevector-u8-ref bv (+ i 4)) #x3f)))
               5))
             ;; 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
             ((and (> available 5) (= (bitwise-and b0 #xfe) #xfc))
              (check-following-bytes 5)
              (values
               (integer->char
                (bitwise-ior
                 (arithmetic-shift (bitwise-and b0 #x01) 30)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 1)) #x3f) 24)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 2)) #x3f) 18)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 3)) #x3f) 12)
                 (arithmetic-shift
                  (bitwise-and (bytevector-u8-ref bv (+ i 4)) #x3f) 6)
                 (bitwise-and (bytevector-u8-ref bv (+ i 5)) #x3f)))
               6))
             (else (values #f 0))))))




    (cond-expand

     (chicken
      (define (binary-port->textual-port port) port))


     (chibi
      (define (binary-port->textual-port port)
        (let ((buffer (make-bytevector 6))
              (buffer-len 0))
          (make-custom-input-port
           (lambda (str start end)
             (let loop ((char-count 0)
                        (byte-count 0))

               ;; XXX byte-count or char-count?
               (if (>= (+ start (+ byte-count 6)) end)

                   byte-count
                   (let ((b (read-u8 port)))
                     (cond ((eof-object? b)
                            (cond ((= buffer-len 0)
                                   byte-count)
                                  (else
                                   (error "trailing utf8 bytes"))))
                           ((>= buffer-len 6)
                            (error "utf8 buffer overflow"))
                           (else
                            (bytevector-u8-set! buffer buffer-len b)
                            (set! buffer-len (+ buffer-len 1))
                            (let-values (((c bytes-used)
                                          (utf8->char buffer 0 buffer-len)))
                              (cond (c
                                     (if (not (= buffer-len bytes-used))
                                         (error "utf8 what?"))
                                     (string-set! str (+ start char-count) c)
                                     (set! buffer-len 0)
                                     (loop (+ char-count 1)
                                           (+ byte-count bytes-used)))
                                    (else
                                     (loop char-count byte-count))))))))))))))



     (gauche
      (define (binary-port->textual-port port)
        (let ((buffer (make-bytevector 6))
              (buffer-len 0))
          (make-virutal-input-port
           ;; :getb (lambda () (read-u8 port))
           :getc
           (lambda ()
             (let loop ((char-count 0)
                        (byte-count 0))
               (let ((b (read-u8 port)))
                 (cond ((eof-object? b)
                        (if (= buffer-len 0) b
                            (error "trailing utf8 bytes")))
                       ((>= buffer-len 6)
                        (error "utf8 buffer overflow"))
                       (else
                        (bytevector-u8-set! buffer buffer-len b)
                        (set! buffer-len (+ buffer-len 1))
                        (let-values (((c bytes-used)
                                      (utf8->char buffer 0 buffer-len)))
                          (cond (c
                                 (if (not (= buffer-len bytes-used))
                                     (error "utf8 what?"))
                                 (set! buffer-len 0)
                                 c)
                                (else (loop char-count byte-count)))))))))
           :ready (lambda (t-for-char-f-for-byte)
                    (u8-ready? port) ;; XXX this isn't right
                    )
           :close (lambda () #t)))))


     (sagittarius
      (define (binary-port->textual-port port)
        (transcoded-port port (make-transcoder
                               (utf-8-codec)
                               (eol-style none)))))


     (else
      ;; for schemes with no procedural ports
      (define (binary-port->textual-port port)
        (let loop ((segments '()))
          (let ((seg (read-bytevector 1024 port)))
            (cond ((eof-object? seg)
                   (open-input-string
                    (utf8->string
                     (reverse-bytevector-list->bytevector segments))))
                  (else
                   (loop (cons seg segments)))))))
      ))



    (cond-expand

     (chicken
      (define (textual-port->binary-port port)
        (make-input-port
         (lambda () ; read-char
           (read-char port))
         (lambda () ; char-ready?
           (char-ready? port))
         (lambda () #t) ; close
         (lambda () ; peek-char
           (peek-char port)))))

     (chibi
      (define (textual-port->binary-port port)
        (let ((buffer '()))
          (define (get-next-byte)
            (cond ((null? buffer)
                   (let ((c (read-char port)))
                     (cond ((eof-object? c) c)
                           (else
                            (let ((bytes (bytevector->u8-list
                                          (string->utf8
                                           (string c)))))
                              (set! buffer (cdr bytes))
                              (car bytes))))))
                  (else
                   (let ((b (car buffer)))
                     (set! buffer (cdr buffer))
                     b))))

          (make-custom-binary-input-port
           (lambda (bv start end)
             (let ((len (- end start)))
               (let loop ((i 0))
                 (cond ((= i len) i)
                       (else
                        (let ((b (get-next-byte)))
                          (cond ((eof-object? b) i)
                                (else
                                 (bytevector-u8-set! bv (+ start i) b)
                                 (loop (+ i 1))))))))))))))

     (gauche
      (define (textual-port->binary-port port)
        (let ((saw-eof #f)
              (buffer '()))
          (make-virutal-input-port
           :getb (lambda ()
                   (cond (saw-eof (eof-object))
                         ((not (null? buffer))
                          (let ((b (car buffer)))
                            (set! buffer (cdr buffer))
                            b))
                         (else
                          (let* ((c (read-char port))
                                 (data (bytevector->u8-list
                                        (string->utf8 (string c))))
                                 (b-first (car data))
                                 (b-rest (cdr data)))
                            (set! buffer b-rest)
                            b-first))))
           ;;:getc (lambda () #f)
           :ready (lambda (t-for-char-f-for-byte)
                    (cond (saw-eof #f)
                          ((not (null? buffer)) #t)
                          (else
                           (char-ready? port))))
           :close (lambda () #t)))))


     (sagittarius
      (define (textual-port->binary-port port)
        (let ((buffer '()))
          (define (get-next-byte)
            (cond ((null? buffer)
                   (let ((c (read-char port)))
                     (cond ((eof-object? c) c)
                           (else
                            (let ((bytes (bytevector->u8-list
                                          (string->utf8 (string c)))))
                              (set! buffer (cdr bytes))
                              (car bytes))))))
                  (else
                   (let ((b (car buffer)))
                     (set! buffer (cdr buffer))
                     b))))

          (make-custom-binary-input-port
           "textual-port->binary-port"
           (lambda (bv start count)
             (let ((len count))
               (let loop ((i 0))
                 (cond ((= i len) i)
                       (else
                        (let ((b (get-next-byte)))
                          (cond ((eof-object? b) i)
                                (else
                                 (bytevector-u8-set! bv (+ start i) b)
                                 (loop (+ i 1))))))))))
           #f ;; get-position
           #f ;; set-position!
           #f ;; close
           #f ;; ready
           ))))

     (else
      ;; for schemes with no procedural ports
      (define (textual-port->binary-port port)
        (let loop ((segments '()))
          (let ((seg (read-string 1024 port)))
            (cond ((eof-object? seg)
                   (open-input-bytevector
                    (reverse-bytevector-list->bytevector segments)))
                  (else
                   (loop (cons (string->utf8 seg) segments)))))))))


    (define (snow-port-position p)
      (cond-expand
       (chibi (file-position p))
       (chicken (file-position p))
       (gauche (port-tell p))
       ((or foment sagittarius) (port-position p))))

    (define (snow-set-port-position! port pos)
      (cond-expand
       (chibi (set-file-position! port pos seek/set))
       (chicken (set-file-position! port pos))
       (gauche (port-seek port pos SEEK_SET))
       ((or foment sagittarius) (set-port-position! port pos))))

    (define (snow-set-port-position-from-current! port offset)
      (cond-expand
       (chibi (set-file-position! port offset seek/cur))
       (chicken (set-file-position! port offset seek/cur))
       (gauche (port-seek port offset SEEK_CUR))
       ((or foment sagittarius) (set-port-position!
                                 port (+ (port-position port) offset)))))

    (define (snow-set-port-position-from-end! port offset)
      (let ((offset (if (< offset 0) offset (- offset))))
        (cond-expand
         (chibi (set-file-position! port offset seek/end))
         (chicken (set-file-position! port offset seek/end))
         (gauche (port-seek port offset SEEK_END))
         ((or foment sagittarius)
          (set-port-position! port offset 'end))
          ;; ;; XXX is there a better way?
          ;; (cond ((textual-port? port)
          ;;        (let loop ()
          ;;          (let ((c (read-char port)))
          ;;            (if (eof-object? c) #t
          ;;                (loop)))))
          ;;       (else
          ;;        (let loop ()
          ;;          (let ((c (read-u8 port)))
          ;;            (if (eof-object? c) #t
          ;;                (loop))))))
          ;; (set-port-position!
          ;;  port (+ (port-position port) offset))
        )))

    ))
