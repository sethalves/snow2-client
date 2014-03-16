(define-library (snow extio)
  (export snow-read-string
          snow-force-output
          snow-pretty-print
          make-delimited-input-port
          binary-port->latin-1-textual-port
          read-line
          )
  (import (scheme base) (scheme write))
  (cond-expand
   (chibi (import (chibi io)))
   (chicken (import (only (chicken) flush-output pretty-print)
                    (only (extras) read-string!)
                    (ports)))
   (gauche (import (snow gauche-extio-utils)))
   (sagittarius))
  (import (snow binio)
          (snow bytevector))
  (begin

    (cond-expand

     ;; snow-read-string is like r7rs read-string except that
     ;; if the length is #f, it will read until eof (like CHICKEN's).

     ((or chicken)
      (define snow-read-string read-string))

     ((or chibi gauche sagittarius)
      (define (read-string-until-eof port)
        (let loop ((strings '()))
          (let ((s (read-string 4000 port)))
            (cond ((eof-object? s)
                   (apply string-append (reverse strings)))
                  (else
                   (loop (cons s strings)))))))

      (define (snow-read-string len port)
        (if (not len)
            (read-string-until-eof port)
            (read-string len port))))
     )


    (cond-expand

     ((or bigloo
          chibi
          larceny
          chez
          sagittarius
          sisc
          stklos)

      (define (snow-force-output . maybe-port)
        (let ((port (if (null? maybe-port) (current-output-port)
                        (car maybe-port))))
          (flush-output-port port))))

     ((or gambit
          guile
          kawa
          scheme48
          scm
          scsh)

      (define (snow-force-output . maybe-port)
        (let ((port (if (null? maybe-port) (current-output-port)
                        (car maybe-port))))
          (force-output port))))

     ((or chicken
          mit
          mzscheme)

      (define (snow-force-output . maybe-port)
        (let ((port (if (null? maybe-port) (current-output-port)
                        (car maybe-port))))
          (flush-output port))))

     (gauche
      ;; See gauche-extio-utils.sld
      ;; this was done to get access to flush
      ;; (define (snow-force-output . maybe-port)
      ;;   (let ((port (if (null? maybe-port) (current-output-port)
      ;;                   (car maybe-port))))
      ;;     (flush port)))
      )
     )

    (cond-expand

     ((or bigloo
          mit)

      (define (snow-pretty-print obj . maybe-port)
        (let ((port (if (null? maybe-port) (current-output-port)
                        (car maybe-port))))
          (pp obj port))))

     ((or chez
          ;; chicken -- XXX how to get pretty-print into r7rs library?
          gambit
          mzscheme
          petite
          scm)

      (define (snow-pretty-print obj . maybe-port)
        (let ((port (if (null? maybe-port) (current-output-port)
                        (car maybe-port))))
          (pretty-print obj port))))

     ((or scheme48
          scsh)

      (define (snow-pretty-print obj . maybe-port)
        (let ((port (if (null? maybe-port) (current-output-port)
                        (car maybe-port))))
          (pretty-print obj port 0)
          (newline port))))

     (stklos

      (define (snow-pretty-print obj . maybe-port)
        (let ((port (if (null? maybe-port) (current-output-port)
                        (car maybe-port))))
          (pretty-print obj port: port))))

     (else

      (define (snow-pretty-print obj . maybe-port)
        (let ((port (if (null? maybe-port) (current-output-port)
                        (car maybe-port))))
          (write obj port)
          (newline port)))))


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

     ;; chibi doesn't yet have binary procedural ports
     ;; (chibi
     ;;  (define (make-delimited-input-port port len)
     ;;    (let ((index 0)
     ;;          (saw-eof #f))
     ;;      (make-custom-input-port
     ;;       (lambda (str start end)
     ;;         (cond ((= index len)
     ;;                ;; (eof-object) ;; ?
     ;;                0)
     ;;               (saw-eof (eof-object))
     ;;               (else
     ;;                (let ((c (read-latin-1-char port)))
     ;;                  (cond ((eof-object? c)
     ;;                         (set! saw-eof #t)
     ;;                         0)
     ;;                        (else
     ;;                         (set! index (+ index 1))
     ;;                         (string-set! str start c)
     ;;                         1))))))))))

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


     (else
      ;; for schemes with no procedural ports (sagittarius)
      (define (make-delimited-input-port port len)
        (let ((buf (make-string len)))
          (let loop ((i 0))
            (if (= i len)
                (cond ((binary-port? port)
                       (open-input-bytevector
                        (string->latin-1 buf)))
                      (else
                       (open-input-string buf)))
                (let ((c (read-latin-1-char port)))
                  (cond ((eof-object? c)
                         (cond ((binary-port? port)
                                (open-input-bytevector
                                 (string->latin-1 (substring buf 0 i))))
                               (else
                                (open-input-string (substring buf 0 i)))))
                        (else
                         (string-set! buf i c)
                         (loop (+ i 1)))))))))))

    (cond-expand
     (sagittarius
      ;; Sagittarius' read-line doesn't accept an upper length.
      ;; These are from chibi.
      (define (%read-line n in)
        (let ((out (open-output-string)))
          (let lp ((i 0))
            (let ((ch (peek-char in)))
              (cond
               ((eof-object? ch)
                (let ((res (get-output-string out)))
                  (and (not (equal? res "")) res)))
               ((eqv? ch #\newline)
                (read-char in)
                (get-output-string out))
               ((eqv? ch #\return)
                (read-char in)
                (if (eqv? #\newline (peek-char in))
                    (read-char in))
                (get-output-string out))
               ((and n (>= i n))
                (get-output-string out))
               (else
                (write-char (read-char in) out)
                (lp (+ i 1))))))))

      (define (read-line . o)
        (let ((in (if (pair? o) (car o) (current-input-port)))
              (n (if (and (pair? o) (pair? (cdr o))) (car (cdr o)) #f)))
          (let ((res (%read-line n in)))
            (if (not res)
                (eof-object)
                (let ((len (string-length res)))
                  (cond
                   ((and (> len 0) (eqv? #\newline (string-ref res (- len 1))))
                    (if (and (> len 1)
                             (eqv? #\return (string-ref res (- len 2))))
                        (substring res 0 (- len 2))
                        (substring res 0 (- len 1))))
                   ((and (> len 0) (eqv? #\return (string-ref res (- len 1))))
                    (substring res 0 (- len 1)))
                   (else
                    res))))))))
     (else))





    (cond-expand


     (chicken
      (define (binary-port->latin-1-textual-port port)
        (let ((saw-eof #f))
          (make-input-port
           (lambda () ; read-char
             (cond (saw-eof (eof-object))
                   (else
                    (read-latin-1-char port))))
           (lambda () ; char-ready?
             (cond (saw-eof #f)
                   (else (latin-1-char-ready? port))))
           (lambda () #t) ; close
           (lambda () ; peek-char
             (peek-latin-1-char port))))))

     ;; (chibi
     ;;  (define (binary-port->latin-1-textual-port port)
     ;;    (let ((saw-eof #f))
     ;;      (make-custom-input-port
     ;;       (lambda (str start end)
     ;;         (cond (saw-eof
     ;;                ;; (eof-object) ;;?
     ;;                0)
     ;;               (else
     ;;                (let ((c (read-latin-1-char port)))
     ;;                  (cond ((eof-object? c)
     ;;                         (set! saw-eof #t)
     ;;                         0)
     ;;                        (else
     ;;                         (string-set! str start c)
     ;;                         1))))))))))

     (gauche
      (define (binary-port->latin-1-textual-port port)
        (let ((saw-eof #f))
          (make-virutal-input-port
            ;; :getb (lambda () (read-u8 port))
            :getc (lambda ()
                    (cond (saw-eof (eof-object))
                          (else
                           (let ((c (read-latin-1-char port)))
                             (cond ((eof-object? c) (set! saw-eof #t)))
                             c))))
            :ready (lambda (t-for-char-f-for-byte) (latin-1-char-ready? port))
            :close (lambda () #t)))))


     (else
      ;; for schemes with no procedural ports (sagittarius)
      (define (binary-port->latin-1-textual-port port)
        (let loop ((chars (list)))
          (let ((i (read-u8 port)))
            (if (eof-object? i)
                (open-input-string
                 (latin-1->string (u8-list->bytevector (reverse chars))))
                (loop (cons i chars))))))))

    ))
