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
                    ;; (only (extras) read-string!)
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

     (chibi
      (define (make-delimited-textual-input-port port len)
        (let ((index 0))
          (make-custom-input-port
           (lambda (str start end)
             (cond ((= index len) (eof-object))
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
             (let* ((plan-to-read (- len index))
                    (plan-to-read (if (< (- end start) plan-to-read)
                                      (- end start) plan-to-read))
                    (did-read
                     (read-bytevector! bv port start (+ start plan-to-read))))
               (cond ((eof-object? did-read) 0)
                     (else
                      (set! index (+ index did-read))
                      did-read)))))))

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


     (else
      ;; for schemes with no procedural ports (sagittarius)
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
            (open-input-string (apply string-append (reverse segments))))
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


     (chibi
      ;; XXX this is currently failing on high-bit characters.
      (define (binary-port->latin-1-textual-port port)
        (make-custom-input-port
         (lambda (str start end)
           (let ((seg (read-bytevector (- end start) port)))
             (cond ((eof-object? seg) 0)
                   (else
                    (let ((s (latin-1->string seg)))
                      (let loop ((i 0))
                        (and (< i (string-length s))
                             (begin
                               (string-set! str (+ i start) (string-ref s i))
                               (loop (+ i 1)))))
                      (bytevector-length seg)))))))))

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
        (let loop ((segments '()))
          (let ((seg (read-bytevector 1024 port)))
            (cond ((eof-object? seg)
                   (open-input-string
                    (reverse-bytevector-list->latin-1-string segments)))
                  (else
                   (loop (cons seg segments)))))))

      ))

    ))
