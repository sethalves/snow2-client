(define-library (snow extio)
  (export snow-read-string
          snow-force-output
          snow-pretty-print
          make-delimited-input-port
          binary-port->textual-port
          textual-port->binary-port
          read-line
          )
  (import (scheme base)
          (scheme write))
  (cond-expand
   (chibi (import (chibi io)))
   (chicken (import (only (chicken) flush-output pretty-print)
                    ;; (only (extras) read-string!)
                    (ports)))
   (gauche (import (snow gauche-extio-utils)))
   (sagittarius))
  (import (snow snowlib)
          (snow binio)
          (snow bytevector)
          (snow srfi-60-integers-as-bits)
          )
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



    (define (utf8->char bv i len)
      ;; http://en.wikipedia.org/wiki/Utf8
      (define (check-following-bytes n)
        (do ((x 0 (+ x 1)))
            ((= x n))
          (if (not (= (bitwise-and (bytevector-u8-ref bv (+ i x 1)) #xc0) #x80))
              (snow-error "invalid utf8" bv i len))))
      (if (= i len)
          (values #f 0)
          (let ((b0 (bytevector-u8-ref bv i))
                (available (- len i)))
            (cond
             ;; 0xxxxxxx
             ((= (bitwise-and b0 #x80) #x00) (values (integer->char b0) 1))
             ((not (= (bitwise-and b0 #xc0) #xc0))
              (snow-error "invalid utf8" bv i len))
             ((= (bitwise-and b0 #xfe) #xfe)
              (snow-error "invalid utf8" bv i len))
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


     ;; (chibi
     ;;  (define (binary-port->textual-port port)
     ;;    (let ((buffer (make-bytevector 6))
     ;;          (buffer-len 0))
     ;;      (make-custom-input-port
     ;;       (lambda (str start end)
     ;;         (let loop ((char-count 0)
     ;;                    (byte-count 0))

     ;;           (newline)
     ;;           (display "start=") (write start) (newline)
     ;;           (display "end=") (write end) (newline)
     ;;           (display "buffer=") (write buffer) (newline)
     ;;           (display "buffer-len=") (write buffer-len) (newline)
     ;;           (display "char-count=") (write char-count) (newline)
     ;;           (display "byte-count=") (write byte-count) (newline)

     ;;           (if (>= (+ start char-count) end)
     ;;               (begin
     ;;                 (display "reached limit, returning ")
     ;;                 byte-count)
     ;;               (let ((b (read-u8 port)))
     ;;                 (cond ((eof-object? b)
     ;;                        (if (= buffer-len 0)
     ;;                            (begin
     ;;                              (display "got eof, returning ")
     ;;                              (write byte-count)
     ;;                              (newline)
     ;;                              byte-count)
     ;;                            (snow-error "trailing utf8 bytes")))
     ;;                       ((>= buffer-len 6)
     ;;                        (snow-error "utf8 buffer overflow"))
     ;;                       (else
     ;;                        (bytevector-u8-set! buffer buffer-len b)
     ;;                        (set! buffer-len (+ buffer-len 1))
     ;;                        (let-values (((c bytes-used)
     ;;                                      (utf8->char buffer 0 buffer-len)))
     ;;                          (cond (c
     ;;                                 (display "c=") (write c) (newline)
     ;;                                 (if (not (= buffer-len bytes-used))
     ;;                                     (snow-error "utf8 what?"))
     ;;                                 ;; (string-set! str (+ start char-count) c)

     ;;                                 (do ((x 0 (+ x 1)))
     ;;                                     ((= x bytes-used))
     ;;                                   (bytevector-u8-set!
     ;;                                    str
     ;;                                    (+ start byte-count x)
     ;;                                    ;; (integer->char
     ;;                                     (bytevector-u8-ref buffer x))
     ;;                                   ;;)
     ;;                                   )

     ;;                                 (set! buffer-len 0)
     ;;                                 (display "looping...\n")
     ;;                                 (loop (+ char-count 1)
     ;;                                       (+ byte-count bytes-used)))
     ;;                                (else
     ;;                                 (loop char-count byte-count))))))))))))))



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
                            (snow-error "trailing utf8 bytes")))
                       ((>= buffer-len 6)
                        (snow-error "utf8 buffer overflow"))
                       (else
                        (bytevector-u8-set! buffer buffer-len b)
                        (set! buffer-len (+ buffer-len 1))
                        (let-values (((c bytes-used)
                                      (utf8->char buffer 0 buffer-len)))
                          (cond (c
                                 (if (not (= buffer-len bytes-used))
                                     (snow-error "utf8 what?"))
                                 (set! buffer-len 0)
                                 c)
                                (else (loop char-count byte-count)))))))))
           :ready (lambda (t-for-char-f-for-byte)
                    (u8-ready? port) ;; XXX this isn't right
                    )
           :close (lambda () #t)))))

     (else
      ;; for schemes with no procedural ports (sagittarius)
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


     (else
      ;; for schemes with no procedural ports (sagittarius)
      (define (textual-port->binary-port port)
        (let loop ((segments '()))
          (let ((seg (read-string 1024 port)))
            (cond ((eof-object? seg)
                   (open-input-bytevector
                    (reverse-bytevector-list->bytevector segments)))
                  (else
                   (loop (cons (string->utf8 seg) segments)))))))))


    ))
