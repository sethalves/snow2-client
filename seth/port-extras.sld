;; -*- scheme -*-

(define-library (seth port-extras)
  (export read-all
          read-all-bytes
          read-available-to-list
          read-available-to-bytevector
          read-available
          read-n
          read-n-bytes
          read-0
          write-characters
          consume-whitespace
          read-byte
          write-byte
          read-word
          read-words
          port-find-byte
          ;; write-string
          write-bytes
          make-line-splitter
          generate-eof-object
          ;; flush-output-port
          )
  (import (scheme base))
  (cond-expand
   (chibi (import (chibi io) (scheme write) (scheme char) (seth bytevector)))
   (chicken (import (scheme char) (seth bytevector) (seth string-read-write)))
   ((or gauche sagittarius) (import (scheme char) (seth bytevector)))
   )
  (begin

    ;; XXX why is this failing?
    ;; (cond-expand
    ;;  (chicken
    ;;   (define flush-output-port flush-output)
    ;;   )
    ;;  (else))

    ;; (cond-expand

    ;;  (sagittarius
    ;;   (define peek-byte peek-u8)
    ;;   (define read-byte read-u8)
    ;;   (define write-byte write-u8))

    ;;  (else
    ;;   (define (peek-byte . opt)
    ;;     (let ((c (apply peek-char opt)))
    ;;       (if (eof-object? c) c
    ;;           (char->integer c))))

    ;;   (define (read-byte . opt)
    ;;     (let ((c (apply read-char opt)))
    ;;       (if (eof-object? c) c
    ;;           (char->integer c))))

    ;;   (define (write-byte data . opt)
    ;;     (apply write-char (cons (integer->char data) opt)))))


    (define (read-all f)
      ;; read until eof
      (let loop ((chars (list))
                 (c (read-char f)))
        (if (eof-object? c)
            (list->string (reverse chars))
            (loop (cons c chars) (read-char f)))))


    (define (read-available-to-list port at-least at-most)
      ;; read a certain amount of data.
      ;; if port is textual, read characters.  if it's binary,
      ;; read bytes.
      (let ((peeker (if (textual-port? port) peek-char peek-u8))
            (reader (if (textual-port? port) read-char read-u8))
            (ready? (if (textual-port? port) char-ready? u8-ready?)))
        (let loop ((data (list))
                   (len 0))
          (cond ((and at-most (>= (length data) at-most))
                 (reverse data))
                ((or (and at-least (< len at-least))
                     (ready? port))
                 (let ((c (peeker port)))
                   ;; something is ready
                   (if (eof-object? c)
                       ;; only an eof.
                       (if (null? data)
                           (reader port) ;; return the eof
                           (reverse data)) ;; return stuff before eof
                       ;; a char (or byte) is ready.  read it...
                       (loop (cons (reader port) data) (+ len 1)))))
                (else
                 ;; nothing ready, return what we have (if anything)
                 (reverse data))))))


    (define (read-available-bytes-to-bytevector port at-least at-most)
      ;; assumes port is a binary port
      (let* ((bytes (read-available-to-list port at-least at-most)))
        (if (eof-object? bytes) bytes
            (let ((bv (make-bytevector (length bytes))))
              (let loop ((bytes bytes)
                         (pos 0))
                (if (null? bytes) bv
                    (begin
                      (bytevector-u8-set! bv pos (car bytes))
                      (loop (cdr bytes) (+ pos 1)))))))))


    (define (read-available port at-least at-most)
      (let* ((chars (read-available-to-list port at-least at-most)))
        (if (eof-object? chars)
            chars
            (if (textual-port? port)
                (list->string chars)
                (list->string (map integer->char chars))))))


    (define (read-n n port)
      (read-available port n n))

    (define (read-n-bytes n port)
      ;; assumes port is a binary port
      (read-available-bytes-to-bytevector port n n))


    (define (read-all-bytes f)
      ;; read-char until eof, return a bytevector
      (let loop ((bytes (list))
                 (c (read-u8 f)))
        (if (eof-object? c)
            (u8-list->bytevector (reverse bytes))
            (loop (cons c bytes) (read-char f)))))


    (define (read-0 f)
      ;; read until a 0 character or eof.  the 0 isn't part of the return value
      (let loop ((ret (list)))
        (let ((c (read-char f)))
          (cond ((or (eof-object? c)
                     (= (char->integer c) 0))
                 (list->string (map integer->char (reverse ret))))
                (else
                 (loop (cons c ret)))))))


    (define (write-characters chars f)
      (let loop ((chars (string->list chars)))
        (if (null? chars) #t
            (begin
              ;; (output-char f (car chars))
              (write-char (car chars) f)
              (loop (cdr chars))))))


    (define (consume-whitespace . port-oa)
      (let ((port (if (null? port-oa) (current-input-port) (car port-oa))))
        (let loop ()
          (let ((c (peek-byte port)))
            (cond ((eof-object? c) (read-char port))
                  ((char-whitespace? (integer->char c))
                   (read-char port)
                   (loop))
                  (else #t))))))


    (define (read-word . port-oa)
      ;; reads a non-whitespace word, and consumes any trailing whitespace.
      (let ((port (if (null? port-oa) (current-input-port) (car port-oa))))
        (let loop ((bytes (list)))
          (let ((c (read-char port)))
            (cond ((eof-object? c)
                   (if (null? bytes)
                       c
                       (list->string (map integer->char (reverse bytes)))))
                  ((char-whitespace? (integer->char c))
                   (consume-whitespace port)
                   (list->string (map integer->char (reverse bytes))))
                  (else (loop (cons c bytes))))))))



    (define (read-words n . port-oa)
      (let ((port (if (null? port-oa) (current-input-port) (car port-oa))))
        (let loop ((result '())
                   (n n))
          (if (> n 0)
              (loop (cons (read-word port) result) (- n 1))
              (reverse result)))))


    (define (port-find-byte port byte)
      ;; byte should be an integer in the range [0, 255]
      (let ((c (read-u8 port)))
        (cond ((eof-object? c) #f)
              ((= c byte) #t)
              (else (port-find-byte port byte)))))


    ;; (define (write-string str num f)
    ;;   ;; write string to f
    ;;   (let loop ((chars (string->list str))
    ;;              (n 0))
    ;;     (cond ((null? chars) (flush-output-port f))
    ;;           ((= n num) (flush-output-port f))
    ;;           (else
    ;;            (write-char (car chars) f)
    ;;            (loop (cdr chars) (+ n 1))))))


    (define (write-bytes data f)
      ;; write bytevector to f
      (let loop ((i 0))
        (if (= i (bytevector-length data)) #t
            (begin
              (write-char (bytevector-u8-ref data i) f)
              (loop (+ i 1))))))


    (define (make-line-splitter line-received-func)
      (define buffer (list))
      (lambda (conn data)
        (cond
         ((eof-object? data)
          ;; xxx what if buffer isn't empty?
          (line-received-func conn data))
         (else
          (set! buffer (append buffer (string->list data)))
          (let loop ((in-line (list))
                     (after-line buffer))
            (cond ((null? after-line)
                   (set! buffer (reverse in-line)))
                  ((equal? (car after-line) #\newline)
                   (let ((line
                          (list->string (reverse (cons #\newline in-line)))))
                     (line-received-func conn line)
                     (loop (list) (cdr after-line))))
                  (else
                   (loop (cons (car after-line) in-line)
                         (cdr after-line)))))))))


    (define (generate-eof-object)
      ;; in racket, there's eof
      (read-char (open-input-string "")))


    ;; (cond-expand (chicken (register-feature! 'seth.port-extras)))
    ))