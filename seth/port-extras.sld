;; -*- scheme -*-

(define-library (seth port-extras)
  (export read-all-chars
          read-all-u8
          read-available-chars
          read-available-u8
          read-0
          consume-whitespace
          read-word
          read-words
          port-find-byte)

  (import (scheme base)
          (scheme write)
          (scheme char)
          (snow bytevector))
  (cond-expand
   (chibi (import (chibi io)))
   (chicken (import (scheme read)))
   (else))

  (begin

    (define (read-all-main f reader)
      ;; read until eof
      (let loop ((chars (list))
                 (c (reader f)))
        (if (eof-object? c)
            (reverse chars)
            (loop (cons c chars) (reader f)))))

    (define (read-all-chars f)
      (list->string (read-all-main f read-char)))

    ;; (define (read-all-latin-1-chars f)
    ;;   (list->string (read-all-main f read-latin-1-char)))

    (define (read-all-u8 f)
      ;; (u8-list->bytevector (read-all-main f read-u8)))
      (let loop ((segments '()))
        (let ((seg (read-bytevector 1024 f)))
          (cond ((eof-object? seg)
                 (reverse-bytevector-list->bytevector segments))
                (else
                 (loop (cons seg segments)))))))


    (define (read-available-main port at-least at-most peeker reader ready?)
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
               (reverse data)))))


    (define (read-available-chars port at-least at-most)
      (list->string (read-available-main port at-least at-most
                                         peek-char read-char char-ready?)))


    (define (read-available-u8 port at-least at-most)
      (u8-list->bytevector (read-available-main port at-least at-most
                                                peek-u8 read-u8 u8-ready?)))


    ;; r7rs has read-string
    ;; (define (read-n n port)
    ;;   (read-available-chars port n n))

    ;; r7rs has read-bytevector
    ;; (define (read-n-u8 n port)
    ;;   (read-available-u8 port n n))


    (define (read-0 f)
      ;; read until a 0 character or eof.  the 0 isn't part of the return value
      (let loop ((ret (list)))
        (let ((c (read-char f)))
          (cond ((or (eof-object? c)
                     (= (char->integer c) 0))
                 (list->string (reverse ret)))
                (else
                 (loop (cons c ret)))))))


    (define (consume-whitespace . port-oa)
      (let ((port (if (null? port-oa) (current-input-port) (car port-oa))))
        (let loop ()
          (let ((c (peek-char port)))
            (cond ((eof-object? c) (read-char port))
                  ((char-whitespace? c)
                   (read-char port)
                   (loop))
                  (else #t))))))


    (define (read-word . port-oa)
      ;; reads a non-whitespace word, and consumes any trailing whitespace.
      (let ((port (if (null? port-oa) (current-input-port) (car port-oa))))
        (let loop ((chars (list)))
          (let ((c (read-char port)))
            (cond ((eof-object? c)
                   (if (null? chars)
                       c
                       (list->string (reverse chars))))
                  ((char-whitespace? c)
                   (consume-whitespace port)
                   (list->string (reverse chars)))
                  (else (loop (cons c chars))))))))



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

    ;; (define (generate-eof-object)
    ;;   ;; in racket, there's eof
    ;;   (read-char (open-input-string "")))

    ))
