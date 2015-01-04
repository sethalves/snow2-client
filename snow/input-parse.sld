(define-library (snow input-parse)
  (export find-string-from-port?
          assert-curr-char
          skip-until
          skip-while
          peek-next-char
          next-token
          next-token-of
          ;; read-string
          parser-error
          )
  (import (scheme base)
          (scheme read)
          (scheme write))
  (cond-expand
   (chibi (import (srfi 1)
                  (scheme char)
                  (scheme cxr)
                  (chibi io)
                  (srfi 13)
                  ))
   (chicken (import (input-parse)))
   (gauche (import (text parse)))
   (sagittarius (import (text parse)))
   (else (import (srfi 1)
                 (scheme char)
                 (scheme cxr)
                 (except (srfi 13)
                         string-copy string-map string-for-each
                         string-fill! string-copy! string->list
                         string-upcase string-downcase))))
  (begin

    (cond-expand
     ((or gauche sagittarius))
     (else
      ;; (define-syntax inc
      ;;   (syntax-rules () ((inc x) (+ 1 x))))
      (define (inc x) (+ 1 x))))

    (cond-expand

     ;; http://ktakashi.github.io/sagittarius-ref.html#ported.text.parse
     (sagittarius)

     ;; http://wiki.call-cc.org/eggref/4/input-parse
     (chicken)

     ;; http://practical-scheme.net/gauche/man/gauche-refe_168.html
     (gauche)

     (else

      (define (assert-curr-char c msg port)
        (let ((curr-char (peek-char port)))
          (cond ((memv curr-char c)
                 (read-char port)
                 curr-char)
                (else
                 (error msg (string-append
                                  "got "
                                  (string curr-char)
                                  " rather than "
                                  (string c)))))))


      (define (skip-until arg port)
        (let loop ((c (read-char port)))
          (cond
           ((memv c arg) c)
           ((eof-object? c)
            (if (memq '*eof* arg) c
                (parser-error port "Unexpected EOF while skipping until " arg)))
           (else (loop (read-char port))))))


      (define (skip-while skip-chars port)
        (do ((c (peek-char port) (peek-char port)))
            ((not (memv c skip-chars)) c)
          (read-char port)))


      (define input-parse%init-buffer
        (let ((buffer (make-string 512)))
          (lambda () buffer)))


      (define (next-token prefix-skipped-chars break-chars comment port)
        (let outer ((buffer (input-parse%init-buffer)) (filled-buffer-l '())
                    (c (skip-while prefix-skipped-chars port)))
          (let ((curr-buf-len (string-length buffer)))
            (let loop ((i 0) (c c))
              (cond
               ((memv c break-chars)
                (if (null? filled-buffer-l) (substring buffer 0 i)
                    (string-concatenate-reverse filled-buffer-l buffer i)))
               ((eof-object? c)
                (if (memq '*eof* break-chars) ;; was EOF expected?
                    (if (null? filled-buffer-l) (substring buffer 0 i)
                        (string-concatenate-reverse filled-buffer-l buffer i))
                    (parser-error port "EOF while reading a token " comment)))
               ((>= i curr-buf-len)
                (outer (make-string curr-buf-len)
                       (cons buffer filled-buffer-l) c))
               (else
                (string-set! buffer i c)
                (read-char port) ;; move to the next char
                (loop (inc i) (peek-char port))))))))


      (define (next-token-of incl-list/pred port)
        (let* ((buffer (input-parse%init-buffer))
               (curr-buf-len (string-length buffer)))
          (let outer ((buffer buffer) (filled-buffer-l '()))
            (let loop ((i 0))
              (if (>= i curr-buf-len) ;; make sure we have space
                  (outer (make-string curr-buf-len)
                         (cons buffer filled-buffer-l))
                  (let ((c (incl-list/pred (peek-char port))))
                    (if c
                        (begin
                          (string-set! buffer i c)
                          (read-char port) ;; move to the next char
                          (loop (inc i)))
                        ;; incl-list/pred decided it had had enough
                        (if (null? filled-buffer-l) (substring buffer 0 i)
                            (string-concatenate-reverse
                             filled-buffer-l buffer i)))))))))


      (define (peek-next-char port)
        (read-char port)
        (peek-char port))))


    (cond-expand
     ((or gauche sagittarius))
     (else

;; -- Function: find-string-from-port? STR IN-PORT MAX-NO-CHARS
;;    Looks for a string STR within the first MAX-NO-CHARS chars of the
;;    input port IN-PORT
;;    MAX-NO-CHARS may be omitted: in that case, the search span would be
;;    limited only by the end of the input stream.
;;    When the STR is found, the function returns the number of
;;    characters it has read from the port, and the port is set
;;    to read the first char after that (that is, after the STR)
;;    The function returns #f when the string wasn't found
;; Note the function reads the port *STRICTLY* sequentially, and does not
;; perform any buffering. So the function can be used even if the port is open
;; on a pipe or other communication channel.
;;
;; Probably can be classified as misc-io.
;;
;; Notes on the algorithm.
;; A special care should be taken in a situation when one had achieved a partial
;; match with (a head of) STR, and then some unexpected character appeared in
;; the stream. It'll be rash to discard all already read characters. Consider
;; an example of string "acab" and the stream "bacacab...", specifically when
;;    a  c  a _b_
;; b  a  c  a  c  a  b ...
;; that is, when 'aca' had matched, but then 'c' showed up in the stream
;; while we were looking for 'b'. In that case, discarding all already read
;; characters and starting the matching process from scratch, that is,
;; from 'c a b ...', would miss a certain match.
;; Note, we don't actually need to keep already read characters, or at least
;; strlen(str) characters in some kind of buffer. If there has been no match,
;; we can safely discard read characters. If there was some partial match,
;; we already know the characters before, they are in the STR itself, so
;; we don't need a special buffer for that.

;; "MISCIO" Search for string from port.
;; Written 1995 by Oleg Kiselyov (oleg@ponder.csci.unt.edu)
;; Modified 1996 by A. Jaffer (jaffer@ai.mit.edu)
;;
;; This code is in the public domain.

      (define (MISCIO%find-string-from-port? str <input-port> . max-no-char-oa)
        (letrec* ;; XXX letrec should be enough here.  bug in foment?
            ((max-no-char (if (null? max-no-char-oa) #f (car max-no-char-oa)))
             (no-chars-read 0)
             (my-peek-char			; Return a peeked char or #f
              (lambda () (and (or (not max-no-char) (< no-chars-read max-no-char))
                              (let ((c (peek-char <input-port>)))
                                (if (eof-object? c) #f c)))))
             (next-char (lambda () (read-char <input-port>)
                                (set! no-chars-read  (inc no-chars-read))))
             (match-1st-char			; of the string str
              (lambda ()
                (let ((c (my-peek-char)))
                  (if (not c) #f
                      (begin (next-char)
                             (if (char=? c (string-ref str 0))
                                 (match-other-chars 1)
                                 (match-1st-char)))))))
             ;; There has been a partial match, up to the point pos-to-match
             ;; (for example, str[0] has been found in the stream)
             ;; Now look to see if str[pos-to-match] for would be found, too
             (match-other-chars
              (lambda (pos-to-match)
                (if (>= pos-to-match (string-length str))
                    no-chars-read		; the entire string has matched
                    (let ((c (my-peek-char)))
                      (and c
                           (if (not (char=? c (string-ref str pos-to-match)))
                               (backtrack 1 pos-to-match)
                               (begin (next-char)
                                      (match-other-chars (inc pos-to-match)))))))))

;; There had been a partial match, but then a wrong char showed up.
;; Before discarding previously read (and matched) characters, we check
;; to see if there was some smaller partial match. Note, characters read
;; so far (which matter) are those of str[0..matched-substr-len - 1]
;; In other words, we will check to see if there is such i>0 that
;; substr(str,0,j) = substr(str,i,matched-substr-len)
;; where j=matched-substr-len - i
             (backtrack
              (lambda (i matched-substr-len)
                (let ((j (- matched-substr-len i)))
                  (if (<= j 0)
                      (match-1st-char) ; backed off completely to the begining of str
                      (let loop ((k 0))
                        (if (>= k j)
                            (match-other-chars j) ; there was indeed a shorter match
                            (if (char=? (string-ref str k)
                                        (string-ref str (+ i k)))
                                (loop (inc k))
                                (backtrack (inc i) matched-substr-len))))))))
             )
          (match-1st-char)))

      (define find-string-from-port? MISCIO%find-string-from-port?)

      ))


    (define (display-to-string obj)
      (let ((s (open-output-string)))
        (display obj s)
        (let ((result (get-output-string s)))
          (close-output-port s)
          result)))

    (define (parser-error port . msg-parts)
      (error
       "parser-error"
       (apply string-append (map display-to-string msg-parts))
       ))
    ))

