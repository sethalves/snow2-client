;; -*- scheme -*-
;; srfi-13, String LIbraries
;; http://srfi.schemers.org/srfi-13/srfi-13.html
;; http://wiki.call-cc.org/man/4/Unit%20srfi-13

(define-library (seth srfi-13-strings)
  (export
   string-tokenize
   string-pad
   string-map
   string-trim
   string-trim-right
   string-trim-both
   string-take
   string-take-right
   ;; XXX the rest...
   )
  (import (scheme base))
  (cond-expand
   (chibi (import (scheme char) (chibi char-set) (chibi char-set full)
                  (srfi 8) (srfi 33) (chibi optional)
                  ))
   (chicken (import (srfi 13)))
   (gauche (import (gauche) (srfi 13)))
   (sagittarius (import (srfi :13))))
  (begin

    (cond-expand

     (chicken
      ;; (use srfi-13)
      )

     ((or gauche sagittarius)
      #t)

     (else
      ;; XXX has anyone ported srfi-13 to chibi?

      (define (string-tokenize s . token-chars+start+end)
        (let* ((args-len (length token-chars+start+end))
               (token-chars (if (> args-len 0)
                                (list-ref token-chars+start+end 0)
                                char-set:graphic))
               (start (if (> args-len 1)
                          (list-ref token-chars+start+end 1)
                          0))
               (end (if (> args-len 2)
                        (list-ref token-chars+start+end 2)
                        (string-length s))))

          (reverse
           (let loop ((tokens '())
                      (current-token "")
                      (s s))
             (cond ((= (string-length s) 0)
                    (if (> (string-length current-token) 0)
                        (cons current-token tokens)
                        tokens))
                   (else
                    (let ((current-char (string-ref s 0))
                          (s (substring s 1 (string-length s))))
                      (cond ((token-chars current-char)
                             (loop (tokens
                                    (string-append current-token current-char)
                                    s)))
                            (else
                             (loop (cons current-token tokens)
                                   ""
                                   s))))))))))

      (define (string-pad str n . char+start+end)
        (let ((pad-char (if (null? char+start+end) #\space
                            (car char+start+end))))
          (let ((orig-length (string-length str)))
            (if (>= orig-length n) str
                (string-append
                 (make-string (- n orig-length) pad-char)
                 str)))))

      (define (string-map proc s . maybe-start+end)
        (list->string (map proc (string->list s))))

      ;;
      ;; string trim
      ;;

      (define (string-trim-decider s i criterion)
        (or (and (procedure? criterion)
                 (criterion (string-ref s i)))
            (and (char? criterion)
                 (eqv? criterion (string-ref s i)))
            (and (char-set? criterion)
                 (char-set-contains? criterion (string-ref s i)))))


      (define (string-trim-arguments s criterion+start+end)
        (let* ((oa-len (length criterion+start+end))
               (criterion (if (> oa-len 0)
                              (car criterion+start+end)
                              char-set:whitespace))
               (start (if (> oa-len 1) (cadr criterion+start+end) 0))
               (end (if (> oa-len 2)
                        (list-ref criterion+start+end 2)
                        (string-length s))))
          (values criterion start end)))


      (define (string-trim s . criterion+start+end)
        (receive
         (criterion start end) (string-trim-arguments s criterion+start+end)
         (let loop ((i start))
           (cond ((= i end) "")
                 ((string-trim-decider s i criterion) (loop (+ i 1)))
                 (else (substring s i end))))))


      (define (string-trim-right s . criterion+start+end)
        (receive
         (criterion start end) (string-trim-arguments s criterion+start+end)
         (let loop ((i end))
           (cond ((= i start) "")
                 ((string-trim-decider s (- i 1) criterion) (loop (- i 1)))
                 (else (substring s start i))))))


      (define (string-trim-both s . criterion+start+end)
        (receive
         (criterion start end) (string-trim-arguments s criterion+start+end)
         (let sloop ((si start))
           (cond ((= si end) "")
                 ((string-trim-decider s si criterion) (sloop (+ si 1)))
                 (else (let eloop ((ei end))
                         (cond ((string-trim-decider s (- ei 1) criterion)
                                (eloop (- ei 1)))
                               (else (substring s si ei)))))))))

      (define (string-take s n)
        (substring s 0 n))

      (define (string-take-right s n)
        (substring s (- (string-length s) n) (string-length s)))

      ))))
