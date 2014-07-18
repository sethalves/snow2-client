;; -*- scheme -*-
;; srfi-13, String LIbraries
;; http://srfi.schemers.org/srfi-13/srfi-13.html
;; http://wiki.call-cc.org/man/4/Unit%20srfi-13

(define-library (srfi 13)
  (export
   string-tokenize
   string-pad
   string-pad-right
   ;; string-map ;; this one is in r7rs
   string-trim
   string-trim-right
   string-trim-both
   string-take
   string-take-right
   string-join
   string-prefix?
   string-prefix-ci?
   string-suffix?
   string-suffix-ci?
   string-contains
   string-contains-ci
   substring/shared
   string-concatenate
   string-concatenate/shared
   string-append/shared
   string-concatenate-reverse
   string-concatenate-reverse/shared
   string-index
   string-index-right
   string-skip
   string-skip-right
   reverse-list->string
   string-null?
   ;; XXX the rest...
   )
  (import (scheme base))
  (cond-expand
   (chibi (import (scheme char) (chibi char-set) (chibi char-set full)
                  (srfi 8) (srfi 33) (chibi optional)))
   (sagittarius (import (srfi :13)))
   (foment (import (scheme char)
                   (srfi 14))))
  (begin

    (cond-expand
     (chicken)
     ((or gauche sagittarius) #t)

     (else
      ;; XXX has anyone ported srfi-13 to chibi?

      ;; here is a partially and poorly implemented, less-macro'ed
      ;; version of srfi-13

      (define (string-tokenize s . token-chars+start+end)
        (let* ((args-len (length token-chars+start+end))
               (token-chars (if (> args-len 0)
                                (list-ref token-chars+start+end 0)
                                ;; (lambda (c)
                                ;;   (char-set-contains? char-set:graphic c))
                                char-set:graphic
                                ))
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
                      (cond ((char-set-contains? token-chars current-char)
                             (loop tokens
                                   (string-append
                                    current-token (string current-char))
                                   s))
                            (else
                             (loop
                              (cond ((> (string-length current-token) 0)
                                     (cons current-token tokens))
                                    (else tokens))
                              ""
                              s))))))))))

      (define (string-pad str n . char+start+end)
        (let* ((args-len (length char+start+end))
               (pad-char (if (> args-len 0)
                             (list-ref char+start+end 0)
                             #\space))
               (start (if (> args-len 1)
                          (list-ref char+start+end 1)
                          0))
               (end (if (> args-len 2)
                        (list-ref char+start+end 2)
                        (string-length str)))
               (str (substring str start end)))

          (let ((orig-length (string-length str)))
            (if (>= orig-length n) str
                (string-append
                 (make-string (- n orig-length) pad-char)
                 str)))))


      (define (string-pad-right str n . char+start+end)
        (let* ((args-len (length char+start+end))
               (pad-char (if (> args-len 0)
                             (list-ref char+start+end 0)
                             #\space))
               (start (if (> args-len 1)
                          (list-ref char+start+end 1)
                          0))
               (end (if (> args-len 2)
                        (list-ref char+start+end 2)
                        (string-length str)))
               (str (substring str start end)))

          (let ((orig-length (string-length str)))
            (if (>= orig-length n) str
                (string-append
                 str
                 (make-string (- n orig-length) pad-char))))))


      ;; (define (string-map proc s . maybe-start+end)
      ;;   (list->string (map proc (string->list s))))


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
        (let-values (((criterion start end)
                      (string-trim-arguments s criterion+start+end)))
         (let loop ((i start))
           (cond ((= i end) "")
                 ((string-trim-decider s i criterion) (loop (+ i 1)))
                 (else (substring s i end))))))


      (define (string-trim-right s . criterion+start+end)
        (let-values (((criterion start end)
                      (string-trim-arguments s criterion+start+end)))
         (let loop ((i end))
           (cond ((= i start) "")
                 ((string-trim-decider s (- i 1) criterion) (loop (- i 1)))
                 (else (substring s start i))))))


      (define (string-trim-both s . criterion+start+end)
        (let-values (((criterion start end)
                      (string-trim-arguments s criterion+start+end)))
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

      (define (string-join items . maybe-delim+grammar)
        ;; XXX use grammar

        ;; 'infix means an infix or separator grammar: insert the delimiter between list elements. An empty list will produce an empty string -- note, however, that parsing an empty string with an infix or separator grammar is ambiguous. Is it an empty list, or a list of one element, the empty string?
        ;; 'strict-infix means the same as 'infix, but will raise an error if given an empty list.
        ;; 'suffix means a suffix or terminator grammar: insert the delimiter after every list element. This grammar has no ambiguities.
        ;; 'prefix means a prefix grammar: insert the delimiter before every list element. This grammar has no ambiguities.

        (let ((delim (if (> (length maybe-delim+grammar) 0)
                         (list-ref maybe-delim+grammar 0) " "))
              (grammar (if (> (length maybe-delim+grammar) 1)
                           (list-ref maybe-delim+grammar 1) 'infix)))
          (if (null? items)
              ""
              (let loop ((result '())
                         (items items))
                (if (null? items)
                    (apply string-append (reverse (cdr result)))
                    (loop (cons delim (cons (car items) result))
                          (cdr items)))))))


      (define (string-prefix-worker? s1 s2 tester opt-args)
        (let* ((olen (length opt-args))
               (start1 (if (> olen 0) (list-ref opt-args 0) 0))
               (end1 (if (> olen 1) (list-ref opt-args 1) (string-length s1)))
               (start2 (if (> olen 2) (list-ref opt-args 2) 0))
               (end2 (if (> olen 3) (list-ref opt-args 3) (string-length s2))))
          (let loop ((i1 start1)
                     (i2 start2))
            (cond ((= i1 end1) #t)
                  ((= i2 end2) #f)
                  ((not (tester (string-ref s1 i1) (string-ref s2 i2))) #f)
                  (else
                   (loop (+ i1 1) (+ i2 1)))))))

      (define (string-prefix? s1 s2 . opt-args)
        (string-prefix-worker? s1 s2 char=? opt-args))

      (define (string-prefix-ci? s1 s2 . opt-args)
        (string-prefix-worker? s1 s2 char-ci=? opt-args))

      (define (string-suffix-worker? s1 s2 tester opt-args)
        (let* ((olen (length opt-args))
               (start1 (if (> olen 0) (list-ref opt-args 0) 0))
               (end1 (if (> olen 1) (list-ref opt-args 1) (string-length s1)))
               (start2 (if (> olen 2) (list-ref opt-args 2) 0))
               (end2 (if (> olen 3) (list-ref opt-args 3) (string-length s2))))
          (let loop ((i1 (- end1 1))
                     (i2 (- end2 1)))
            (cond ((< i1 start1) #t)
                  ((< i2 start2) #f)
                  ((not (tester (string-ref s1 i1) (string-ref s2 i2))) #f)
                  (else
                   (loop (- i1 1) (- i2 1)))))))

      (define (string-suffix? s1 s2 . opt-args)
        (string-suffix-worker? s1 s2 char=? opt-args))

      (define (string-suffix-ci? s1 s2 . opt-args)
        (string-suffix-worker? s1 s2 char-ci=? opt-args))


      (define (string-contains-worker s1 s2 prefix? opt-args)
        (let* ((olen (length opt-args))
               (start1 (if (> olen 0) (list-ref opt-args 0) 0))
               (end1 (if (> olen 1) (list-ref opt-args 1) (string-length s1)))
               (start2 (if (> olen 2) (list-ref opt-args 2) 0))
               (end2 (if (> olen 3) (list-ref opt-args 3) (string-length s2))))
          (let loop ((i1 start1))
            (cond ((= i1 end1) #f)
                  ((prefix? s2 s1 start2 end2 i1 end1) i1)
                  (else (loop (+ i1 1)))))))

      (define (string-contains s1 s2 . opt-args)
        (string-contains-worker s1 s2 string-prefix? opt-args))

      (define (string-contains-ci s1 s2 . opt-args)
        (string-contains-worker s1 s2 string-prefix-ci? opt-args))



      (define substring/shared substring)

      (define (string-concatenate args)
        (apply string-append args))

      (define string-concatenate/shared string-concatenate)

      (define string-append/shared string-append)

      (define (string-concatenate-reverse string-list . oa)
        (let* ((oa-len (length oa))
               (final-string-0 (if (> oa-len 0) (car oa) #f))
               (end (if (> oa-len 1) (cadr oa) #f))
               (final-string-1
                (cond ((not final-string-0) #f)
                      ((not end) final-string-0)
                      (else
                       (substring/shared final-string-0 0 end))))
               (string-list
                (cond (final-string-1
                       (cons final-string-1 string-list))
                      (else string-list))))
          (string-concatenate (reverse string-list))))

      (define string-concatenate-reverse/shared string-concatenate-reverse)



      (define (string-index/skip-tester char/char-set/pred)
        (cond ((procedure? char/char-set/pred) char/char-set/pred)
              ((char-set? char/char-set/pred)
               (lambda (c) (char-set-contains? char/char-set/pred c)))
              (else (lambda (c) (eqv? c char/char-set/pred)))))

      (define (string-index/skip-start-end s start+end)
        (let* ((args-len (length start+end))
               (start (if (> args-len 0)
                          (list-ref start+end 0)
                          0))
               (end (if (> args-len 1)
                        (list-ref start+end 1)
                        (string-length s))))
          (values start end)))

      (define (string-index s char/char-set/pred . start+end)
        (let-values (((start end) (string-index/skip-start-end s start+end)))
          (let ((tester (string-index/skip-tester char/char-set/pred)))
            (let loop ((i start))
              (cond ((>= i end) #f)
                    ((tester (string-ref s i)) i)
                    (else (loop (+ i 1))))))))

      (define (string-index-right s char/char-set/pred . start+end)
        (let-values (((start end) (string-index/skip-start-end s start+end)))
          (let ((tester (string-index/skip-tester char/char-set/pred)))
            (let loop ((i (- end 1)))
              (cond ((< i start) #f)
                    ((tester (string-ref s i)) i)
                    (else (loop (- i 1))))))))

      (define (string-skip s char/char-set/pred . start+end)
        (let-values (((start end) (string-index/skip-start-end s start+end)))
          (let* ((tester (string-index/skip-tester char/char-set/pred))
                 (stester (lambda (c) (not (tester c)))))
            (let loop ((i start))
              (cond ((>= i end) #f)
                    ((stester (string-ref s i)) i)
                    (else (loop (+ i 1))))))))

      (define (string-skip-right s char/char-set/pred . start+end)
        (let-values (((start end) (string-index/skip-start-end s start+end)))
          (let* ((tester (string-index/skip-tester char/char-set/pred))
                 (stester (lambda (c) (not (tester c)))))
            (let loop ((i (- end 1)))
              (cond ((< i start) #f)
                    ((stester (string-ref s i)) i)
                    (else (loop (- i 1))))))))


      (define (string-null? s)
        (= (string-length s) 0))


      ))


    (cond-expand
     ;; XXX remove chicken from this list when chicken's (srfi 13) has it
     ((or chibi chicken foment)
      (define (reverse-list->string char-list)
        (let* ((len (length char-list))
               (s (make-string len)))
          (let loop ((i (- len 1))
                     (char-list char-list))
            (cond ((null? char-list) s)
                  (else
                   (string-set! s i (car char-list))
                   (loop (- i 1)
                         (cdr char-list))))))))
     (else))

    ))
