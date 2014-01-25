;; -*- scheme -*-

;;
;; bytevector
;;
;; http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-3.html
;;
;; http://groups.csail.mit.edu/mac/projects/s48/1.1/manual/s48manual_40.html
;;

;; guile 2 can use this:
;; (use-modules (rnrs bytevectors))


(define-library (snow bytevector)
  (export bytevector
          make-bytevector
          bytevector?
          bytevector->u8-list
          u8-list->bytevector
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          utf8->string
          string->utf8
          latin-1->string
          string->latin-1
          bytevector-copy-partial
          bytevector-copy-partial!

          bytevector-u16-set!
          bytevector-u16-ref
          bytevector-u16-length
          make-u16-bytevector
          )
  (import (scheme base))
  (cond-expand
   (chibi (import (chibi io)))
   (chicken (import (chicken) (srfi 4)))
   (foment (import (scheme char)))
   (gauche (import (gauche uvector)))
   (sagittarius (import (util bytevector)))
   )
  (begin

    (cond-expand

     ((or chibi chicken foment gauche sagittarius)
      (define (latin-1->string bytes)
        (list->string
         (map integer->char
              (let loop ((i 0)
                         (lst (list)))
                (if (= i (bytevector-length bytes))
                    (reverse lst)
                    (loop (+ i 1)
                          (cons (bytevector-u8-ref bytes i) lst)))))))

      (define (string->latin-1 str)
        ;; XXX this wont work unless it's all ascii.
        (let* ((lst (map char->integer (string->list str)))
               (bv (make-bytevector (length lst))))
          (let loop ((lst lst)
                     (pos 0))
            (if (null? lst) bv
                (begin
                  (bytevector-u8-set! bv pos (car lst))
                  (loop (cdr lst) (+ pos 1)))))))

      (define (bytevector->u8-list bv)
        (let loop ((i 0)
                   (lst (list)))
          (if (= i (bytevector-length bv))
              (reverse lst)
              (loop (+ i 1)
                    (cons (bytevector-u8-ref bv i) lst)))))


      (define (u8-list->bytevector lst)
        (let ((bv (make-bytevector (length lst))))
          (let loop ((lst lst)
                     (pos 0))
            (if (null? lst) bv
                (begin
                  (bytevector-u8-set! bv pos (car lst))
                  (loop (cdr lst) (+ pos 1)))))))

      ))


    (cond-expand
     (sagittarius
      (define (bytevector . args)
        (let* ((len (length args))
               (bv (make-bytevector len)))
          (let loop ((i 0)
                     (args args))
            (cond ((= i len) bv)
                  (else
                   (bytevector-u8-set! bv i (car args))
                   (loop (+ i 1) (cdr args))))))))

     (chicken
      ;; (define make-bytevector make-u8vector)
      (define u8-list->bytevector list->u8vector)
      ;; (define utf8->string latin-1->string)
      ;; (define string->utf8 string->latin-1)
      )
     (foment
      #t)
     ((or bigloo gauche guile)
      (define bytevector u8vector)
      (define make-bytevector make-u8vector)
      (define bytevector? u8vector?)
      (define u8-list->bytevector list->u8vector)
      (define bytevector-length u8vector-length)
      (define bytevector-u8-ref u8vector-ref)
      (define bytevector-u8-set! u8vector-set!)
      (define utf8->string latin-1->string)
      (define string->utf8 string->latin-1))
     )


    (cond-expand
     ((or chibi chicken foment gauche sagittarius)
      ;; these didn't make it into final r7rs.
      (define (bytevector-copy-partial bv start end)
        (let ((res (make-bytevector (- end start))))
          (bytevector-copy-partial! bv start end res 0)
          res))

      ;; (define (bytevector-copy-partial! from start end to at)
      ;;   (do ((i start (+ i 1)))
      ;;       ((= i end))
      ;;     (bytevector-u8-set! to (+ (- i start) at)
      ;;                         (bytevector-u8-ref from i))))

      (define (bytevector-copy-partial! src src-start src-end dst dst-start)
        ;; Copy direction must be selected in case src and dst
        ;; are the same vector.
        ;; This is from snow's homovector -- snow-subu8vector-move!
        (if (< src-start dst-start)
            (let loop1 ((i (- src-end 1))
                        (j (- (+ dst-start (- src-end src-start)) 1)))
              (if (< i src-start)
                  dst
                  (begin
                    (bytevector-u8-set! dst j (bytevector-u8-ref src i))
                    (loop1 (- i 1)
                           (- j 1)))))
            (let loop2 ((i src-start)
                        (j dst-start))
              (if (< i src-end)
                  (begin
                    (bytevector-u8-set! dst j (bytevector-u8-ref src i))
                    (loop2 (+ i 1)
                           (+ j 1)))
                  dst))))
      )
     (else #t))


    (define bytevector-u16-set! vector-set!)
    (define bytevector-u16-ref vector-ref)
    (define bytevector-u16-length vector-length)
    (define make-u16-bytevector make-vector)

    ))
