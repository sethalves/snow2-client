(define-library (snow assert)
  (export snow-assert)
  (import (scheme base))
  (import (snow snowlib))
  (cond-expand
   (chibi)
   ;; (chicken (import (only (chicken) assert)))
   (chicken)
   (gauche)
   (sagittarius))
  (begin
    (cond-expand
     ((or chibi
          chicken
          gauche
          sagittarius)
      (define-syntax snow-assert
        (syntax-rules ()
          ((_ e)
           (if (not e)
               (snow-raise
                (make-snow-error-condition "Assertion failed" `e))
               ;; (print "Assertion failed:" `e)
               )))))
     )))
