(define-library (snow assert)
  (export snow-assert)
  (import (scheme base))
  (import (snow snowlib))
  (begin
    (define-syntax snow-assert
      (syntax-rules ()
        ((_ e)
         (if (not e)
             (error "Assertion failed" `e e)))))))
