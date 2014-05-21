(define-library (snow assert)
  (export snow-assert)
  (import (scheme base))
  (begin
    (define-syntax snow-assert
      (syntax-rules ()
        ((_ e)
         (if (not e)
             (error "Assertion failed" `e e)))))))
