(define-library (srfi 37)
  (export option
          args-fold
          option-names
          option-required-arg?
          option-optional-arg?
          option-processor
          args-fold*
          option-argument
          option-terminator?
          option-description)
  (import (scheme base)
          (scheme case-lambda))
  (include "37.scm"))
