#!/usr/local/bin/chibi-scheme

(import (scheme base))
(import (scheme write))
(import (chibi io))
(import (chibi process))
(import (chibi pathname))
(import (scheme process-context))

;; (define top (path-directory (car (command-line))))
;; (load (string-append top "/snow2-client-common.scm"))
(include "snow2-client-common.scm")

(define program-and-command-line command-line)

(main-program)
