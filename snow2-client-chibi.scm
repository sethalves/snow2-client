#! /bin/sh
#| -*- scheme -*-
exec chibi-scheme -A /usr/local/share/scheme -A . -s $0 "$@"
|#

;; CHIBI_MODULE_PATH="$CHIBI_MODULE_PATH" exec chibi-scheme -A /usr/local/share/scheme -A . -s $0 "$@"

(import (scheme base)
        (prefix (seth snow2 client) snow2-))
(snow2-main-program)
