#! /bin/sh
#| -*- scheme -*-
exec chibi-scheme -I /usr/local/share/scheme -s $0 "$@"
|#

(import (scheme base) (prefix (seth snow2-utils) snow2-))
(snow2-main-program)
