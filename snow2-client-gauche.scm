#! /bin/sh
#| -*- scheme -*-
exec gosh \
-e '(push! *load-suffixes* ".sld")' \
-e '(push! *load-path* ".")' \
-e '(push! *load-path* "/usr/local/share/scheme")' \
-r7 $0 "$@"
|#

(import (scheme base) (prefix (seth snow2 client) snow2-))
(snow2-main-program)
