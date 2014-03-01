#! /bin/sh
#| -*- scheme -*-
exec csi -include-path /usr/local/share/scheme -s $0 "$@"
|#

(use r7rs)
;; (import-for-syntax r7rs)
(use srfi-69)
(require-library scheme.process-context)
(include "snow/snowlib.sld")
(include "snow/srfi-60-integers-as-bits.sld")
(include "snow/bytevector.sld")
(include "snow/binio.sld")
(include "snow/bignum.sld")
(include "snow/random.sld")
(include "snow/srfi-13-strings.sld")
(include "snow/filesys.sld")
(include "snow/digest.sld")
(include "snow/genport.sld")
(include "snow/zlib.sld")
(include "snow/tar.sld")
(include "seth/srfi-37-argument-processor.sld")
(include "seth/srfi-69-hash-tables.sld")
(include "seth/temporary-file.sld")
(include "seth/quoted-printable.sld")
(include "seth/base64.sld")
(include "seth/mime.sld")
(include "seth/http.sld")
(include "seth/string-read-write.sld")
(include "seth/snow2-utils.sld")
(import (prefix (seth snow2-utils) snow2-))

(snow2-main-program)
