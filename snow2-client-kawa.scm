#!/bin/bash
#| -*- scheme -*-
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
KAWALIB=${KAWALIB-/usr/local/share/java/kawa-2.0.1.jar}
CLASSPATH="${KAWALIB}:${DIR}:${CLASSPATH}"
export CLASSPATH
exec java \
     -Xss4096k \
     -Dkawa.import.path="./*.sld" \
     -Dkawa.include.path='|:.' \
     kawa.repl $SOURCE "$@"
|#

(import (scheme base) (prefix (seth snow2 client) snow2-))
(snow2-main-program)
