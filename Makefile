#
#
#

CHICKEN_COMPILER=csc

all:

compile-chicken:
	$(CHICKEN_COMPILER) snow2-client-chicken.scm -o snow2-client-chicken

clean:
	rm -f snow2-client-chicken *~
