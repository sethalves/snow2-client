.POSIX:
.DELETE_ON_ERROR:
#
#
#

CHICKEN_COMPILER=csc -X r7rs

ifeq "$(SCHEME)" "chicken"
PACKAGE_DIR=$(DESTDIR)/usr/share/scheme/snow2-chicken
BIN_DIR=$(DESTDIR)/usr/local/bin
EGGS=$(shell chicken-status -e)
  ifneq ($(findstring srfi-27, $(EGGS)), srfi-27)
    $(error 'missing srfi-27 egg, try: make chicken-deps')
  endif
  ifneq ($(findstring srfi-29, $(EGGS)), srfi-29)
    $(error 'missing srfi-29 egg, try: make chicken-deps')
  endif
  ifneq ($(findstring srfi-37, $(EGGS)), srfi-37)
    $(error 'missing srfi-37 egg, try: make chicken-deps')
  endif
  ifneq ($(findstring http-client, $(EGGS)), http-client)
    $(error 'missing http-client egg, try: make chicken-deps')
  endif
  ifneq ($(findstring openssl, $(EGGS)), openssl)
    $(error 'missing openssl egg, try: make chicken-deps')
  endif
  ifneq ($(findstring udp, $(EGGS)), udp)
    $(error 'missing udp egg, try: make chicken-deps')
  endif
  ifneq ($(findstring r7rs, $(EGGS)), r7rs)
    $(error 'missing r7rs egg, try: make chicken-deps')
  endif
endif

ifeq "$(SCHEME)" "chibi"
PACKAGE_DIR=$(DESTDIR)/usr/share/scheme/snow2-chibi
BIN_DIR=$(DESTDIR)/usr/local/bin
endif

ifeq "$(SCHEME)" "foment"
PACKAGE_DIR=$(DESTDIR)/usr/share/scheme/snow2-foment
BIN_DIR=$(DESTDIR)/usr/local/bin
endif

ifeq "$(SCHEME)" "gauche"
PACKAGE_DIR=$(DESTDIR)/usr/share/scheme/snow2-gauche
BIN_DIR=$(DESTDIR)/usr/local/bin
endif

ifeq "$(SCHEME)" "kawa"
PACKAGE_DIR=$(DESTDIR)/usr/share/scheme/snow2-kawa
BIN_DIR=$(DESTDIR)/usr/local/bin
endif

ifeq "$(SCHEME)" "sagittarius"
PACKAGE_DIR=$(DESTDIR)/usr/local/share/scheme/snow2-sagittarius
BIN_DIR=$(DESTDIR)/usr/local/bin
endif

.PHONY: all
all: build

.PHONY: build
build: build-$(SCHEME)

.PHONY: install
install: build install-$(SCHEME)

.PHONY: install-libs
install-libs:
	mkdir -p $(PACKAGE_DIR)
	find srfi -type f | while read I; do ./build/install "$$I" -d "$(PACKAGE_DIR)/$$(dirname $$I)"; done
	find snow -type f | while read I; do ./build/install "$$I" -d "$(PACKAGE_DIR)/$$(dirname $$I)"; done
	find seth -type f | while read I; do ./build/install "$$I" -d "$(PACKAGE_DIR)/$$(dirname $$I)"; done
	find chibi -type f | while read I; do ./build/install "$$I" -d "$(PACKAGE_DIR)/$$(dirname $$I)"; done

.PHONY: uninstall
uninstall: uninstall-$(SCHEME)

.PHONY: uninstall-libs
uninstall-libs:
	find srfi -type f | while read I; do $(RM) "$(PACKAGE_DIR)/$$I"; done
	find snow -type f | while read I; do $(RM) "$(PACKAGE_DIR)/$$I"; done
	find seth -type f | while read I; do $(RM) "$(PACKAGE_DIR)/$$I"; done
	find chibi -type f | while read I; do $(RM) "$(PACKAGE_DIR)/$$I"; done
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*/*/*
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*/*
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*

.PHONY: clean
clean: clean-chibi clean-chicken \
       clean-foment clean-gauche \
       clean-kawa clean-sagittarius 
	$(RM) *~

bootstrap: bootstrap-$(SCHEME)

links:
	rm -rf snow seth srfi
	ln -s ../snow2-packages/snow/snow snow
	ln -s ../snow2-packages/seth/seth seth
	ln -s ../r7rs-srfis/srfi srfi


debs:
	dpkg-buildpackage -b -rfakeroot -A -k'seth@hungry.com'

debs-clean:
	debuild clean


#	fakeroot dpkg-buildpackage -b
#	dpkg-buildpackage -b -rfakeroot


#
# chibi
#

build-chibi:

install-chibi: install-libs
	mkdir -p $(BIN_DIR)
	install ./snow2-client-chibi.scm $(PACKAGE_DIR)/snow2
	rm -f $(BIN_DIR)/snow2
	ln -s $(PACKAGE_DIR)/snow2 $(BIN_DIR)/snow2

uninstall-chibi: uninstall-libs
	rm -f $(BIN_DIR)/snow2
	rm -f $(PACKAGE_DIR)/snow2
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)

clean-chibi:


#
# chicken
#

chicken-deps:
	chicken-install srfi-19 srfi-27 srfi-29 srfi-37 srfi-95 http-client openssl udp r7rs ssax sxpath hmac sha1

snow2-client-chicken: snow2-client-chicken.scm snow seth srfi chibi
	$(CHICKEN_COMPILER) snow2-client-chicken.scm -o snow2-client-chicken

build-chicken: snow2-client-chicken


install-chicken: build-chicken
	mkdir -p $(BIN_DIR)
	rm -f $(BIN_DIR)/snow2
	install ./snow2-client-chicken $(BIN_DIR)/snow2

uninstall-chicken:
	rm -f $(BIN_DIR)/snow2

clean-chicken:
	rm -f snow2-client-chicken snow2-client-chicken.c

bootstrap-chicken-x: clean-chicken links build-chicken

bootstrap-chicken: bootstrap-chicken-x
	rm -rf snow seth srfi
	./snow2-client-chicken -:a40 install '(seth snow2 types)'
	make SCHEME=chicken build

#
# foment
#

build-foment:

install-foment: install-libs
	mkdir -p $(BIN_DIR)
	install ./snow2-client-foment.scm $(PACKAGE_DIR)/snow2
	rm -f $(BIN_DIR)/snow2
	ln -s $(PACKAGE_DIR)/snow2 $(BIN_DIR)/snow2

uninstall-foment: uninstall-libs
	rm -f $(BIN_DIR)/snow2
	rm -f $(PACKAGE_DIR)/snow2
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)

clean-foment:


#
# gauche
#

build-gauche:

install-gauche: install-libs
	mkdir -p $(BIN_DIR)
	install ./snow2-client-gauche.scm $(PACKAGE_DIR)/snow2
	rm -f $(BIN_DIR)/snow2
	ln -s $(PACKAGE_DIR)/snow2 $(BIN_DIR)/snow2

uninstall-gauche: uninstall-libs
	rm -f $(BIN_DIR)/snow2
	rm -f $(PACKAGE_DIR)/snow2
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)

clean-gauche:


#
# kawa
#

build-kawa:
	KAWALIB=$${KAWALIB-/usr/local/share/java/kawa-2.0.1.jar} \
	CLASSPATH="$${KAWALIB}:$${DIR}:$${CLASSPATH}" \
	java -Xss4096k \
	  -Dkawa.import.path="./*.sld" -Dkawa.include.path='|:.' \
	  kawa.repl -C snow2-client-kawa.scm

install-kawa: build-kawa
	mkdir -p $(PACKAGE_DIR)
	find . -name *\.class | \
	  while read I; do install -D "$$I" "$(PACKAGE_DIR)/$$I"; done
	install ./snow2-client-kawa.scm $(PACKAGE_DIR)/snow2
	rm -f $(BIN_DIR)/snow2
	ln -s $(PACKAGE_DIR)/snow2 $(BIN_DIR)/snow2

uninstall-kawa: build-kawa
	rm -f $(BIN_DIR)/snow2
	rm -f $(PACKAGE_DIR)/snow2
	find . -name \*.class | \
	  while read I; do rm -f "$(PACKAGE_DIR)/$$I"; done
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*/*/*
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*/*
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)

clean-kawa:
	find . -name \*.class | while read I; do rm "$$I"; done

#
# sagittarius
#

build-sagittarius:

install-sagittarius: install-libs
	mkdir -p $(BIN_DIR)
	cp ./snow2-client-sagittarius.scm snow2
	./build/install snow2 -d $(PACKAGE_DIR)
	$(RM) $(BIN_DIR)/snow2
	ln -s $(PACKAGE_DIR)/snow2 $(BIN_DIR)/snow2

uninstall-sagittarius: uninstall-libs
	$(RM) $(BIN_DIR)/snow2
	$(RM) $(PACKAGE_DIR)/snow2
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)

clean-sagittarius:


.DEFAULT:
	$(info make SCHEME=chibi <target>)
	$(info make SCHEME=chicken <target>)
	$(info make SCHEME=foment <target>)
	$(info make SCHEME=gauche <target>)
	$(info make SCHEME=kawa <target>)
	$(info make SCHEME=sagittarius <target>)
	$(error <target> should be one of: build clean install uninstall)


# https://www.debian.org/doc/manuals/maint-guide/dreq.en.html#rules

distclean: clean

test:

check:
