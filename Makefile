#
#
#

ifndef SCHEME
$(info make SCHEME=chibi <target>)
$(info make SCHEME=chicken <target>)
$(info make SCHEME=foment <target>)
$(info make SCHEME=gauche <target>)
$(info make SCHEME=sagittarius <target>)
$(info <target> should be one of: build clean install uninstall)
endif

ifeq "$(SCHEME)" "chicken"
CHICKEN_HOME=$(shell csi -e '(display (chicken-home))')
SHARE=$(shell dirname $(CHICKEN_HOME))
TOP=$(shell dirname $(SHARE))
PACKAGE_DIR=$(SHARE)/scheme
BIN_DIR=$(TOP)/bin
CHICKEN_COMPILER=csc -X r7rs -I $(PACKAGE_DIR)
endif

ifeq "$(SCHEME)" "chibi"
PACKAGE_DIR=/usr/local/share/snow2-chibi
BIN_DIR=/usr/local/bin
endif

ifeq "$(SCHEME)" "foment"
PACKAGE_DIR=/usr/local/share/snow2-foment
BIN_DIR=/usr/local/bin
endif

ifeq "$(SCHEME)" "gauche"
PACKAGE_DIR=/usr/local/share/snow2-gauche
BIN_DIR=/usr/local/bin
endif

ifeq "$(SCHEME)" "sagittarius"
PACKAGE_DIR=/usr/local/share/snow2-sagittarius
BIN_DIR=/usr/local/bin
endif


all: build

links:
	rm -rf snow seth srfi
	ln -s ../snow2-packages/snow/snow snow
	ln -s ../snow2-packages/seth/seth seth
	ln -s ../r7rs-srfis/srfi srfi


build: build-$(SCHEME)

install: build install-$(SCHEME)

install-libs:
	mkdir -p $(PACKAGE_DIR)
	find srfi -type f | while read I; do install -D "$$I" "$(PACKAGE_DIR)/$$I"; done
	find snow -type f | while read I; do install -D "$$I" "$(PACKAGE_DIR)/$$I"; done
	find seth -type f | while read I; do install -D "$$I" "$(PACKAGE_DIR)/$$I"; done
	find chibi -type f | while read I; do install -D "$$I" "$(PACKAGE_DIR)/$$I"; done


uninstall: uninstall-$(SCHEME)

uninstall-libs:
	find srfi -type f | while read I; do rm -f "$(PACKAGE_DIR)/$$I"; done
	find snow -type f | while read I; do rm -f "$(PACKAGE_DIR)/$$I"; done
	find seth -type f | while read I; do rm -f "$(PACKAGE_DIR)/$$I"; done
	find chibi -type f | while read I; do rm -f "$(PACKAGE_DIR)/$$I"; done
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*/*/*
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*/*
	- rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*


clean: clean-$(SCHEME)
	rm -f *~


#
# chibi
#

build-chibi:

install-chibi: install-libs
	cp ./snow2-client-chibi.scm $(PACKAGE_DIR)/snow2
	rm -f $(BIN_DIR)/snow2
	ln -s $(PACKAGE_DIR)/snow2 $(BIN_DIR)/snow2

uninstall-chibi: uninstall-libs
	rm -f $(BIN_DIR)/snow2
	rm -f $(PACKAGE_DIR)/snow2
	rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)

clean-chibi:


#
# chicken
#

snow2-client-chicken: snow2-client-chicken.scm snow seth srfi chibi
	$(CHICKEN_COMPILER) snow2-client-chicken.scm -o snow2-client-chicken

build-chicken: snow2-client-chicken

install-chicken: build-chicken
	install ./snow2-client-chicken $(BIN_DIR)/snow2

uninstall-chicken:
	rm -f $(BIN_DIR)/snow2

clean-chicken:
	rm -f snow2

bootstrap-chicken: links
	rm -f snow2
	make SCHEME=chicken build
	rm -rf snow seth srfi
	./snow2 -:a40 install '(seth snow2 types)'
	make SCHEME=chicken build

#
# foment
#

build-foment:

install-foment: install-libs
	cp ./snow2-client-foment.scm $(PACKAGE_DIR)/snow2
	rm -f $(BIN_DIR)/snow2
	ln -s $(PACKAGE_DIR)/snow2 $(BIN_DIR)/snow2

uninstall-foment: uninstall-libs
	rm -f $(BIN_DIR)/snow2
	rm -f $(PACKAGE_DIR)/snow2
	rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)

clean-foment:


#
# gauche
#

build-gauche:

install-gauche: install-libs
	cp ./snow2-client-gauche.scm $(PACKAGE_DIR)/snow2
	rm -f $(BIN_DIR)/snow2
	ln -s $(PACKAGE_DIR)/snow2 $(BIN_DIR)/snow2

uninstall-gauche: uninstall-libs
	rm -f $(BIN_DIR)/snow2
	rm -f $(PACKAGE_DIR)/snow2
	rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)

clean-gauche:


#
# sagittarius
#

build-sagittarius:

install-sagittarius: install-libs
	cp ./snow2-client-sagittarius.scm $(PACKAGE_DIR)/snow2
	rm -f $(BIN_DIR)/snow2
	ln -s $(PACKAGE_DIR)/snow2 $(BIN_DIR)/snow2

uninstall-sagittarius: uninstall-libs
	rm -f $(BIN_DIR)/snow2
	rm -f $(PACKAGE_DIR)/snow2
	rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)

clean-sagittarius:
