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
SHARE=/usr/local/share
TOP=$(shell dirname $(SHARE))
PACKAGE_DIR=$(SHARE)/scheme
BIN_DIR=$(TOP)/bin
endif

ifeq "$(SCHEME)" "foment"
SHARE=/usr/local/share
TOP=$(shell dirname $(SHARE))
PACKAGE_DIR=$(SHARE)/scheme
BIN_DIR=$(TOP)/bin
endif

ifeq "$(SCHEME)" "gauche"
SHARE=/usr/local/share
TOP=$(shell dirname $(SHARE))
PACKAGE_DIR=$(SHARE)/scheme
BIN_DIR=$(TOP)/bin
endif

ifeq "$(SCHEME)" "sagittarius"
SHARE=/usr/local/share
TOP=$(shell dirname $(SHARE))
PACKAGE_DIR=$(SHARE)/scheme
BIN_DIR=$(TOP)/bin
endif



all:

links:
	rm -rf snow seth srfi
	ln -s ../snow2-packages/snow/snow snow
	ln -s ../snow2-packages/seth/seth seth
	ln -s ../r7rs-srfis/srfi srfi


build: build-$(SCHEME)

install: build install-libs install-$(SCHEME)

install-libs:
	sudo mkdir -p $(PACKAGE_DIR)
	find srfi -type f | while read I; do sudo install -D "$$I" "$(PACKAGE_DIR)/$$I"; done
	find snow -type f | while read I; do sudo install -D "$$I" "$(PACKAGE_DIR)/$$I"; done
	find seth -type f | while read I; do sudo install -D "$$I" "$(PACKAGE_DIR)/$$I"; done
	find chibi -type f | while read I; do sudo install -D "$$I" "$(PACKAGE_DIR)/$$I"; done


uninstall: uninstall-$(SCHEME) uninstall-libs

uninstall-libs:
	find srfi -type f | while read I; do sudo rm -f "$(PACKAGE_DIR)/$$I"; done
	find snow -type f | while read I; do sudo rm -f "$(PACKAGE_DIR)/$$I"; done
	find seth -type f | while read I; do sudo rm -f "$(PACKAGE_DIR)/$$I"; done
	find chibi -type f | while read I; do sudo rm -f "$(PACKAGE_DIR)/$$I"; done
	- sudo rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*/*/*
	- sudo rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*/*
	- sudo rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)/*
	sudo rmdir --ignore-fail-on-non-empty $(PACKAGE_DIR)


clean: clean-$(SCHEME)
	rm -f *~


#
# chicken
#

build-chicken:
	$(CHICKEN_COMPILER) snow2-client-chicken.scm -o snow2

# install-chicken:
# 	sudo cp ./snow2-client-chicken.scm $(BIN_DIR)/snow2

install-chicken: build-chicken
	sudo cp ./snow2 $(BIN_DIR)

uninstall-chicken:
	sudo rm -f $(BIN_DIR)/snow2

clean-chicken:
	rm -f snow2

bootstrap-chicken: links
	rm -f snow2
	make SCHEME=chicken build
	rm -rf snow seth srfi
	./snow2 -:a40 install '(seth snow2 types)'
	make SCHEME=chicken build

#
# chibi
#

build-chibi:

install-chibi:
	sudo cp ./snow2-client-chibi.scm $(BIN_DIR)/snow2

uninstall-chibi:
	sudo rm -f $(BIN_DIR)/snow2

clean-chibi:


#
# foment
#

build-foment:

install-foment:
	sudo cp ./snow2-client-foment.scm $(BIN_DIR)/snow2

uninstall-foment:
	sudo rm -f $(BIN_DIR)/snow2

clean-foment:



#
# gauche
#

build-gauche:

install-gauche:
	sudo cp ./snow2-client-gauche.scm $(BIN_DIR)/snow2

uninstall-gauche:
	sudo rm -f $(BIN_DIR)/snow2

clean-gauche:



#
# sagittarius
#

build-sagittarius:

install-sagittarius:
	sudo cp ./snow2-client-sagittarius.scm $(BIN_DIR)/snow2

uninstall-sagittarius:
	sudo rm -f $(BIN_DIR)/snow2

clean-sagittarius:

