#
#
#

CHICKEN_COMPILER=csc

ifndef SCHEME
$(info make SCHEME=chibi <target>)
$(info make SCHEME=chicken <target>)
$(info <target> should be one of: build clean install uninstall)
endif

ifeq "$(SCHEME)" "chicken"
CHICKEN_HOME=$(shell csi -e '(display (chicken-home))')
SHARE=$(shell dirname $(CHICKEN_HOME))
TOP=$(shell dirname $(SHARE))
PACKAGE_DIR=$(SHARE)/scheme
BIN_DIR=$(TOP)/bin
endif


ifeq "$(SCHEME)" "chibi"
SHARE=/usr/local/share
TOP=$(shell dirname $(SHARE))
PACKAGE_DIR=$(SHARE)/scheme
BIN_DIR=$(TOP)/bin
endif


all:


#
# build
#

build: build-$(SCHEME)

build-chicken:
	$(CHICKEN_COMPILER) snow2-client-chicken.scm -o snow2

build-chibi:

#
# install
#

install: install-$(SCHEME)

install-libs:
	sudo mkdir -p $(PACKAGE_DIR)/seth
	sudo cp seth/bytevector.sld $(PACKAGE_DIR)/seth/
	sudo cp seth/tar.sld $(PACKAGE_DIR)/seth/
	sudo cp seth/snow2-utils.sld $(PACKAGE_DIR)/seth/

install-chicken: build-chicken install-libs
	sudo cp ./snow2 $(BIN_DIR)

install-chibi: build-chibi install-libs
	sudo cp ./snow2-client-chibi.scm $(BIN_DIR)/snow2

#
# uninstall
#

uninstall: uninstall-$(SCHEME)

uninstall-libs:
	sudo rm -f $(PACKAGE_DIR)/seth/bytevector.sld
	sudo rm -f $(PACKAGE_DIR)/seth/tar.sld
	sudo rm -f $(PACKAGE_DIR)/seth/snow2-utils.sld
	- sudo rmdir $(PACKAGE_DIR)/seth

uninstall-chicken: uninstall-libs
	sudo rm -f $(BIN_DIR)/snow2

uninstall-chibi: uninstall-libs
	sudo rm -f $(BIN_DIR)/snow2

#
# clean
#

clean:
	rm -f snow2 *~
