#
#
#

ifndef SCHEME
$(info make SCHEME=chibi <target>)
$(info make SCHEME=chicken <target>)
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


all:

build: build-$(SCHEME)

install: build install-libs install-$(SCHEME)

install-libs:
	sudo mkdir -p $(PACKAGE_DIR)/snow
	sudo cp snow/*.sld $(PACKAGE_DIR)/snow/
	sudo mkdir -p $(PACKAGE_DIR)/seth
	sudo cp seth/*.sld $(PACKAGE_DIR)/seth/

uninstall: uninstall-$(SCHEME) uninstall-libs

uninstall-libs:
	sudo rm -f $(PACKAGE_DIR)/seth/*.sld
	- sudo rmdir $(PACKAGE_DIR)/seth

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

