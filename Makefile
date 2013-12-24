
include Swift/src/make.include

# All files that go into package
PKG_FILES = $(shell find Swift -not -path '*/.svn*')


PKG_FILES += Swift/DESCRIPTION
PKG_FILES += Swift/NAMESPACE 
PKG_FILES +=  Swift/src/Makefile Swift/src/make.include Swift/src/geturl.sh
PKG_FILES +=  $(shell find Swift/src/ -name '*.shasum')
PKG_FILES += $(shell find Swift/src/swift-patches -not -path '*/.svn*') 

PACKAGE_DEPS = $(PKG_FILES) Makefile

# Extract the version number from the R package description file
# There should be a line in the file of the form:
# Version: (major).(minor)

SWIFTR_VERSION=$(shell sed -n 's/^Version:[ \t]*\([0-9][0-9]*\.[0-9\.]*\)[ \t]*$$/\1/p' Swift/DESCRIPTION)


GEN_PKG_NAME=Swift_$(SWIFTR_VERSION).tar.gz
FULL_PKG=Swift_$(SWIFTR_VERSION)_full.tar.gz
CRAN_PKG=Swift_$(SWIFTR_VERSION)_cran.tar.gz


COG_SRC = cog-svn


SWIFT_SRC = $(COG_SRC)/modules/swift

SWIFT_DIST = $(SWIFT_SRC)/dist/swift-svn

SWIFT_INST = Swift/inst/swift

SWIFT_PATCH_DIR = Swift/src/swift-patches

all: $(FULL_PKG) $(CRAN_PKG)

build: 	$(FULL_PKG)

install: $(FULL_PKG)
	R CMD INSTALL $(FULL_PKG)

installcran: $(CRAN_PKG)
	R CMD INSTALL $(CRAN_PKG)


check: $(FULL_PKG)
	R CMD check $(FULL_PKG)

checkcran: $(CRAN_PKG)
	R CMD check $(CRAN_PKG)

test: install
	Rscript Swift/tests/runtests.R

testcran: installcran
	Rscript Swift/tests/runtests.R

clean: 
	rm -f ./$(FULL_PKG)
	rm -f ./$(CRAN_PKG)
	rm -rf ./$(SWIFT_INST)
	rm -rf ./$(COG_SRC)


$(CRAN_PKG): $(PACKAGE_DEPS) $(SWIFT_SRC_NAME).shasum
	- if [ -d .svn -a -x "`which svn`" ]; then svn info > Swift/svninfo; fi
	make -C Swift/src removeall
	cp $(SWIFT_SRC_NAME).shasum Swift/src
	R CMD build Swift
	mv $(GEN_PKG_NAME) $(CRAN_PKG) 

$(FULL_PKG): $(PACKAGE_DEPS) $(SWIFT_SRC)/.built
	- if [ -d .svn -a -x "`which svn`" ]; then svn info > Swift/svninfo; fi
	make -C Swift/src removebuild # remove everything aside from final Swift
	mkdir -p Swift/inst
	rm -Rf ./$(SWIFT_INST)
	cp -r $(SWIFT_DIST) $(SWIFT_INST)
	- if [ -d .svn -a -x "`which svn`" ]; then svn info > Swift/svninfo; fi
	R CMD build Swift
	mv $(GEN_PKG_NAME) $(FULL_PKG) 


swiftsrc: $(SWIFT_SRC_NAME).shasum $(SWIFT_SRC_NAME).tar.gz

$(SWIFT_SRC_NAME).tar.gz:
	sh checkout-swift.sh $(SWIFT_SRC_NAME) $(SWIFT_PATCH_DIR)/$(SWIFT_SRC_PATCH) $(SWIFT_SRC_TAG) $(COG_SRC_TAG)
	cp -r $(SWIFT_PATCH_DIR) $(SWIFT_SRC_NAME)/SwiftR-patches
	tar --exclude='.svn' -cvvzf  $(SWIFT_SRC_NAME).tar.gz $(SWIFT_SRC_NAME)
	rm -rf ./$(SWIFT_SRC_NAME)

$(SWIFT_SRC_NAME).shasum: $(SWIFT_SRC_NAME).tar.gz
	shasum $(SWIFT_SRC_NAME).tar.gz > $(SWIFT_SRC_NAME).shasum


	

$(SWIFT_SRC)/.built: $(COG_SRC)/.checkedout
	(cd $(SWIFT_SRC) && ant dist && touch .built)

$(COG_SRC)/.checkedout: checkout-swift.sh
	sh checkout-swift.sh $(COG_SRC) $(SWIFT_PATCH_DIR)/$(SWIFT_SRC_PATCH) $(SWIFT_SRC_TAG) $(COG_SRC_TAG)
	touch $(COG_SRC)/.checkedout

checkversion: 
	echo SwiftR version is $(SWIFTR_VERSION) according to Swift/DESCRIPTION file


