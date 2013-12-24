# This is a makefile that checks outs and builds Swift
# This is done to allow distribution of a pure source-only Swift package
# A fresh copy of the source is only checked out if swift isn't included
# in the package.
include make.include

ANT_DIR = apache-ant-1.8.2
ANT_PKG = apache-ant-1.8.2-bin.tar.bz2
ANT_PKG_SHASUM = apache-ant-1.8.2-bin.shasum

ANT_PKG_URL = http://www.apache.org/dist/ant/binaries/$(ANT_PKG)

SWIFT_MODULE_SRC = $(SWIFT_SRC_NAME)/modules/swift

SWIFT_DIST = $(SWIFT_MODULE_SRC)/dist/swift-svn

SWIFT_INST = ../inst/swift

SWIFT_BIN = $(SWIFT_INST)/bin/swift


SHASUM = $(shell if hash shasum &> /dev/null ; then echo shasum ; else echo sha1sum; fi)

all: $(SWIFT_BIN)

$(SWIFT_SRC_NAME)/.downloadedok: 
	# hack to avoid redoing
	if [ ! -f $(SWIFT_BIN) ]; then \
	  rm -f $(SWIFT_SRC_NAME).tar.gz && \
	  sh geturl.sh "http://people.cs.uchicago.edu/~tga/swiftR/swift-source/$(SWIFT_SRC_NAME).tar.gz" && \
    	  $(SHASUM) --check ${SWIFT_SRC_NAME}.shasum && \
	  tar xvzf ${SWIFT_SRC_NAME}.tar.gz && \
	  touch $(SWIFT_SRC_NAME)/.downloadedok ; \
	fi


$(SWIFT_MODULE_SRC)/.built: $(SWIFT_SRC_NAME)/.downloadedok $(ANT_DIR)/.downloadedok
	# hack to avoid rebuilding
	if [ ! -f $(SWIFT_BIN) ]; then \
	  java -Dant.home="$(pwd)/$(ANT_DIR)" -classpath $(ANT_DIR)/lib/ant-launcher.jar \
	  	org.apache.tools.ant.launch.Launcher  -f $(SWIFT_MODULE_SRC)/build.xml dist && \
	  touch $(SWIFT_MODULE_SRC)/.built; \
	fi

# Target to make sure that swift exists
$(SWIFT_BIN): $(SWIFT_MODULE_SRC)/.built
	# hack to avoid recopying
	if [ ! -f $(SWIFT_BIN) ]; then \
	  mkdir -p ../inst && \
	  rm -Rf ./$(SWIFT_INST) && \
	  cp -r $(SWIFT_DIST) $(SWIFT_INST); \
	fi

removeall: removebuild
	rm -rf ./$(SWIFT_INST)
	rm -rf ../inst


removebuild:
	rm -rf ./$(ANT_DIR) ./$(ANT_PKG)
	rm -rf ./$(SWIFT_SRC_NAME)
	rm -rf ./$(SWIFT_SRC_NAME).tar.gz
	

fetchant: $(ANT_DIR)/.downloadedok

$(ANT_DIR)/.downloadedok:
	# hack to avoid redoing
	if [ ! -f $(SWIFT_BIN) ]; then \
	    rm -f $(ANT_PKG) && \
	    sh geturl.sh $(ANT_PKG_URL) && \
	    $(SHASUM) --check $(ANT_PKG_SHASUM) && \
	    rm -rf $(ANT_DIR) && \
	    tar xjf $(ANT_PKG) && \
	    touch $(ANT_DIR)/.downloadedok; \
	fi
