
# AUTOBUILD_START
# DO NOT EDIT (digest: be1ced7f09a327f0833049658928e932)

SETUP = ocaml setup.ml

BUILDFLAGS     =
DOCFLAGS       =
TESTFLAGS      =
CLEANFLAGS     =
DISTCLEANFLAGS =
CONFIGUREFLAGS =
INSTALLFLAGS   =

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data
	$(SETUP) -test $(TESTFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data: 
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test clean distclean install

# AUTOBUILD_STOP
