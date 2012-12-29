PACKAGE_CONF_PATH = $(shell ls -d cabal-dev/packages-*.conf)

all: configure build

install-deps:
	cabal-dev install-deps

configure:
	cabal-dev configure --enable-tests -fdebug

build:
	cabal-dev build

install:
	cabal-dev install -fdebug

run:
	runghc -package-conf=$(PACKAGE_CONF_PATH) examples/Cirno.hs \
		cirno@localhost test kagami@localhost

test:
	./dist/build/cirno-tests/cirno-tests
