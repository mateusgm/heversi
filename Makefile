run:
	reverskell

stop:
	kill `pidof reverskell`

restart: stop run

dependencies:
	cabal install happstack hstringtemplate

config: dependencies
	runhaskell Setup.hs configure --user

build:
	runhaskell Setup.hs build

install:
	runhaskell Setup.hs install

