run:
	reverskell +RTS -A4M -N4 -qg0 -qb -g1 &

stop:
	kill `pidof reverskell`

restart: stop run

dependencies:
	cabal install happstack hstringtemplate

config:
	runhaskell Setup.hs configure --user

build:
	runhaskell Setup.hs build

install:
	runhaskell Setup.hs install

