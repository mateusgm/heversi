all: clean config build install run 

run:
	reverskell +RTS -A256M -N4 -qg0 -qb0 &

# -N: number of threads
# -A: GC allocation area size
# -qbX: enables load balancing for GC generation X or disable it at all (no X)
# -qgX: use parallel GC in generation X or disable it at all (no X)

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

clean:
	rm -rf ~/.cabal/bin/reverskell dist/build dist/src


