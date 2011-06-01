run:
	runhaskell -XFlexibleInstances Main.hs &

stop:
	kill `pidof runghc`
	kill `pidof ghc`

restart: stop run
