start:
	runhaskell Main.hs &
  
stop:
	kill `pidof runghc`
	kill `pidof ghc`

restart: stop start