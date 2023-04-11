.PHONY = test 
test:
	cabal test

.PHONY = watch
watch: 
	@find homework -name '*.hs'  | entr $(MAKE)
