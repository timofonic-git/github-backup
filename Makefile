PREFIX=/usr
CABAL?=cabal # set to "./Setup" if you lack a cabal program

build: Build/SysConfig.hs
	$(CABAL) build
	ln -sf dist/build/github-backup/github-backup github-backup

Build/SysConfig.hs: configure.hs Build/TestConfig.hs Build/Configure.hs
	if [ "$(CABAL)" = ./Setup ]; then ghc --make Setup; fi
	$(CABAL) configure

install: build
	install -d $(DESTDIR)$(PREFIX)/bin
	install github-backup $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 github-backup.1 $(DESTDIR)$(PREFIX)/share/man/man1

clean:
	rm -rf github-backup dist configure Build/SysConfig.hs Setup
	find -name \*.o -exec rm {} \;
	find -name \*.hi -exec rm {} \;

# Upload to hackage.
hackage: clean
	./make-sdist.sh
	@cabal upload dist/*.tar.gz
