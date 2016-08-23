PREFIX=/usr
CABAL?=cabal # set to "./Setup" if you lack a cabal program

build: Build/SysConfig.hs
	$(CABAL) build
	ln -sf dist/build/github-backup/github-backup github-backup
	ln -sf dist/build/gitriddance/gitriddance gitriddance
	@$(MAKE) tags >/dev/null 2>&1 &

Build/SysConfig.hs: configure.hs Build/TestConfig.hs Build/Configure.hs
	if [ "$(CABAL)" = ./Setup ]; then ghc --make Setup; fi
	$(CABAL) configure --ghc-options="$(shell Build/collect-ghc-options.sh)"

install: build
	install -d $(DESTDIR)$(PREFIX)/bin
	install github-backup gitriddance $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 github-backup.1 gitriddance.1 $(DESTDIR)$(PREFIX)/share/man/man1
	install -d $(DESTDIR)$(PREFIX)/share/bash-completion/completions
	./github-backup --bash-completion-script github-backup > $(DESTDIR)$(PREFIX)/share/bash-completion/completions/github-backup

clean:
	rm -rf github-backup gitriddance dist configure Build/SysConfig.hs Setup tags
	find -name \*.o -exec rm {} \;
	find -name \*.hi -exec rm {} \;

# Upload to hackage.
hackage: clean
	@cabal sdist
	@cabal upload dist/*.tar.gz

# hothasktags chokes on some template haskell etc, so ignore errors
tags:
	find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags > tags 2>/dev/null
