PREFIX=/usr
BASEFLAGS=-Wall -outputdir tmp
GHCFLAGS=-O2 $(BASEFLAGS)
bins=github-backup
mans=github-backup.1
all=$(bins)

ifdef PROFILE
GHCFLAGS=-prof -auto-all -rtsopts -caf-all -fforce-recomp $(BASEFLAGS)
endif

GHCMAKE=ghc $(GHCFLAGS) --make

# Am I typing :make in vim? Do a fast build.
ifdef VIM
all=fast
endif

all: $(all)

# Disables optimisation. Not for production use.
fast: GHCFLAGS=$(BASEFLAGS)
fast: $(bins)

$(bins):
	$(GHCMAKE) $@

install: all
	install -d $(DESTDIR)$(PREFIX)/bin
	install $(bins) $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 $(mans) $(DESTDIR)$(PREFIX)/share/man/man1

clean:
	rm -rf $(bins) tmp

# Upload to hackage.
hackage: clean
	@cabal sdist
	@cabal upload dist/*.tar.gz

.PHONY: $(bins)
