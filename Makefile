EMACS ?= emacs
SANDBOX_DIR ?= ./sandbox

.PHONY: deps
deps:
	@mkdir -p "$(SANDBOX_DIR)"
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv --emacs "$(EMACS)" --sandbox="$(SANDBOX_DIR)" \
			--install-deps --install-linters

.PHONY: test
test: clean
	@echo "Using $(shell which $(EMACS))..."
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv --emacs "$(EMACS)" --sandbox="$(SANDBOX_DIR)" \
			--no-compile test

# TODO allow multiple main files
.PHONY: lint
lint:
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv --emacs "$(EMACS)" --sandbox="$(SANDBOX_DIR)" \
			--exclude once-setup lint

.PHONY: clean
clean:
	rm -f -- *.elc **/*.elc *-autoloads.el **/*-autoloads.el *\~ **/*\~

.PHONY: texi
texi:
	@# put index version of readme in texi folder
	@mkdir -p texi
	@git show :README.org > once.org
	@rm -f once.texi
	@# NOTE a pre-commit hook will fail if wrap this line
	./makem/makem.sh -vv --emacs "$(EMACS)" \
		--sandbox="$(SANDBOX_DIR)" batch -- once.org -l ox-extra \
			 -f org-texinfo-export-to-texinfo
	@# add missing final newline
	@echo >> once.texi
	@rm -f once.org
