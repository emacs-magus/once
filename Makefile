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
