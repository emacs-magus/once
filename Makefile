EMACS ?= emacs
SANDBOX_DIR ?= ./sandbox

.PHONY: deps
deps:
	@mkdir -p "$(SANDBOX_DIR)"
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv --emacs "$(EMACS)" --sandbox="$(SANDBOX_DIR)" \
			--install-deps --install-linters

.PHONY: test
test: test-main test-aliases
	@echo "All tests completed."

# run main tests excluding alias tests
.PHONY: test-main
test-main: clean
	@echo "Using $(shell which $(EMACS))..."
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv --emacs "$(EMACS)" --sandbox="$(SANDBOX_DIR)" \
			--no-compile --exclude "tests/test-once-setup-aliases.el" \
			--exclude "tests/test-once-use-package-aliases.el" test

# run alias tests separately (these don't work if other features were already
# loaded)
.PHONY: test-aliases
test-aliases: clean
	@echo "Using $(shell which $(EMACS))..."
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv --emacs "$(EMACS)" --sandbox="$(SANDBOX_DIR)" \
			--no-compile \
			--exclude "tests/test-once.el" \
			--exclude "tests/test-once-setup.el" \
			--exclude "tests/test-once-use-package.el" \
			--file "tests/test-once-setup-aliases.el" \
			--file "tests/test-once-use-package-aliases.el" test

# TODO allow multiple main files
.PHONY: lint
lint:
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv --emacs "$(EMACS)" --sandbox="$(SANDBOX_DIR)"

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
