# Agent Guidelines for once.el

## Build/Test Commands
- `make test` - Run all buttercup tests
- `make lint` - Run all linters (checkdoc, declare, indent, package, regexps)
- `make clean` - Remove compiled .elc and autoload files
- `make deps` - Install dependencies to sandbox
- `make texi` - Generate texinfo documentation
- Run single test file: `./makem/makem.sh test-buttercup -- tests/test-once.el`

## Code Style
- **Language**: Emacs Lisp with `lexical-binding: t` in file header
- **Naming**: Private functions/vars use `--` prefix (e.g., `once--function-p`)
- **Documentation**: All functions must have docstrings
- **Formatting**: space indentation, no tabs; max 80 chars preferred
- **Imports**: Use `require` at top of file; optional deps checked with `featurep`
- **Customization**: Use `defcustom` for user settings with `:group 'once`
- **Errors**: Use `error` for fatal errors
- **Comments**: Use `;;` or `;;;` for section headers and avoid inline comments

## Testing
- Tests are in `tests/` using buttercup framework
- Use `describe`/`it` blocks; `before-each`/`after-each` for setup

## Project Structure
- Main files: `once.el`, `once-core.el`, `once-incrementally.el`
- Integration: `once-use-package/`, `once-setup/`
