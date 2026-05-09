## Project Structure

- `init.el` — Entry point, loads core and modules in order
- `core/` — Core layer: `core.el` → `core-defvar` → `core-basic` → `core-package` → `core-keybinds`
- `modules/` — Feature modules (editor / tools / langs), loaded from `init.el`
  - `modules/langs/` — Language-specific modules (cc, python, rust, golang, etc.)
  - `modules/autoload/` — Module-level autoload definitions
- `lib/` — ~190 git submodule packages managed by Borg
- `Makefile` — Build entry point, includes `lib/borg/borg.mk`

## Conventions

- Configuration style: declarative `use-package` (from `lib/use-package`)
- Module file naming: `init-<feature>.el`
- New features: create a module file first, then add the load line in `init.el`
- Language support goes under `modules/langs/`
- Autoload definitions go in corresponding files under `modules/autoload/`
- Keybindings are managed centrally in `modules/init-keybindings.el`

## Common Commands

### Borg Build

- `make build` — Clean init bytecode, compile all drone packages and init files
- `make native` — Same as build but with native-comp for drones
- `make quick` — Clean then build partial drones and init files
- `make redo` — `make clean` + `make build`, full rebuild
- `make clean` — Remove all bytecode and native compilation artifacts
- `make init-build` — Compile only init files (tangles init.org first if present)
- `make init-clean` — Remove only init file bytecode
- `make build/DRONE` — Compile a single package, e.g. `make build/vertico`
- `make native/DRONE` — Native-compile a single package

### Initialization & Updates

- `make bootstrap-borg` — First-time clone of borg into lib/
- `make bootstrap` — Full initialization: submodule init → clone → checkout → build
- `make clone-modules` — Clone all uninitialized submodules
- `make checkout-modules` — Checkout all submodules to correct branches
- `bash subup.sh` — Batch update all git submodules to latest versions

### Cleanup & Checks

- `make clean-all` — Clean compilation caches, autoloads, and local build artifacts (vterm/sqlite3/emacsql)
- `make codespell-dry` — Spell check (dry run)
- `make codespell-fix` — Spell check with auto-fix

## Notes

- Do **NOT** modify anything under `lib/` — those are third-party packages managed as git submodules.
