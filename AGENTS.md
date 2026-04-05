# Agent Guide

This repository is a minimal public Emacs configuration.
The primary file is `.emacs` and uses straight.el with use-package.

## Quick Orientation

- Main config: `.emacs`
- Customizations file: `~/.emacs.d/my-custom.el`
- Package manager: straight.el
- Primary macro: `use-package`

## Build, Lint, Test Commands

This repo does not define build, lint, or test scripts.
There are no Makefiles, package.json scripts, or test runners configured.
If you need validation, use Emacs itself to byte-compile or load the file.

Suggested manual commands (run from your own environment):

- Load config: `emacs -Q -l ~/.emacs`
- Byte-compile: `emacs -Q --batch -f batch-byte-compile ~/.emacs`

Single test execution:

- No test framework is configured in this repository.
- If tests are added later, document single-test commands here.

## Code Style Guidelines

Follow established Emacs Lisp conventions and the patterns in `.emacs`.

### Formatting

- Use UTF-8 but keep text ASCII when possible.
- Indentation defaults to spaces; `indent-tabs-mode` is nil globally.
- `tab-width` is 4; keep alignment consistent with Emacs Lisp defaults.
- Avoid trailing whitespace; keep blank lines intentional.
- Prefer line lengths that stay readable in standard Emacs windows.

### File Layout

- Keep bootstrap and package setup at the top of `.emacs`.
- Group packages by purpose (global settings, LSP, major modes, etc.).
- Keep related settings adjacent to the `use-package` that owns them.

### use-package Conventions

- Use `:straight t` to install packages via straight.el.
- Keep `:ensure t` as used in the file unless removing it consistently.
- Prefer `:hook` for mode hooks, `:bind` for key bindings.
- Use `:init` for setup that must run before package load.
- Use `:config` for settings that depend on the package being loaded.

### Imports and Requires

- Avoid explicit `require` unless a package needs eager loading.
- When `require` is needed, document the reason briefly in a comment.
- Rely on `use-package` autoloading where possible.

### Naming Conventions

- Use `my-` prefix for custom functions and variables.
- Keep function names kebab-case (Emacs Lisp standard).
- Use descriptive variable names; avoid single-letter names.
- Prefer `setq` for simple values, `setq-local` for buffer-local values.

### Types and Data

- Emacs Lisp is dynamically typed; prefer clear docstrings and comments.
- Use `defvar`/`defcustom` for configuration knobs when needed.
- Use `let` and `let*` to scope values tightly.

### Error Handling

- Prefer `condition-case` only when necessary.
- Avoid suppressing errors unless the config can proceed safely.
- If a package is optional, guard with `when (featurep ...)` or checks.

### Hooks and Modes

- Use `add-hook` with function symbols, not lambdas, when possible.
- Keep hook bodies small; extract to named functions when longer.
- Avoid global side effects inside mode-specific hooks.

### Key Bindings

- Use `kbd` for key specs.
- Avoid global bindings unless they are clearly intentional.
- Group bindings in `:bind` blocks in the owning `use-package`.

### Performance

- Avoid heavy work in startup; use `:defer` or hooks.
- Prefer lazy loading for rarely used packages.

## Repository-Specific Notes

- `custom-file` is set to `~/.emacs.d/my-custom.el`.
- `company` is enabled globally; keep additions compatible.
- `lsp-mode` is used for language support; prefer `lsp-deferred`.
- Tree-sitter is enabled; keep configuration aligned with it.
- TypeScript modes use tabs and a 4-column indent.

## Adding New Packages

- Add new `use-package` blocks in the relevant section.
- Keep the grouping header comments consistent.
- If a package requires external binaries, note it in a comment.

## Documentation Expectations

- Keep README minimal; place agent guidance here.
- Use comments sparingly and only for non-obvious behavior.
- Do not add long tutorial-style blocks.

## Cursor/Copilot Rules

- No `.cursor/rules`, `.cursorrules`, or `.github/copilot-instructions.md` found.
- If added later, mirror them in this file.

## Safety and Security

- This is a public repo; avoid secrets or machine-specific paths.
- Do not commit tokens, API keys, or personal data.
- Keep URLs and package sources trustworthy and minimal.

## When You Change Behavior

- Note functional changes in commit messages.
- Keep behavior predictable for batch loading.
- Avoid interactive prompts during startup.
