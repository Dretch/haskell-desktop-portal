# Haskell Desktop Portal

A Haskell wrapper for the [XDG Desktop Portal](https://github.com/flatpak/xdg-desktop-portal) DBUS API. Primarily intended to support applications packaged as Flatpaks (see [monomer-flatpak-example](https://github.com/Dretch/monomer-flatpak-example)).

## Current Status
- Unstable. Functionality and API may change considerably.

## FAQs
- **Q. Why does this not use Template Haskell to generate interface code from the [XML API definitions](https://github.com/flatpak/xdg-desktop-portal/data)**?
- **A.** The Portal API does not lend itself to code generation because inputs and outputs are mostly informally defined via vardicts rather than via simple positional parameters. Also the XML definitions are LGPL, which would make this library LGPL too.

## Development Guide

### How to contribute
- If you just want to add a wrapper for an API method that is not currently supported, open a PR.
- If you want something less straightforward, open an issue to discuss it.

### General Guidelines
- Module/function/field names should mimic the underlying portal API as much as possible.
- Functions should generally be tested (see existing tests for examples).
- Functions should take records called `...Options` and return records called `...Results`.
- `...Options` records should have a `Default` instance.
- Record fields should not have unique prefixes.

### To format the source code
```bash
# This needs at least ormolu 0.5.0.0 to avoid breaking dot-record syntax
ormolu --mode inplace $(find . -name '*.hs')
```