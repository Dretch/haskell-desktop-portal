# Haskell Desktop Portal

A Haskell wrapper for the [XDG Desktop Portal](https://github.com/flatpak/xdg-desktop-portal) DBUS API. Like [libportal](https://github.com/flatpak/libportal), but written in Haskell. Primarily intended to support applications packaged as Flatpaks (see [monomer-flatpak-example](https://github.com/Dretch/monomer-flatpak-example)).

## Current Status
- Unstable. Functionality and API may change considerably.

## FAQs
- **Q. Why does this not use Template Haskell to generate interface code from the [XML API definitions](https://github.com/flatpak/xdg-desktop-portal/data)**?
- **A.** The Portal API does not lend itself to code generation because inputs and outputs are mostly informally defined via vardicts rather than via simple positional parameters. Also the XML definitions are LGPL, which would make this library LGPL too.

## API Documentation

See the generated docs on [Hackage](https://hackage.haskell.org/package/desktop-portal).

## Example Code

The [monomer-flatpak-example](https://github.com/Dretch/monomer-flatpak-example) app includes example code for many of the APIs.

## Development Guide

### How to contribute
- If you just want to add a wrapper for an API method that is not currently supported, open a PR.
- If you want something less straightforward, open an issue to discuss it.

### General Guidelines
- Module/function/field names should mimic the underlying portal API as much as possible.
- Functions should generally be tested (see existing tests for examples).
- API methods that take or return a variable set of values via a vardict should be translated into Haskell functions that take records called `...Options` and return records called `...Results`.
- `...Options` records should have a `Default` instance where all fields have a reasonable empty value.
- Record fields should not have unique prefixes.
- API methods that return URIs that are known to always be `file:` URIs should be wrapped with functions returning [`Prelude.FilePath`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#t:FilePath) (at some point this can be replaced with [`System.OsPath`](https://hackage.haskell.org/package/filepath-1.4.100.3/docs/System-OsPath.html#t:OsPath)). If they are not known to be file URIs, then [`Text.URI.URI`](https://hackage.haskell.org/package/modern-uri-0.3.6.0/docs/Text-URI.html#t:URI) should be returned.

### To format the source code
```bash
# This needs at least ormolu 0.5.0.0 to avoid breaking dot-record syntax
ormolu --mode inplace $(find . -name '*.hs')
```