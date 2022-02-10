# hledger-locker

This CLI tool partially addresses the issue of account lifetime (see, for example, https://github.com/simonmichael/hledger/issues/1389).
We use a so-called `locker` file that contains directives of the form
```ledger
# this is a comment
; this is also a comment
; there are no inline comments
; grammar is [close|open] SmartDate AccountName, e.g.
close 2022-01-01 assets:cash
open today equity
; empty lines are ignored
```

The tool would check the presence of postings to the specified accounts and report errors if
- there are postings on dates strictly after the `close` date
- there are postings on date strictly before the `open` date

### Note on dates
We accept all dates that `hledger add` would accept, including dates like `yesterday`.
They are then evaluated with respect to current local date, so the assertion outcome can change from run to run.

## Usage
```
Usage: hlocker.EXE [-v|--version] [-f|--journal-file JOURNAL_FILE]
                   [--locker-file LOCKER_FILE]
  Close/Open account assertions for hledger journal files

Available options:
  -h,--help                Show this help text
  -v,--version             Print version and exit
  -f,--journal-file JOURNAL_FILE
                           path to JOURNAL_FILE
  --locker-file LOCKER_FILE
                           path to LOCKER_FILE
```

We look for journal file, if `-f` is not provided in the `$LEDGER_FILE` environement variable and in `$HOME/.hledger.journal`.

We look for locker file, if `--locker-file` is not specified, in `$HLEDGER_LOCKER_FILE` and in `$HOME/.hledger.locker`.

There is also a `--debug` flag for a more verbose output.

## Installation

Right now, only `stack build` and `stack install`. Other installation methods are not yet tested/written.
Artefacts in releases will be available soon-ish.

## To do

### Functionality

- Read journal entries from `stdin`
- Specify `locker` via CLI options

### Code and documentation

- building and installing with `nix`
- building and installing with `cabal`
- better test coverage
- CI/CD

