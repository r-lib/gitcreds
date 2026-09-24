# Changelog

## gitcreds (development version)

- Cached credentials can now also be supplied in a `GITCREDS_PAT_*`
  environment variable, alongside the `GITHUB_PAT_*` name that gitcreds
  still writes and returns from
  [`gitcreds_cache_envvar()`](../reference/gitcreds_cache_envvar.md).
  Removing a credential clears both names
  ([\#56](https://github.com/r-lib/gitcreds/issues/56)).

- The `gitcreds_no_credentials` error now names the URL and the
  environment variable that would supply a credential for it, instead of
  only saying “Could not find any credentials”
  ([\#56](https://github.com/r-lib/gitcreds/issues/56)).

## gitcreds 0.1.2

CRAN release: 2022-09-08

- No user visible changes.

## gitcreds 0.1.1

CRAN release: 2020-12-04

- gitcreds now works with older git versions on Windows. It should work
  without any configuration for git 2.12.1 or later, and with minimal
  configuration for git 2.9.2 - git 2.12.0. See
  [`?gitcreds_get`](../reference/gitcreds_get.md) for more.

- git errors have now better error messages, they include the output
  from git as well.

- The `git-auth.R` file is now standalone, and can be embedded into
  other packages.

## gitcreds 0.1.0

CRAN release: 2020-11-06

First public release.
