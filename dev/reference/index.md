# Package index

## Easy API

- [`gitcreds_get()`](gitcreds_get.md)
  [`gitcreds_set()`](gitcreds_get.md)
  [`gitcreds_delete()`](gitcreds_get.md)
  [`gitcreds_list_helpers()`](gitcreds_get.md) : Query and set git
  credentials

## Caching credentials

- [`gitcreds_cache_envvar()`](gitcreds_cache_envvar.md) : Environment
  variable to cache the password for a URL

## List credentials

- [`gitcreds_list()`](gitcreds_list.md) : List all credentials stored by
  a git credential helper

## Low level API

- [`gitcreds_fill()`](gitcreds-api.md)
  [`gitcreds_approve()`](gitcreds-api.md)
  [`gitcreds_reject()`](gitcreds-api.md) : Access the low level
  credential API

- [`gitcreds_parse_output()`](gitcreds_parse_output.md) :

  Parse standard output from `git credential fill`
