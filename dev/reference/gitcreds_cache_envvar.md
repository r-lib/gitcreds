# Environment variable to cache the password for a URL

[`gitcreds_get()`](gitcreds_get.md) caches credentials in environment
variables. `gitcreds_cache_envvar()` calculates the environment
variaable name that is used as the cache, for a URL.

## Usage

``` r
gitcreds_cache_envvar(url)
```

## Arguments

- url:

  Character vector of URLs, they may contain user names and paths as
  well. See details below.

## Value

Character vector of environment variables.

## See also

[`gitcreds_get()`](gitcreds_get.md).

## Examples

``` r
gitcreds_cache_envvar("https://github.com")
#> [1] "GITHUB_PAT_GITHUB_COM"
gitcreds_cache_envvar("https://api.github.com/path/to/endpoint")
#> [1] "GITHUB_PAT_GITHUB_COM"
gitcreds_cache_envvar("https://jane@github.com")
#> [1] "GITHUB_PAT_JANE_AT_GITHUB_COM"
gitcreds_cache_envvar("https://another.site.github.com")
#> [1] "GITHUB_PAT_ANOTHER_SITE_GITHUB_COM"
```
