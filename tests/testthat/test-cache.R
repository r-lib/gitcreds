
gc_test_that("gitcreds_cache_envvvar", {
  cases <- list(
    c("https://github.com", "GITHUB_PAT_GITHUB_COM"),
    c("https://api.github.com/path/to/endpoint", "GITHUB_PAT_GITHUB_COM"),
    c("https://jane@github.com", "GITHUB_PAT_JANE_AT_GITHUB_COM"),
    c("https://another.site.github.com", "GITHUB_PAT_ANOTHER_SITE_GITHUB_COM"),
    c("http://foo.bar", "GITHUB_PAT_FOO_BAR")
  )

  for (case in cases) {
    expect_equal(gitcreds_cache_envvar(case[[1]]), case[[2]])
  }

  # vectorized
  expect_equal(
    gitcreds_cache_envvar(vapply(cases, "[[", character(1), 1)),
    vapply(cases, "[[", character(1), 2)
  )

  # error
  expect_error(gitcreds_cache_envvar("foo.bar"), "Invalid URL")
})

gc_test_that("gitcreds_get_cache", {
  # single password
  withr::local_envvar(c(GITHUB_PAT_GITHUB_COM = "token"))
  cred <- gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$password, "token")

  # username + password
  withr::local_envvar(c(GITHUB_PAT_GITHUB_COM = "user:pass"))
  cred <- gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$username, "user")
  expect_equal(cred$password, "pass")

  # fall back to GITHUB_PAT
  withr::local_envvar(c(
    GITHUB_PAT_GITHUB_COM = NA_character_,
    GITHUB_PAT = "mytoken"
  ))
  cred <- gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$password, "mytoken")

  # fall back to GITHUB_TOKEN
  withr::local_envvar(c(
    GITHUB_PAT_GITHUB_COM = NA_character_,
    GITHUB_PAT = NA_character_,
    GITHUB_TOKEN = "mytoken3"
  ))
  cred <- gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$password, "mytoken3")

  # Not set
  withr::local_envvar(c(
    GITHUB_PAT_GITHUB_COM = NA_character_,
    GITHUB_PAT = NA_character_,
    GITHUB_TOKEN = NA_character_
  ))
  expect_null(gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_COM"))

  # Warn for invalid
  withr::local_envvar(c(GITHUB_PAT_GITHUB_COM = "what:is:this"))
  expect_warning(
    expect_null(gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_COM")),
    "Invalid gitcreds credentials in env var"
  )

  # fails if it has to
  withr::local_envvar(c(GITHUB_PAT_GITHUB_COM = "FAIL"))
  expect_error(
    gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_COM"),
    class = "gitcreds_no_credentials"
  )

  withr::local_envvar(c(GITHUB_PAT_GITHUB_COM = "FAIL:gitcreds_no_helper"))
  expect_error(
    gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_COM"),
    class = "gitcreds_no_helper"
  )
})

gc_test_that("gitcreds_set_cache", {
  # : is escaped
  gitcreds$gitcreds_set_cache("GITHUB_PAT_GITHUB_COM", list("x:y" = "a:b"))
  cred <- gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$username, "x:y")
  expect_equal(cred$password, "a:b")
})
