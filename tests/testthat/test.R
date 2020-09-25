test_that("set_env() round trip works with just 1 env var", {
  expect_equal(Sys.getenv("FOO", "unset"), "unset")

  oenv <- set_env(c(FOO = "abc"))
  expect_equal(Sys.getenv("FOO"), "abc")

  set_env(oenv)
  expect_equal(Sys.getenv("FOO", "unset"), "unset")
})

test_that("env var set to empty string is same as unset", {
  skip_on_os("windows")

  oenv <- set_env(c(GITHUB_PAT_GITHUB_ACME_COM = ""))
  on.exit(set_env(oenv), add = TRUE)

  x <- gitcreds_get_cache("GITHUB_PAT_GITHUB_ACME_COM")
  expect_null(x$password)
})
