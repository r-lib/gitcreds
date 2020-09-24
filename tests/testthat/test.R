test_that("gitcreds handles env var set to empty string", {
  skip_on_os("windows")

  oenv <- set_env(c(GITHUB_PAT_GITHUB_ACME_COM = ""))
  on.exit(set_env(oenv), add = TRUE)

  x <- gitcreds::gitcreds_get("https://github.acme.com")
  expect_equal(x$password, "")
})
