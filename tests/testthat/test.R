test_that("gitcreds handles env var set to empty string", {
  skip_on_os("windows")

  orig <- Sys.getenv("GITHUB_PAT_GITHUB_ACME_COM")
  on.exit(Sys.setenv(GITHUB_PAT_GITHUB_ACME_COM = orig), add = TRUE)

  Sys.setenv(GITHUB_PAT_GITHUB_ACME_COM = "")
  x <- gitcreds::gitcreds_get("https://github.acme.com")
  expect_equal(x$password, "")
})
