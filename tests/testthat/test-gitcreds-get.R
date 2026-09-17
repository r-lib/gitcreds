gc_test_that("gitcreds_get", os = c("windows", "macos"), {
  cred <- list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = "secret"
  )
  gitcreds_approve(cred)

  chk <- function(cr) {
    expect_s3_class(cr, "gitcreds")
    expect_equal(cr$protocol, "https")
    expect_equal(cr$host, "github.com")
    expect_equal(cr$username, "PersonalAccessToken")
    expect_equal(cr$password, "secret")
  }

  cred2 <- gitcreds_get()
  chk(cred2)

  # cache is set
  gitcreds_reject(cred)
  cred3 <- gitcreds_get()
  chk(cred3)

  # use_cache is FALSE
  expect_error(
    gitcreds_get(use_cache = FALSE),
    class = "gitcreds_no_credentials"
  )

  # set_cache is FALSE
  gitcreds$gitcreds_delete_cache(gitcreds_cache_envvar("https://github.com"))
  expect_null(gitcreds$gitcreds_get_cache(
    gitcreds_cache_envvar("https://github.com")
  ))

  gitcreds_approve(cred)
  cred4 <- gitcreds_get(set_cache = FALSE)
  chk(cred4)
  expect_null(gitcreds$gitcreds_get_cache(
    gitcreds_cache_envvar("https://github.com")
  ))
})

test_that("no_credentials_message() names the url and the env var", {
  msg <- gitcreds$no_credentials_message("https://codeberg.org")
  expect_match(msg, "https://codeberg.org", fixed = TRUE)
  expect_match(msg, "GITHUB_PAT_CODEBERG_ORG", fixed = TRUE)
  expect_match(msg, "gitcreds_set(", fixed = TRUE)
})

test_that("no_credentials_message() drops the hint for an unparseable url", {
  msg <- gitcreds$no_credentials_message("foo.bar")
  expect_equal(msg, "Could not find any credentials for 'foo.bar'")
})

test_that("the no-credentials error carries the url and the env var", {
  err <- tryCatch(
    gitcreds_parse_output("protocol=dummy", "https://codeberg.org"),
    gitcreds_no_credentials = function(e) e
  )
  expect_s3_class(err, "gitcreds_no_credentials")
  expect_equal(err$url, "https://codeberg.org")
  expect_match(conditionMessage(err), "GITHUB_PAT_CODEBERG_ORG", fixed = TRUE)
})
