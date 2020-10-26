
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
  gitcreds_delete_cache(gitcreds_cache_envvar("https://github.com"))
  expect_null(gitcreds_get_cache(gitcreds_cache_envvar("https://github.com")))

  gitcreds_approve(cred)
  cred4 <- gitcreds_get(set_cache = FALSE)
  chk(cred4)
  expect_null(gitcreds_get_cache(gitcreds_cache_envvar("https://github.com")))
})
