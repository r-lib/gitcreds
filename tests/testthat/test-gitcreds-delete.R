
gc_test_that("gitcreds_delete", {
  # fails if not interactive
  mockery::stub(gitcreds_delete, "is_interactive", FALSE)
  expect_error(
    gitcreds_delete(),
    class = "gitcreds_not_interactive_error"
  )

  mockery::stub(gitcreds_delete, "is_interactive", TRUE)

  # FALSE if nothing was deleted
  expect_false(gitcreds_delete())

  # can be aborted
  cred <- list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = "secret"
  )
  gitcreds_approve(cred)
  gitcreds_get()

  mockery::stub(gitcreds_delete, "ack", FALSE)
  expect_error(
    gitcreds_delete(),
    class = "gitcreds_abort_delete_error"
  )

  # really deletes
  mockery::stub(gitcreds_delete, "ack", TRUE)
  gitcreds_delete()
  expect_error(
    gitcreds_get(use_cache = FALSE),
    class = "gitcreds_no_credentials"
  )

  # deletes cache as well
  expect_null(gitcreds_get_cache(gitcreds_cache_envvar("https://github.com")))
})
