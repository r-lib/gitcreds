
gc_test_that("gitcreds_delete", {
  # fails if not interactive
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_error(
    gitcreds_delete(),
    class = "gitcreds_not_interactive_error"
  )

  local_mocked_bindings(is_interactive = function() TRUE)

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

  local_mocked_bindings(ack = function(...) FALSE)
  expect_error(
    gitcreds_delete(),
    class = "gitcreds_abort_delete_error"
  )

  # really deletes
  local_mocked_bindings(ack = function(...) TRUE)
  gitcreds_delete()
  expect_error(
    gitcreds_get(use_cache = FALSE),
    class = "gitcreds_no_credentials"
  )

  # deletes cache as well
  expect_null(gitcreds$gitcreds_get_cache(
    gitcreds_cache_envvar("https://github.com")
  ))
})
