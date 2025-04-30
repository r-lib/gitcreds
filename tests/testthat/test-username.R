gc_test_that("gitcreds_username_for_url", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  gitcreds$git_run(c("init", "."))
  gitcreds$git_run(c("config", "credential.username", "global"))
  gitcreds$git_run(c(
    "config",
    "credential.https://example.com.username",
    "spec"
  ))
  expect_equal(
    gitcreds$gitcreds_username_for_url("https://example.com"),
    "spec"
  )
  expect_equal(gitcreds$gitcreds_username_for_url("https://foo.com"), "global")
  expect_equal(gitcreds$gitcreds_username(), "global")
  expect_equal(gitcreds$gitcreds_username("https://foo.com"), "global")
  expect_equal(gitcreds$gitcreds_username("https://example.com"), "spec")
})

gc_test_that("gitcreds$gitcreds_username_generic", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  gitcreds$git_run(c("init", "."))
  gitcreds$git_run(c("config", "credential.username", "global"))
  gitcreds$git_run(c(
    "config",
    "credential.https://example.com.username",
    "spec"
  ))
  expect_equal(gitcreds$gitcreds_username_generic(), "global")
})

gc_test_that("errors", {
  mock <- function(args, ...) {
    args[1] <- basename(tempfile())
    gitcreds:::gitcreds$git_run(args, ...)
  }
  mockery::stub(gitcreds$gitcreds_username_for_url, "git_run", mock)
  mockery::stub(gitcreds$gitcreds_username_generic, "git_run", mock)
  expect_null(gitcreds$gitcreds_username_for_url("https://github.com"))
  expect_null(gitcreds$gitcreds_username_generic())

  mock2 <- function(...) {
    gitcreds:::gitcreds$git_run(c("config", "--unset", "xxxxxx.yyyyy"))
  }
  mockery::stub(gitcreds$gitcreds_username_for_url, "git_run", mock2)
  mockery::stub(gitcreds$gitcreds_username_generic, "git_run", mock2)
  expect_snapshot(error = TRUE, {
    gitcreds$gitcreds_username_for_url("https://github.com")
    gitcreds$gitcreds_username_generic()
  })
})
