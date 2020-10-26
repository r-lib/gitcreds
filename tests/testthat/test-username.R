
gc_test_that("gitcreds_username_for_url", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  git_run(c("init", "."))
  git_run(c("config", "credential.username", "global"))
  git_run(c("config", "credential.https://example.com.username", "spec"))
  expect_equal(gitcreds_username_for_url("https://example.com"), "spec")
  expect_equal(gitcreds_username_for_url("https://foo.com"), "global")
  expect_equal(gitcreds_username(), "global")
  expect_equal(gitcreds_username("https://foo.com"), "global")
  expect_equal(gitcreds_username("https://example.com"), "spec")
})

gc_test_that("gitcreds_username_generic", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  git_run(c("init", "."))
  git_run(c("config", "credential.username", "global"))
  git_run(c("config", "credential.https://example.com.username", "spec"))
  expect_equal(gitcreds_username_generic(), "global")
})
