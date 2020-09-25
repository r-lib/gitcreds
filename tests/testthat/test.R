test_that("set_env() round trip works with just 1 env var", {
  expect_equal(Sys.getenv("FOO", "unset"), "unset")

  oenv <- set_env(c(FOO = "abc"))
  expect_equal(Sys.getenv("FOO"), "abc")

  set_env(oenv)
  expect_equal(Sys.getenv("FOO", "unset"), "unset")
})
