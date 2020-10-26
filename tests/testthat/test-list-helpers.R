
gc_test_that("gitcreds_list_helpers", os = c("windows", "macos"), {
  # no helpers at all
  local_helpers(character())
  expect_equal(gitcreds_list_helpers(), character())

  # custom helpers
  local_helpers(c("foo", "bar"))
  expect_equal(gitcreds_list_helpers(), c("foo", "bar"))

  # resetting helpers
  local_helpers(c("reset", "\"\"", "foobar"))
  expect_equal(gitcreds_list_helpers(), c("foobar"))
})
