
test_that("format.gitcreds", {
  cred <- new_gitcreds(
    protocol = "https",
    host = "github.com",
    username = "user",
    password = "secret",
    arbitrary = "xxxxxxxxx"
  )

  fmt <- format(cred)
  expect_true(any(grepl("arbitrary", fmt, fixed = TRUE)))
  expect_true(any(grepl("xxxxxx", fmt, fixed = TRUE)))
  expect_false(any(grepl("secret", fmt, fixed = TRUE)))
  expect_true(any(grepl("hidden", fmt, fixed = TRUE)))
})

test_that("print.gitcreds", {
  cred <- new_gitcreds(
    protocol = "https",
    host = "github.com",
    username = "user",
    password = "secret",
    arbitrary = "xxxxxxxxx"
  )

  out <- capture.output(ret <- print(cred))
  expect_equal(ret, cred)
  expect_true(any(grepl("arbitrary", out, fixed = TRUE)))
  expect_true(any(grepl("xxxxxx", out, fixed = TRUE)))
  expect_false(any(grepl("secret", out, fixed = TRUE)))
  expect_true(any(grepl("hidden", out, fixed = TRUE)))
})
