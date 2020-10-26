
test_that("set_env() round trip works with just 1 env var", {
  withr::local_envvar(c(FOO = NA_character_))

  oenv <- set_env(c(FOO = "abc"))
  expect_equal(Sys.getenv("FOO"), "abc")

  set_env(oenv)
  expect_equal(Sys.getenv("FOO", "unset"), "unset")
})

test_that("set_env() NA unsets", {
  withr::local_envvar(c(FOO = NA_character_))

  Sys.setenv(FOO = "bar")
  set_env(c(FOO = NA_character_))

  expect_equal(Sys.getenv("FOO", ""), "")
})

test_that("env var set to empty string is same as unset", {
  skip_on_os("windows")

  withr::local_envvar(c(GITHUB_PAT_GITHUB_ACME_COM = NA_character_))

  oenv <- set_env(c(GITHUB_PAT_GITHUB_ACME_COM = ""))
  on.exit(set_env(oenv), add = TRUE)

  x <- gitcreds_get_cache("GITHUB_PAT_GITHUB_ACME_COM")
  expect_null(x)
})

test_that("ack", {
  cred <- new_gitcreds(
    protocol = "https",
    host = "github.com",
    username = "user",
    password = "secret",
    arbitrary = "xxxxxxxxx"
  )

  mockery::stub(ack, "utils::menu", 1)
  expect_false(ack("https://github.com", cred))

  mockery::stub(ack, "utils::menu", 2)
  expect_true(ack("https://github.com", cred))

  num <- 0L
  mockery::stub(ack, "utils::menu", function(...) {
    num <<- num + 1L
    if (num == 1L) 3 else 1
  })

  mm <- NULL
  withCallingHandlers(
    ret <- ack("https://github.com", cred),
    message = function(m) mm <<- m
  )
  expect_equal(num, 2)
  expect_false(ret)
  expect_true(any(grepl("Current password", mm$message)))
})

gc_test_that("check_for_git", {
  expect_silent(check_for_git())

  mockery::stub(check_for_git, "system2", function(command, ...) {
    base::system2(basename(tempfile()), ...)
  })
  expect_error(
    check_for_git(),
    class = "gitcreds_nogit_error"
  )
})

gc_test_that("get_os", os = "macos", helpers = "osxkeychain", {
  mockery::stub(get_os, "Sys.info", c(sysname = "foobar"))
  expect_equal(get_os(), "unknown")
})

test_that("get_url_username", {
  expect_equal(get_url_username("https://user1@host.com"), "user1")
  expect_null(get_url_username("https://host.com"))
})
