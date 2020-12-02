
test_that("set_env() round trip works with just 1 env var", {
  withr::local_envvar(c(FOO = NA_character_))

  oenv <- gitcreds$set_env(c(FOO = "abc"))
  expect_equal(Sys.getenv("FOO"), "abc")

  gitcreds$set_env(oenv)
  expect_equal(Sys.getenv("FOO", "unset"), "unset")
})

test_that("set_env() NA unsets", {
  withr::local_envvar(c(FOO = NA_character_))

  Sys.setenv(FOO = "bar")
  gitcreds$set_env(c(FOO = NA_character_))

  expect_equal(Sys.getenv("FOO", ""), "")
})

test_that("env var set to empty string is same as unset", {
  skip_on_os("windows")

  withr::local_envvar(c(GITHUB_PAT_GITHUB_ACME_COM = NA_character_))

  oenv <- gitcreds$set_env(c(GITHUB_PAT_GITHUB_ACME_COM = ""))
  on.exit(gitcreds$set_env(oenv), add = TRUE)

  x <- gitcreds$gitcreds_get_cache("GITHUB_PAT_GITHUB_ACME_COM")
  expect_null(x)
})

test_that("ack", {
  cred <- gitcreds$new_gitcreds(
    protocol = "https",
    host = "github.com",
    username = "user",
    password = "secret",
    arbitrary = "xxxxxxxxx"
  )

  mockery::stub(gitcreds$ack, "utils::menu", 1)
  expect_false(gitcreds$ack("https://github.com", cred))

  mockery::stub(gitcreds$ack, "utils::menu", 2)
  expect_true(gitcreds$ack("https://github.com", cred))

  num <- 0L
  mockery::stub(gitcreds$ack, "utils::menu", function(...) {
    num <<- num + 1L
    if (num == 1L) 3 else 1
  })

  mm <- NULL
  withCallingHandlers(
    ret <- gitcreds$ack("https://github.com", cred),
    message = function(m) mm <<- m
  )
  expect_equal(num, 2)
  expect_false(ret)
  expect_true(any(grepl("Current password", mm$message)))
})

gc_test_that("check_for_git", {
  expect_silent(gitcreds$check_for_git())

  mockery::stub(gitcreds$check_for_git, "system2", function(command, ...) {
    base::system2(basename(tempfile()), ...)
  })
  expect_error(
    gitcreds$check_for_git(),
    class = "gitcreds_nogit_error"
  )
})

gc_test_that("get_os", os = "macos", helpers = "osxkeychain", {
  mockery::stub(gitcreds$get_os, "Sys.info", c(sysname = "foobar"))
  expect_equal(gitcreds$get_os(), "unknown")
})

test_that("get_url_username", {
  expect_equal(gitcreds$get_url_username("https://user1@host.com"), "user1")
  expect_null(gitcreds$get_url_username("https://host.com"))
})

test_that("throw", {
  err <- gitcreds$new_error("myerror", message = "boo")
  expect_error(gitcreds$throw(err), class = "myerror")

  wrn <- gitcreds$new_warning("mywarning", message = "ouch")
  expect_warning(gitcreds$throw(wrn), class = "mywarning")

  msg <- structure(
    list(message = "hey"),
    class = c("mymessage", "message", "condition")
  )
  expect_message(gitcreds$throw(msg), class = "mymessage")

  cnd <- structure(
    list(message = "this"),
    class = c("mycondition", "condition")
  )
  expect_condition(gitcreds$throw(cnd), class = "mycondition")
})

test_that("null_file", {
  expect_silent(writeLines(letters, gitcreds$null_file()))
  expect_equal(readLines(gitcreds$null_file()), character())
})

test_that("msg", {
  # testthat is catching all messages, so we need to change the
  # class to test this
  mockery::stub(gitcreds$msg, "simpleMessage", function(message, call = NULL) {
    structure(
      list(message = message, call = call),
      class = c("mymessage", "condition")
    )
  })

  # not interactive, we have a sink, goes to stderr
  out <- capture.output(gitcreds$msg("foo", "bar"), type = "message")
  expect_equal(out, "foobar")

  # no output if we catch it
  mm <- NULL
  out <- capture.output(
    tryCatch(gitcreds$msg("foo", "bar"), mymessage = function(m) mm <<- m)
  )
  expect_equal(out, character())
  expect_true(mm$message %in% c("foobar\n", "foobar\r\n"))
})

test_that("default_output", {
  mockery::stub(gitcreds$default_output, "is_interactive", TRUE)
  expect_equal(gitcreds$default_output(), stdout())
  expect_equal(
    withr::with_output_sink(tempfile(), gitcreds$default_output()),
    stderr()
  )
  expect_equal(
    withr::with_message_sink(tempfile(), gitcreds$default_output()),
    stderr()
  )

  mockery::stub(gitcreds$default_output, "is_interactive", FALSE)
  expect_equal(gitcreds$default_output(), stderr())
  expect_equal(
    withr::with_output_sink(tempfile(), gitcreds$default_output()),
    stderr()
  )
  expect_equal(
    withr::with_message_sink(tempfile(), gitcreds$default_output()),
    stderr()
  )
})

test_that("is_interactive", {
  withr::with_options(
    list(rlib_interactive = TRUE, rlang_interactive = NULL),
    expect_true(gitcreds$is_interactive())
  )
  withr::with_options(
    list(rlib_interactive = FALSE, rlang_interactive = NULL),
    expect_false(gitcreds$is_interactive())
  )
  withr::with_options(
    list(rlib_interactive = NULL, rlang_interactive = TRUE),
    expect_true(gitcreds$is_interactive())
  )
  withr::with_options(
    list(rlib_interactive = NULL, rlang_interactive = FALSE),
    expect_false(gitcreds$is_interactive())
  )
  withr::with_options(
    list(rlib_interactive = TRUE, rlang_interactive = FALSE),
    expect_true(gitcreds$is_interactive())
  )
  withr::with_options(
    list(rlib_interactive = FALSE, rlang_interactive = TRUE),
    expect_false(gitcreds$is_interactive())
  )
  withr::with_options(
    list(rlib_interactive = NULL, rlang_interactive = NULL,
         knitr.in.progress = TRUE),
    expect_false(gitcreds$is_interactive())
  )
  withr::with_options(
    list(rlib_interactive = NULL, rlang_interactive = NULL,
         knitr.in.progress = NULL),
    withr::with_envvar(
      c(TESTTHAT = "true"),
      expect_false(gitcreds$is_interactive())
    )
  )

  mockery::stub(gitcreds$is_interactive, "base::interactive", "yes")
  withr::with_options(
    list(rlib_interactive = NULL, rlang_interactive = NULL,
         knitr.in.progress = NULL),
    withr::with_envvar(
      c(TESTTHAT = NA_character_),
      expect_equal(gitcreds$is_interactive(), "yes")
    )
  )
})

gc_test_that("git_run", {
  err <- tryCatch(gitcreds$git_run("qwertyzxcvbn"), error = function(x) x)
  expect_s3_class(err, "git_error")
  expect_s3_class(err, "gitcreds_error")
  expect_match(conditionMessage(err), "System git failed:.*qwertyzxcvbn")
})
