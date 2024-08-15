
gc_test_that("gitcreds_set_new", os = c("windows", "macos"), {
  local_mocked_bindings(readline = function(...) "new-secret")
  local_mocked_bindings(readline = function(...) NULL)
  gitcreds_set_new("https://github.com")

  cred <- gitcreds_get(set_cache = FALSE)
  expect_equal(cred$host, "github.com")
  expect_equal(cred$password, "new-secret")
})

gc_test_that("gitcreds_set_replace", os = c("windows", "macos"), {
  cred <- list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = "secret"
  )
  gitcreds_approve(cred)

  local_mocked_bindings(ack = function(...) FALSE)
  local_mocked_bindings(readline = function(...) "new-secret-2")
  local_mocked_bindings(cat = function(...) NULL)
  expect_error(
    gitcreds_set_replace("https://github.com", gitcreds_get()),
    class = "gitcreds_abort_replace_error"
  )

  local_mocked_bindings(ack = function(...) TRUE)
  gitcreds_set_replace("https://github.com", gitcreds_get())

  cred <- gitcreds_get(use_cache = FALSE)
  expect_equal(cred$host, "github.com")
  expect_equal(cred$password, "new-secret-2")
})

gc_test_that("gitcreds_set", {
  # fails if not interactive
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_error(
    gitcreds_set(),
    class = "gitcreds_not_interactive_error"
  )

  called <- NULL
  local_mocked_bindings(is_interactive = function() TRUE)
  local_mocked_bindings(gitcreds_set_replace = function() {
    called <<- "gitcreds_set_replace"
  })
  local_mocked_bindings(gitcreds_set_new = function(...) {
    called <<- "gitcreds_set_new"
  })

  # calls set_new if none
  called <- NULL
  gitcreds_set()
  expect_equal(called, "gitcreds_set_new")

  # calls set_replace if there is one
  cred <- list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = "secret"
  )
  gitcreds_approve(cred)

  called <- NULL
  gitcreds_set()
  expect_equal(called, "gitcreds_set_replace")

  # does not use the cache
  gitcreds_get()
  gitcreds_reject(cred)
  called <- NULL
  gitcreds_set()
  expect_equal(called, "gitcreds_set_new")

  # deletes the cache
  expect_null(gitcreds$gitcreds_get_cache(
    gitcreds_cache_envvar("https://github.com")
  ))
})

gc_test_that("multiple matching credentials", {
  cred <- list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = "secret"
  )
  gitcreds_approve(cred)
  cred2 <- list(
    url = "https://github.com",
    username = "PersonalAccessToken2",
    password = "secret2"
  )
  gitcreds_approve(cred2)

  local_mocked_bindings(ack = function(...) FALSE)
  local_mocked_bindings(readline = function(...) "new-secret-2")
  local_mocked_bindings(cat = function(...) NULL)
  expect_error(
    gitcreds_set_replace("https://github.com", gitcreds_get()),
    class = "gitcreds_abort_replace_error"
  )

  local_mocked_bindings(ack = function(...) TRUE)
  gitcreds_set_replace("https://github.com", gitcreds_get())

  cred <- gitcreds_get(use_cache = FALSE)
  expect_equal(cred$host, "github.com")
  expect_equal(cred$password, "new-secret-2")
})
