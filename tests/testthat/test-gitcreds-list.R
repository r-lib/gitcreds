
gc_test_that("gitcreds_list", {
  # no helper
  local_helpers(character())
  expect_error(
    gitcreds_list(),
    class = "gitcreds_no_helper"
  )

  # unknown helper
  local_helpers(basename(tempfile()))
  expect_error(
    gitcreds_list(),
    class = "gitcreds_unknown_helper"
  )

  # the right helper function is chosen
  mockery::stub(gitcreds_list, "gitcreds_list_osxkeychain", "osx")
  mockery::stub(gitcreds_list, "gitcreds_list_manager", "gcm")
  mockery::stub(gitcreds_list, "gitcreds_list_manager_core", "gcmcore")
  expect_equal(gitcreds_list(credential_helper = "osxkeychain"), "osx")
  expect_equal(gitcreds_list(credential_helper = "manager"), "gcm")
  expect_equal(gitcreds_list(credential_helper = "manager-core"), "gcmcore")

  # warn for multiple helpers
  local_helpers(c("foo", "bar"))
  mockery::stub(gitcreds_list, "switch", NULL)
  expect_warning(
    gitcreds_list(),
    class = "gitcreds_multiple_helpers"
  )
})

gc_test_that("gitcreds_list_osxkeychain", os = "macos",
             helpers = "osxkeychain", {
  # needs oskeyring
  fun <- function() {
    mockery::stub(gitcreds_list_osxkeychain, "requireNamespace", FALSE)
    gitcreds_list_osxkeychain()
  }
  expect_error(
    fun(),
    "needs the `oskeyring` package"
  )

  # no credentials just yet
  expect_equal(gitcreds_list(), list())

  cred <- list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = "secret"
  )
  gitcreds_approve(cred)

  lst <- gitcreds_list()
  expect_equal(length(lst), 1L)
  expect_equal(lst[[1]]$attributes$server, "github.com")

  lst2 <- gitcreds_list(url = NULL)
  expect_true(length(lst2) >= 1)
  expect_true("github.com" %in% vapply(lst2, function(it) it$attributes$server, ""))

  cred2 <- list(
    url = "https://github2.com",
    username = "user",
    password = "secret2"
  )
  gitcreds_approve(cred2)

  lst3 <- gitcreds_list()
  expect_equal(length(lst3), 1L)
  expect_equal(lst3[[1]]$attributes$server, "github.com")

  lst4 <- gitcreds_list("https://github2.com")
  expect_equal(length(lst4), 1L)
  expect_equal(lst4[[1]]$attributes$server, "github2.com")

  lst5 <- gitcreds_list(url = NULL)
  expect_true(length(lst5) >= 2)
  expect_true("github.com" %in% vapply(lst5, function(it) it$attributes$server, ""))
  expect_true("github2.com" %in% vapply(lst5, function(it) it$attributes$server, ""))
})

gc_test_that("gitcreds_list_manager_core", os = c("windows", "macos"), {
  mockery::stub(gitcreds_list_manager_core, "gitcreds_list_manager_core_macos", "macos")
  mockery::stub(gitcreds_list_manager_core, "gitcreds_list_manager_core_win", "win")

  mockery::stub(gitcreds_list_manager_core, "get_os", "windows")
  expect_equal(gitcreds_list_manager_core(), "win")

  mockery::stub(gitcreds_list_manager_core, "get_os", "macos")
  expect_equal(gitcreds_list_manager_core(), "macos")

  mockery::stub(gitcreds_list_manager_core, "get_os", "linux")
  expect_error(
    gitcreds_list_manager_core(),
    "Unsupported OS"
  )
})

gc_test_that("gitcreds_list_manager_core_macos", os = "macos",
             helper = "manager-core", {
  # needs oskeyring
  fun <- function() {
    mockery::stub(gitcreds_list_manager_core_macos, "requireNamespace", FALSE)
    gitcreds_list_manager_core_macos()
  }
  expect_error(
    fun(),
    "needs the `oskeyring` package"
  )

  # no credentials just yet
  expect_equal(gitcreds_list(), list())

  cred <- list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = "secret"
  )
  gitcreds_approve(cred)

  lst <- gitcreds_list()
  expect_equal(length(lst), 1L)
  expect_equal(lst[[1]]$attributes$service, "git:https://github.com")

  lst2 <- gitcreds_list(url = NULL)
  expect_true(length(lst2) == 1)
  expect_equal(lst2[[1]]$attributes$service, "git:https://github.com")

  cred2 <- list(
    url = "https://github2.com",
    username = "user",
    password = "secret2"
  )
  gitcreds_approve(cred2)

  lst3 <- gitcreds_list()
  expect_equal(length(lst3), 1L)
  expect_equal(lst3[[1]]$attributes$service, "git:https://github.com")

  lst4 <- gitcreds_list("https://github2.com")
  expect_equal(length(lst4), 1L)
  expect_equal(lst4[[1]]$attributes$service, "git:https://github2.com")

  lst5 <- gitcreds_list(url = NULL)
  expect_true(length(lst5) >= 2)
  expect_true(
    "git:https://github.com" %in%
    vapply(lst5, function(it) it$attributes$service, "")
  )
  expect_true(
    "git:https://github2.com" %in%
    vapply(lst5, function(it) it$attributes$service, "")
  )
})

gc_test_that("gitcreds_list_manager_core_win", os = "windows",
             helper = "manager-core", {

  if (packageVersion("oskeyring") < "0.1.0") skip("Needs newer oskeyring")

  # needs oskeyring
  fun <- function() {
    mockery::stub(gitcreds_list_manager_core_macos, "requireNamespace", FALSE)
    gitcreds_list_manager_core_macos()
  }
  expect_error(
    fun(),
    "needs the `oskeyring` package"
  )

  # no credentials just yet
  expect_equal(gitcreds_list(), list())

  cred <- list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = "secret"
  )
  gitcreds_approve(cred)

  lst <- gitcreds_list()
  expect_equal(length(lst), 1L)
  expect_equal(lst[[1]]$target_name, "git:https://github.com")

  lst2 <- gitcreds_list(url = NULL)
  expect_true(length(lst2) == 1)
  expect_equal(lst2[[1]]$target_name, "git:https://github.com")

  cred2 <- list(
    url = "https://github2.com",
    username = "user",
    password = "secret2"
  )
  gitcreds_approve(cred2)

  lst3 <- gitcreds_list()
  expect_equal(length(lst3), 1L)
  expect_equal(lst3[[1]]$target_name, "git:https://github.com")

  lst4 <- gitcreds_list("https://github2.com")
  expect_equal(length(lst4), 1L)
  expect_equal(lst4[[1]]$target_name, "git:https://github2.com")

  lst5 <- gitcreds_list(url = NULL)
  expect_true(length(lst5) >= 2)
  expect_true(
    "git:https://github.com" %in%
      vapply(lst5, function(it) it$target_name, "")
  )
  expect_true(
    "git:https://github2.com" %in%
      vapply(lst5, function(it) it$target_name, "")
  )
})

gc_test_that("gitcreds_list_manager", os = "windows",
             helper = "manager", {

  if (packageVersion("oskeyring") < "0.1.0") skip("Needs newer oskeyring")

  # needs oskeyring
  fun <- function() {
    mockery::stub(gitcreds_list_manager_core_macos, "requireNamespace", FALSE)
    gitcreds_list_manager_core_macos()
  }
  expect_error(
    fun(),
    "needs the `oskeyring` package"
  )

  # no credentials just yet
  expect_equal(gitcreds_list(), list())

  cred <- list(
    url = "https://github.com",
    username = "PersonalAccessToken",
    password = "secret"
  )
  gitcreds_approve(cred)

  lst <- gitcreds_list()
  expect_equal(length(lst), 1L)
  expect_equal(lst[[1]]$target_name, "git:https://github.com")

  lst2 <- gitcreds_list(url = NULL)
  expect_true(length(lst2) == 1)
  expect_equal(lst2[[1]]$target_name, "git:https://github.com")

  cred2 <- list(
    url = "https://github2.com",
    username = "user",
    password = "secret2"
  )
  gitcreds_approve(cred2)

  lst3 <- gitcreds_list()
  expect_equal(length(lst3), 1L)
  expect_equal(lst3[[1]]$target_name, "git:https://github.com")

  lst4 <- gitcreds_list("https://github2.com")
  expect_equal(length(lst4), 1L)
  expect_equal(lst4[[1]]$target_name, "git:https://user@github2.com")

  lst5 <- gitcreds_list(url = NULL)
  expect_true(length(lst5) >= 2)
  expect_true(
    "git:https://github.com" %in%
      vapply(lst5, function(it) it$target_name, "")
  )
  expect_true(
    "git:https://user@github2.com" %in%
      vapply(lst5, function(it) it$target_name, "")
  )

})
