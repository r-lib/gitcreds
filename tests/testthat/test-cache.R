gc_test_that("gitcreds_cache_envvvar", {
  cases <- list(
    c("https://github.com", "GITCREDS_PAT_GITHUB_COM"),
    c("https://api.github.com/path/to/endpoint", "GITCREDS_PAT_GITHUB_COM"),
    c("https://jane@github.com", "GITCREDS_PAT_JANE_AT_GITHUB_COM"),
    c(
      "https://another.site.github.com",
      "GITCREDS_PAT_ANOTHER_SITE_GITHUB_COM"
    ),
    c("http://foo.bar", "GITCREDS_PAT_FOO_BAR")
  )

  for (case in cases) {
    expect_equal(gitcreds_cache_envvar(case[[1]]), case[[2]])
  }

  # vectorized
  expect_equal(
    gitcreds_cache_envvar(vapply(cases, "[[", character(1), 1)),
    vapply(cases, "[[", character(1), 2)
  )

  # error
  expect_snapshot(error = TRUE, gitcreds_cache_envvar("foo.bar"))
})

gc_test_that("gitcreds_get_cache", {
  # single password
  withr::local_envvar(c(GITCREDS_PAT_GITHUB_COM = "token"))
  cred <- gitcreds$gitcreds_get_cache("GITCREDS_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$password, "token")

  # username + password
  withr::local_envvar(c(GITCREDS_PAT_GITHUB_COM = "user:pass"))
  cred <- gitcreds$gitcreds_get_cache("GITCREDS_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$username, "user")
  expect_equal(cred$password, "pass")

  # fall back to GITHUB_PAT
  withr::local_envvar(c(
    GITCREDS_PAT_GITHUB_COM = NA_character_,
    GITHUB_PAT = "mytoken"
  ))
  cred <- gitcreds$gitcreds_get_cache("GITCREDS_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$password, "mytoken")

  # fall back to GITHUB_TOKEN
  withr::local_envvar(c(
    GITCREDS_PAT_GITHUB_COM = NA_character_,
    GITHUB_PAT = NA_character_,
    GITHUB_TOKEN = "mytoken3"
  ))
  cred <- gitcreds$gitcreds_get_cache("GITCREDS_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$password, "mytoken3")

  # Not set
  withr::local_envvar(c(
    GITCREDS_PAT_GITHUB_COM = NA_character_,
    GITHUB_PAT = NA_character_,
    GITHUB_TOKEN = NA_character_
  ))
  expect_null(gitcreds$gitcreds_get_cache("GITCREDS_PAT_GITHUB_COM"))

  # Warn for invalid
  withr::local_envvar(c(GITCREDS_PAT_GITHUB_COM = "what:is:this"))
  expect_warning(
    expect_null(gitcreds$gitcreds_get_cache("GITCREDS_PAT_GITHUB_COM")),
    "Invalid gitcreds credentials in env var"
  )

  # fails if it has to
  withr::local_envvar(c(GITCREDS_PAT_GITHUB_COM = "FAIL"))
  expect_error(
    gitcreds$gitcreds_get_cache("GITCREDS_PAT_GITHUB_COM"),
    class = "gitcreds_no_credentials"
  )

  withr::local_envvar(c(GITCREDS_PAT_GITHUB_COM = "FAIL:gitcreds_no_helper"))
  expect_error(
    gitcreds$gitcreds_get_cache("GITCREDS_PAT_GITHUB_COM"),
    class = "gitcreds_no_helper"
  )
})

gc_test_that("gitcreds_set_cache", {
  # : is escaped
  gitcreds$gitcreds_set_cache("GITCREDS_PAT_GITHUB_COM", list("x:y" = "a:b"))
  cred <- gitcreds$gitcreds_get_cache("GITCREDS_PAT_GITHUB_COM")
  expect_s3_class(cred, "gitcreds")
  expect_equal(cred$username, "x:y")
  expect_equal(cred$password, "a:b")
})

test_that("gitcreds_cache_envvars() lists both names, new one first", {
  expect_equal(
    gitcreds$gitcreds_cache_envvars("https://gitlab.com"),
    c("GITCREDS_PAT_GITLAB_COM", "GITHUB_PAT_GITLAB_COM")
  )
})

test_that("gitcreds_cache_envvar() returns the new name", {
  expect_equal(
    gitcreds_cache_envvar("https://gitlab.com"),
    "GITCREDS_PAT_GITLAB_COM"
  )
})

test_that("either name supplies a credential, new one wins", {
  evs <- gitcreds$gitcreds_cache_envvars("https://gitlab.com")
  reset_legacy_warning()

  withr::local_envvar(c(
    GITCREDS_PAT_GITLAB_COM = NA_character_,
    GITHUB_PAT_GITLAB_COM = "legacy-token"
  ))
  expect_warning(
    expect_equal(gitcreds$gitcreds_get_cache(evs)$password, "legacy-token")
  )

  withr::local_envvar(c(GITCREDS_PAT_GITLAB_COM = "new-token"))
  expect_silent(
    expect_equal(gitcreds$gitcreds_get_cache(evs)$password, "new-token")
  )
})

test_that("a FAIL sentinel is honored under the canonical name only", {
  evs <- gitcreds$gitcreds_cache_envvars("https://gitlab.com")
  reset_legacy_warning()

  withr::local_envvar(c(
    GITCREDS_PAT_GITLAB_COM = "FAIL",
    GITHUB_PAT_GITLAB_COM = NA_character_
  ))
  expect_error(
    gitcreds$gitcreds_get_cache(evs),
    class = "gitcreds_no_credentials"
  )

  withr::local_envvar(c(
    GITCREDS_PAT_GITLAB_COM = NA_character_,
    GITHUB_PAT_GITLAB_COM = "FAIL"
  ))
  expect_null(gitcreds$gitcreds_get_cache(evs))
})

test_that("a legacy FAIL does not hide a credential that is present", {
  evs <- gitcreds$gitcreds_cache_envvars("https://gitlab.com")
  reset_legacy_warning()

  withr::local_envvar(c(
    GITCREDS_PAT_GITLAB_COM = NA_character_,
    GITHUB_PAT_GITLAB_COM = "FAIL"
  ))
  expect_null(gitcreds$gitcreds_get_cache(evs))

  withr::local_envvar(c(GITCREDS_PAT_GITLAB_COM = "new-token"))
  expect_equal(gitcreds$gitcreds_get_cache(evs)$password, "new-token")
})

test_that("the legacy-name warning fires once per session", {
  evs <- gitcreds$gitcreds_cache_envvars("https://gitlab.com")
  reset_legacy_warning()

  withr::local_envvar(c(
    GITCREDS_PAT_GITLAB_COM = NA_character_,
    GITHUB_PAT_GITLAB_COM = "legacy-token",
    GITCREDS_LEGACY_WARN = NA_character_
  ))

  expect_warning(
    gitcreds$gitcreds_get_cache(evs),
    "GITCREDS_PAT_GITLAB_COM"
  )
  expect_silent(gitcreds$gitcreds_get_cache(evs))
})

test_that("GITCREDS_LEGACY_WARN=false silences the legacy-name warning", {
  evs <- gitcreds$gitcreds_cache_envvars("https://gitlab.com")
  reset_legacy_warning()

  withr::local_envvar(c(
    GITCREDS_PAT_GITLAB_COM = NA_character_,
    GITHUB_PAT_GITLAB_COM = "legacy-token",
    GITCREDS_LEGACY_WARN = "false"
  ))
  expect_silent(gitcreds$gitcreds_get_cache(evs))
})

test_that("the bare github.com names do not warn", {
  evs <- gitcreds$gitcreds_cache_envvars("https://github.com")
  reset_legacy_warning()

  withr::local_envvar(c(
    GITCREDS_PAT_GITHUB_COM = NA_character_,
    GITHUB_PAT_GITHUB_COM = NA_character_,
    GITHUB_TOKEN = NA_character_,
    GITHUB_PAT = "bare-token"
  ))
  expect_silent(gitcreds$gitcreds_get_cache(evs))
})

test_that("gitcreds_delete_cache() clears every accepted name", {
  withr::local_envvar(c(
    GITCREDS_PAT_GITLAB_COM = "new-token",
    GITHUB_PAT_GITLAB_COM = "legacy-token"
  ))
  gitcreds$gitcreds_delete_cache(
    gitcreds$gitcreds_cache_envvars("https://gitlab.com")
  )
  expect_equal(Sys.getenv("GITCREDS_PAT_GITLAB_COM", "unset"), "unset")
  expect_equal(Sys.getenv("GITHUB_PAT_GITLAB_COM", "unset"), "unset")
})

test_that("bare GITHUB_PAT applies to github.com, but loses to a prefix", {
  evs <- gitcreds$gitcreds_cache_envvars("https://github.com")

  withr::local_envvar(c(
    GITCREDS_PAT_GITHUB_COM = NA_character_,
    GITHUB_PAT_GITHUB_COM = NA_character_,
    GITHUB_TOKEN = NA_character_,
    GITHUB_PAT = "bare-token"
  ))
  expect_equal(gitcreds$gitcreds_get_cache(evs)$password, "bare-token")

  withr::local_envvar(c(GITHUB_PAT_GITHUB_COM = "prefixed-token"))
  reset_legacy_warning()
  expect_warning(
    expect_equal(gitcreds$gitcreds_get_cache(evs)$password, "prefixed-token")
  )
})

test_that("bare names do not apply to other hosts", {
  evs <- gitcreds$gitcreds_cache_envvars("https://gitlab.com")
  withr::local_envvar(c(
    GITCREDS_PAT_GITLAB_COM = NA_character_,
    GITHUB_PAT_GITLAB_COM = NA_character_,
    GITHUB_PAT = "bare-token"
  ))
  expect_null(gitcreds$gitcreds_get_cache(evs))
})
