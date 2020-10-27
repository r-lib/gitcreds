
is_ci <- function() {
  Sys.getenv("CI", "") != ""
}

gc_test_that <- function(desc, code, os = NULL, helpers = NULL) {
  if (!is_ci()) return()
  if (!is.null(os)) {
    if (! get_os() %in% os) return()
  }

  if (is.null(helpers)) {
    os <- get_os()
    if (os == "windows") {
      helpers <- c("manager", "manager-core")
    } else if (os == "macos") {
      helpers <- c("osxkeychain", "manager-core")
    } else {
      helpers <- character()
    }
  }

  # Need to create a closure here, because mockery will create the
  # stubs in that environment
  code <- substitute(code)
  lapply(helpers, function(helper) {
    local_helpers(helper)
    withr::local_envvar(GITCREDS_TEST_HELPER = helper)
    test <- substitute(gc_test_that_run(desc, code), list(code = code))
    eval(test)
  })
}

gc_test_that_run <- function(desc, code) {
  if (interactive() && !isTRUE(getOption("gitcreds_test_consent"))) {
    ch <- utils::menu(
      title = paste0("\n\nThis testsuite will delete your ",
                     "git credentials and config !!!"),
      choices = c("Yeah, fine.", "Wait, what?")
    )
    if (ch == 2) { cat("\nAborting...\n"); invokeRestart("abort") }
    options(gitcreds_test_consent = TRUE)
  }

  if (get_os() == "windows") {
    cleanup_windows()
    on.exit(cleanup_windows(), add = TRUE)
  }
  if (get_os() == "macos")  {
    cleanup_macos()
    on.exit(cleanup_macos(), add = TRUE)
  }

  envnames <- grep("^GITHUB_PAT", names(Sys.getenv()), value = TRUE)
  envs <- structure(rep(NA_character_, length(envnames)), names = envnames)
  withr::local_envvar(c(
    envs,
    GCM_AUTHORITY = NA_character_,
    GCM_PROVIDER = NA_character_
  ))

  test_that(desc, code)
}

cleanup_windows <- function() {
  cleanup_windows_manager()
  cleanup_windows_manager_core()
}

cleanup_windows_manager <- function() {
  recs <- gitcreds_list(credential_helper = "manager", url = NULL)
  for (rec in recs) {
    try_silently({
      cred <- list(
        url = sub("^git:", "", rec$target_name),
        username = rec$username
      )
      gitcreds_reject(cred, c("-c", "credential.helper=manager"))
    })
  }
}

cleanup_windows_manager_core <- function() {
  recs <- gitcreds_list(credential_helper = "manager-core", url = NULL)
  for (rec in recs) {
    try_silently({
      cred <- list(
        url = sub("^git:", "", rec$target_name),
        username = rec$username
      )
      gitcreds_reject(cred, c("-c", "credential.helper=manager-core"))
    })
  }
}

cleanup_macos <- function() {
  cleanup_macos_osxkeychain()
  cleanup_macos_manager_core()
}

try_silently <- function(expr) {
  try(expr, silent = TRUE)
}

cleanup_macos_osxkeychain <- function() {
  recs <- gitcreds_list(credential_helper = "osxkeychain", url = NULL)
  for (rec in recs) {
    try_silently({
      cred <- list(
        host = rec$attributes$server,
        protocol = rec$attributes$protocol,
        username = rec$attributes$account
      )
      gitcreds_reject(cred, c("-c", "credential.helper=osxkeychain"))
    })
  }
}

cleanup_macos_manager_core <- function() {
  recs <- gitcreds_list(credential_helper = "manager-core", url = NULL)
  for (rec in recs) {
    try_silently({
      cred <- list(
        url = sub("^git:", "", rec$attributes$service),
        username = rec$attributes$account
      )
      gitcreds_reject(cred, c("-c", "credential.helper=manager-core"))
    })
  }
}

clear_helpers <- function() {
  # Might return startus 5 if no helpers are set
  try_silently(git_run(c("config", "--global", "--unset-all", "credential.helper")))
  try_silently(git_run(c("config", "--global", "--remove-section", "credential")))
}

local_helpers <- function(helpers, .local_envir = parent.frame()) {
  withr::defer(clear_helpers(), envir = .local_envir)
  clear_helpers()
  git_run(c("config", "--global", "--add", "credential.helper", "\"\""))
  for (helper in helpers) {
    git_run(c("config", "--global", "--add", "credential.helper", helper))
  }
}
