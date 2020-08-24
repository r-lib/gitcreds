
# ------------------------------------------------------------------------
# Public API
# ------------------------------------------------------------------------

#' Query and set git credentials
#'
#' These functions use the `git credential` system command to query and set
#' git credentials. They need an external git installation. You can
#' download git from https://git-scm.com/downloads. A recent version, but
#' at least git 2.9 is suggested.
#'
#' @details
#' `gitcreds_get()` queries git credentials. It is tyically used by package
#' code that needs to authenticate to GitHub or another git repository.
#'
#' `gitcreds_set()` sets git credentials. It is typically called by the
#' user, and it only works in interactive sessions. To set up password-less
#' authentication to GitHub, first create a personal access token (PAT). See
#' https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token.
#' Then give this token as the password for `gitcreds_set()`.
#'
#' `gitcreds_list_helpers()` lists the active credential helpers.
#'
#' # Advanced topics
#'
#' ## The current working directory
#'
#' TODO
#'
#' ## Multiple credential helpers
#'
#' TODO
#'
#' ## Non-GitHUb accounts
#'
#' TODO
#'
#' ## Multiple GitHub accounts
#'
#' TODO
#'
#' ## Notes on various credential helpers
#'
#' ### The default macOS helper (`osxkeychain`)
#'
#' TODO
#'
#' ### Git Credential Manager for Windows (`manager`)
#'
#' TODO
#'
#' ### Git Credential Manager Core (`manager-core`)
#'
#' TODO
#'
#' @param url URL to get or set credentials for. It may contain a user
#' name, which is typically (but not always) used by the credential
#' helpers. It may also contain a path, which is typically (but not always)
#' ignored by the credential helpers.
#'
#' @return `gitcreds_get()` returns a `gitcreds` object, a named list
#' of strings, the fields returned by the git credential handler.
#' Typically the fields are `protocol`, `host`, `username`, `password`.
#' Some credential helpers support path-dependent credentials and also
#' return a `path` field.
#'
#' @export
#' @examples
#' \dontrun{
#' gitcreds_get()
#' gitcreds_get("https://github.com")
#' gitcreds_get("https://myuser@github.com/myorg/myrepo")
#' }

gitcreds_get <- function(url = "https://github.com") {

  stopifnot(is_string(url), has_no_newline(url))
  check_for_git()

  helper <- paste0(
    "credential.helper=\"! echo protocol=dummy;",
    "echo host=dummy;",
    "echo username=dummy;",
    "echo password=dummy\""
  )

  out <- gitcreds_fill(list(url = url), c("-c", helper))

  parse_gitcreds_output(out, url)
}

#' @export
#' @rdname gitcreds_get
#' @return `gitcreds_set()` returns `NULL`, invisibly.

gitcreds_set <- function(url = "https://github.com") {
  if (!interactive()) {
    throw(new_error(
      "gitcreds_not_interactive_error",
      message = "`gitcreds_set()` only works in interactive sessions"
    ))
  }
  stopifnot(is_string(url), has_no_newline(url))
  check_for_git()

  current <- gitcreds_get(url)

  if (!is.null(current)) {
    gitcreds_set_replace(url, current)
  } else {
    gitcreds_set_new(url)
  }
}

#' Replace credentials with new ones
#'
#' It only works interactively, because of `menu()` in `ack_replace()` and
#' `readline()`.
#'
#' We need to set a username, it is compulsory for git credential.
#' 1. If there was one in the url, then we use that.
#' 2. Otherwise if git has a username configured for this URL, we use that.
#' 3. Otherwise we use the username in the credentials we are replacing.
#'
#' @param url URL.
#' @param current Must not be `NULL`, and it must contain a
#' `gitcreds` object. (Well, a named list, really.)
#' @return Nothing.
#'
#' @noRd

gitcreds_set_replace <- function(url, current) {
  if (!ack_replace(url, current)) {
    throw(new_error("gitcreds_abort_replace_error"))
  }

  pat <- readline("\n? Enter new password or token: ")

  username <- get_url_username(url) %||%
    gitcreds_username(url) %||%
    current$username

  msg("-> Removing current credentials...")
  gitcreds_reject(current)

  msg("-> Adding new credentials...")
  gitcreds_approve(list(url = url, username = username, password = pat))

  msg("-> Done.")

  invisible()
}

#' Set new credentials
#'
#' This should not replace or remove any old credentials, but of course
#' we cannot be sure, because credential helpers pretty much do what they
#' want.
#'
#' We need to set a username, it is compulsory for git credential.
#' 1. If there was one in the url, then we use that.
#' 2. Otherwise if git has a username configured for this URL, we use that.
#' 3. Otherwise we use a default username.
#'
#' @param url URL.
#' @return Nothing.
#' @noRd

gitcreds_set_new <- function(url) {
  pat <- readline("\n? Enter new password or token: ")

  username <- get_url_username(url) %||%
    gitcreds_username(url) %||%
    default_username()

  msg("-> Adding new credentials...")
  gitcreds_approve(list(url = url, username = username, password = pat))

  msg("-> Done.")

  invisible()
}

#' @return `gitcreds_list_helpers()` returns a character vector,
#' corresponding to the `credential.helper` git configuration key.
#' Usually it contains a single credential helper, but it is possible to
#' configure multiple helpers.
#'
#' @rdname gitcreds_get
#' @export

gitcreds_list_helpers <- function() {
  check_for_git()
  out <- git_run(c("config", "--get-all", "credential.helper"))
  clear <- rev(which(out == ""))
  if (length(clear)) out <- out[-(1:clear[1])]
  out
}

#' @export

print.gitcreds <- function(x, header = TRUE, ...) {
  cat(format(x, header = header, ...), sep = "\n")
}

#' @export

format.gitcreds <- function(x, header = TRUE, ...) {
  nms <- names(x)
  vls <- unlist(x, use.names = FALSE)
  vls[nms == "password"] <- "<-- hidden -->"
  c(
    if (header) "<gitcreds>",
    paste0("  ", format(nms), ": ", vls)
  )
}

# ------------------------------------------------------------------------
# Raw git credential API
# ------------------------------------------------------------------------

#' Run `git credential fill`
#'
#' @param input Named list to pass to the credential helper.
#' @param args Extra args, used _before_ `fill`, to allow
#' `git -c ... fill`.
#' @return Standard output, line by line.
#'
#' @seealso [gitcreds_run()] and [git_run()].
#' @noRd

gitcreds_fill <- function(input, args = character()) {
  gitcreds_run("fill", input, args)
}

#' Run `git credential approve` to add new credentials
#'
#' @inheritParams gitcreds_fill
#' @param creds `gitcreds` object (named list).
#' @return Standard output, line by line.
#'
#' @seealso [gitcreds_run()] and [git_run()].
#' @noRd

gitcreds_approve <- function(creds, args = character()) {
  gitcreds_run("approve", creds, args)
}

#' Run `git credential reject` to remove credentials
#'
#' @inheritParams gitcreds_fill
#' @param creds `gitcreds` object (named list).
#' @return Standard output, line by line.
#'
#' @seealso [gitcreds_run()] and [git_run()].
#' @noRd

gitcreds_reject <- function(creds, args = character()) {
  gitcreds_run("reject", creds, args)
}

#' Run a `git credential` command
#'
#' @details
#' We set the [gitcreds_env()] environment variables, to avoid dialog boxes
#' from some credential helpers and also validation that potentiall needs
#' an internet connection.
#'
#' @param command Command name, e.g. `"fill"`.
#' @param input Named list of input, see
#' https://git-scm.com/docs/git-credential#IOFMT
#' @param args Extra command line arguments, added after `git` and
#' _before_ `command`, to allow `git -c ... fill`.
#' @return Standard output, line by line.
#'
#' @seealso [git_run()].
#' @noRd

gitcreds_run <- function(command, input, args = character()) {
  env <- gitcreds_env()
  oenv <- set_env(env)
  on.exit(set_env(oenv), add = TRUE)

  stdin <- create_gitcreds_input(input)

  git_run(c(args, "credential", command), input = stdin)
}

# ------------------------------------------------------------------------
# Helpers specific to git
# ------------------------------------------------------------------------

#' Run a git command
#'
#' @details
#' Currently we don't set the credential specific environment variables
#' here, and credential helpers invoked by `git` behave the same way as
#' they would from the command line.
#'
#' ## Errors
#'
#' On error `git_run()` returns an error with class `git_error` and
#' also `gitcreds_error`. The error object includes
#' * `args` the command line arguments,
#' * `status`: the exit status of the command,
#' * `stdout`: the standard output of the command, line by line.
#' * `stderr`: the standard error of the command, line by line.
#'
#' @param args Command line arguments.
#' @param input The standard input (the `input` argument of [system2()].
#' @return Standard output, line by line.
#'
#' @noRd

git_run <- function(args, input = NULL) {
  stderr_file <- tempfile("gitcreds-stderr-")
  on.exit(unlink(stderr_file, recursive = TRUE), add = TRUE)
  out <- tryCatch(
    suppressWarnings(system2(
      "git", args, input = input, stdout = TRUE, stderr = stderr_file
    )),
    error = function(e) NULL
  )

  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    throw(new_error(
      "git_error",
      args = args,
      stdout = out,
      status = attr(out, "status"),
      stderr = read_file(stderr_file)
    ))
  }

  out
}

#' Request confirmation from the user, to replace credentials
#'
#' This function only works in interactive sessions.
#'
#' @param url URL to set new credentials for.
#' @param current The current credentials.
#' @return `FALSE` is the user changed their mind, to keep the current
#' credentials. `TRUE` for replacing them.
#'
#' @seealso [gitcreds_set()].
#' @noRd

ack_replace <- function(url, current) {
  msg("\n-> Your current credentials for ", squote(url), ":\n")
  msg(paste0(format(current, header = FALSE), collapse = "\n"), "\n")

  choices <- c(
    "Keep these credentials",
    "Replace these credentials",
    if (has_password(current)) "See the password / token"
  )

  repeat {
    ch <- utils::menu(title = "-> What would you like to do?", choices)

    if (ch == 1) return(FALSE)
    if (ch == 2) return(TRUE)

    msg("\nCurrent password: ", current$password, "\n\n")
  }
}

#' Whether a `gitcreds` credential has a non-empty `password`
#'
#' This is usually `TRUE`.
#'
#' @param creds `gitcreds`
#' @return `TRUE` is there is a `password`
#'
#' @noRd

has_password <- function(creds) {
  is_string(creds$password) && creds$password != ""
}

#' Create a string that can be passed as standard input to `git credential`
#' commands
#'
#' @param args Usually a `gitcreds` object, but can be a named list in
#' general. This is a format: https://git-scm.com/docs/git-credential#IOFMT
#' @return String.
#'
#' @noRd

create_gitcreds_input <- function(args) {
  paste0(
    paste0(names(args), "=", args, collapse = "\n"),
    "\n\n"
  )
}

#' Environment to set for all `git credential` commands.
#' @noRd

gitcreds_env <- function() {
  # Avoid interactivity and validation with some common credential helpers
  c(
    GCM_INTERACTIVE = "Never",
    GCM_MODAL_PROMPT = "false",
    GCM_VALIDATE = "false"
  )
}

#' Check if `git` is installed and can run
#'
#' If not installed, a `gitcreds_nogit_error` is thrown.
#'
#' @return Nothing
#' @noRd

check_for_git <- function() {
  # This is simpler than Sys.which(), and also less fragile
  has_git <- tryCatch({
    suppressWarnings(system2(
      "git", "--version",
      stdout = TRUE, stderr = null_file()
    ))
    TRUE
  }, error = function(e) FALSE)

  if (!has_git) throw(new_error("gitcreds_nogit_error"))
}

#' Parse standard output from `git credential fill`
#'
#' @details
#' For dummy credentials (i.e. the lack of credentials), it throws an
#' error of class `gitcreds_no_credentials`.
#'
#' @param txt Character vector, standard output lines from
#' `git credential fill`.
#' @param url URL we queried, to be able to create a better error message.
#' @return `gitcreds` object.
#'
#' @noRd

parse_gitcreds_output <- function(txt, url) {
  if (is.null(txt) || txt[1] == "protocol=dummy") {
    throw(new_error("gitcreds_no_credentials", url = url))
  }
  nms <- sub("=.*$", "", txt)
  vls <- sub("^[^=]+=", "", txt)
  structure(as.list(vls), names = nms, class = "gitcreds")
}

#' Query the `username` to use for `git config credential`
#'
#' @details
#' The current working directory matters for this command, as you can
#' configure `username` in a local `.git/config` file (via
#' `git config --local`).
#'
#' @param url URL to query the username for, or `NULL`. If not `NULL`,
#' then we first try to query an URL-specific username. See
#' https://git-scm.com/docs/gitcredentials for more about URL-specific
#' credential config
#' @return A string with the username, or `NULL` if no default was found.
#'
#' @noRd

gitcreds_username <- function(url = NULL) {
  gitcreds_username_for_url(url) %||% gitcreds_username_generic()
}

gitcreds_username_for_url <- function(url) {
  if (is.null(url)) return(NULL)
  tryCatch(
    git_run(c(
      "config", "--get-urlmatch", "credential.username", shQuote(url)
    )),
    git_error = function(err) {
      if (err$status == 1) NULL else throw(err)
    }
  )
}

gitcreds_username_generic <- function() {
  tryCatch(
    git_run(c("config", "credential.username")),
    git_error = function(err) {
      if (err$status == 1) NULL else throw(err)
    }
  )
}

#' User name to use when creating a credential, if there is nothing better
#'
#' These user names are typical for some git tools.
#'
#' @noRd

default_username <- function() {
  if (.Platform$OS.type == "windows") "PersonalAccessToken" else "token"
}

# ------------------------------------------------------------------------
# Errors
# ------------------------------------------------------------------------

gitcred_errors <- function() {
  c(
    git_error = "System git failed",
    gitcreds_nogit_error = "Could not find system git",
    gitcreds_not_interactive_error = "gitcreds needs an interactive session",
    gitcreds_abort_replace_error = "User aborted updating credentials",
    gitcreds_no_credentials = "Could not find any credentials"
  )
}

new_error <- function(class, ..., message = "", call. = TRUE, domain = NULL) {
  if (message == "") message <- gitcred_errors()[[class]]
  message <- .makeMessage(message, domain = domain)
  cond <- list(message = message, ...)
  if (call.) cond$call <- sys.call(-1)
  class(cond) <- c(class, "gitcreds_error", "error", "condition")
  cond
}

throw <- function(cond) {
  stop(cond)
}

# ------------------------------------------------------------------------
# Genetic helpers
# ------------------------------------------------------------------------

#' Set/remove env var and return the old values
#'
#' @param envs Named character vector or list of env vars to set. `NA`
#' values will un-set an env var.
#' @return Character vector, the old values of the supplied environment
#' variables, `NA` for the ones that were not set.
#'
#' @noRd

set_env <- function(envs) {
  current <- Sys.getenv(names(envs), NA_character_)
  na <- is.na(envs)
  if (any(na)) {
    Sys.unsetenv(names(envs)[na])
  }
  if (any(!na)) {
    do.call("Sys.setenv", as.list(envs[!na]))
  }
  invisible(current)
}

#' Get the user name from a `protocol://username@host/path` URL.
#'
#' @param url URL
#' @return String or `NULL` if `url` does not have a username.
#'
#' @noRd

get_url_username <- function(url) {
  nm <- parse_url(url)$username
  if (nm == "") NULL else nm
}

#' Parse URL
#'
#' It does not parse query parameters, as we don't deal with them here.
#' The port number is included in the host name, if present.
#'
#' @param url Character vector of one or more URLs.
#' @return Data frame with string columns: `protocol`, `username`,
#' `password`, `host`, `path`.
#'
#' @noRd

parse_url <- function(url) {
  re_url <- paste0(
    "^(?<protocol>https?)://",
    "(?:(?<username>[^@/:]+)(?::(?<password>[^@/]+))?@)?",
    "(?<host>[^/]+)",
    "(?<path>.*)$"            # don't worry about query params here...
  )

  mch <- re_match(url, re_url)
  mch[, setdiff(colnames(mch), c(".match", ".text")), drop = FALSE]
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

has_no_newline <- function(url) {
  ! grepl("\n", url, fixed = TRUE)
}

# From the rematch2 package

re_match <- function(text, pattern, perl = TRUE, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  res
}

null_file <- function() {
  if (.Platform$OS.type == "windows") "nul:" else "/dev/null"
}

`%||%` <- function(l, r) if (is.null(l)) r else l

#' Like [message()], but print to standard output in interactive
#' sessions
#'
#' To avoid red output in RStudio, RGui, and R.app.
#'
#' @inheritParams message
#' @return Nothing
#'
#' @noRd

msg <- function(..., domain = NULL, appendLF = TRUE) {
  cnd <- .makeMessage(..., domain = domain, appendLF = appendLF)
  withRestarts(muffleMessage = function() NULL, {
    signalCondition(simpleMessage(msg))
    output <- default_output()
    cat(cnd, file = output, sep = "")
  })
  invisible()
}

#' Where to print messages to
#'
#' If the session is not interactive, then it potentially matters
#' whether we print to stdout or stderr, so we print to stderr.
#'
#' The same applies when there is a sink for stdout or stderr.
#'
#' @return The connection to print to.
#'
#' @noRd

default_output <- function() {
  if (is_interactive() && no_active_sink()) stdout() else stderr()
}

no_active_sink <- function() {
  # See ?sink.number for the explanation
  sink.number("output") == 0 && sink.number("message") == 2
}

#' Smarter `interactive()`
#'
#' @noRd

is_interactive <- function() {
  opt <- getOption("rlib_interactive")
  opt2 <- getOption("rlang_interactive")
  if (isTRUE(opt)) {
    TRUE
  } else if (identical(opt, FALSE)) {
    FALSE
  } else if (isTRUE(opt2)) {
    TRUE
  } else if (identical(opt2, FALSE)) {
    FALSE
  } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
    FALSE
  } else if (tolower(getOption("rstudio.notebook.executing", "false")) == "true") {
    FALSE
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    interactive()
  }
}

#' Squote wrapper to avoid smart quotes
#'
#' @inheritParams sQuote
#' @inherit sQuote return
#' @noRd

squote <- function(x) {
  old <- options(useFancyQuotes = FALSE)
  on.exit(options(old), add = TRUE)
  sQuote(x)
}

#' Read all of a file
#'
#' @param path File to read.
#' @param ... Passed to [readChar()].
#' @return String.
#'
#' @noRd

read_file <- function(path, ...) {
  readChar(path, nchars = file.info(path)$size, ...)
}
