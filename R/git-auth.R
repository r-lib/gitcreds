
# ------------------------------------------------------------------------
# Public API
# ------------------------------------------------------------------------

#' @export

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

gitcreds_set_replace <- function(url, current) {
  if (!ack_replace(url, current)) {
    throw(new_error("gitcreds_abort_replace_error"))
  }

  pat <- readline("\n? Enter new password or token: ")

  # We need to set a username, it is compulsory for git credential.
  # 1. If there was one in the url, then we use that.
  # 2. Otherwise if git has a username configured for this URL, we use that.
  # 3. Otherwise we use the username in the credentials we are replacing.

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

gitcreds_set_new <- function(url) {
  pat <- readline("\n? Enter new password or token: ")

  # We need to set a username, it is compulsory for git credential.
  # 1. If there was one in the url, then we use that.
  # 2. Otherwise if git has a username configured for this URL, we use that.
  # 3. Otherwise we use a default username.

  username <- get_url_username(url) %||%
    gitcreds_username(url) %||%
    default_username()

  msg("-> Adding new credentials...")
  gitcreds_approve(list(url = url, username = username, password = pat))

  msg("-> Done.")

  invisible()
}

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

gitcreds_fill <- function(input, args = character()) {
  gitcreds_run("fill", input, args)
}

gitcreds_approve <- function(creds, args = character()) {
  gitcreds_run("approve", creds, args)
}

gitcreds_reject <- function(creds, args = character()) {
  gitcreds_run("reject", creds, args)
}

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
      status = attr(out, "status"),
      stderr = read_file(stderr_file)
    ))
  }

  out
}

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

has_password <- function(creds) {
  is_string(creds$password) && creds$password != ""
}

create_gitcreds_input <- function(args) {
  paste0(
    paste0(names(args), "=", args, collapse = "\n"),
    "\n\n"
  )
}

gitcreds_env <- function() {
  # Avoid interactivity and validation with some common credential helpers
  c(
    GCM_INTERACTIVE = "Never",
    GCM_MODAL_PROMPT = "false",
    GCM_VALIDATE = "false"
  )
}

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

parse_gitcreds_output <- function(txt, url) {
  if (is.null(txt) || txt[1] == "protocol=dummy") {
    throw(new_error("gitcreds_no_credentials", url = url))
  }
  nms <- sub("=.*$", "", txt)
  vls <- sub("^[^=]+=", "", txt)
  structure(as.list(vls), names = nms, class = "gitcreds")
}

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

get_url_username <- function(url) {
  nm <- parse_url(url)$username
  if (nm == "") NULL else nm
}

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

msg <- function(..., domain = NULL, appendLF = TRUE) {
  cnd <- .makeMessage(..., domain = domain, appendLF = appendLF)
  withRestarts(muffleMessage = function() NULL, {
    signalCondition(simpleMessage(msg))
    output <- default_output()
    cat(cnd, file = output, sep = "")
  })
}

default_output <- function() {
  if (is_interactive() && no_active_sink()) stdout() else stderr()
}

no_active_sink <- function() {
  sink.number("output") == 0 && sink.number("message") == 2
}

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

squote <- function(x) {
  sQuote(x, FALSE)
}

read_file <- function(path, ...) {
  readChar(path, nchars = file.info(path)$size, ...)
}
