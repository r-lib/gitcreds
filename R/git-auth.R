
# ------------------------------------------------------------------------
# Public API
# ------------------------------------------------------------------------

#' @export

gitcreds_get <- function(url = "https://github.com") {

  stopifnot(is_string(url), has_no_newline(url))
  if (!check_for_git()) stop("You need to install git")

  helper <- paste0(
    "credential.helper=\"! echo protocol=dummy;",
    "echo host=dummy;",
    "echo username=dummy;",
    "echo password=dummy\""
  )

  out <- gitcreds_fill(list(url = url), c("-c", helper))

  parse_gitcreds_output(out)
}

#' @export

gitcreds_list_helpers <- function() {
  if (!check_for_git()) stop("You need to install git")
  out <- system2("git", c("config", "--get-all", "credential.helper"),
                 stdout = TRUE, stderr = null_file())
  clear <- rev(which(out == ""))
  if (length(clear)) out <- out[-(1:clear[1])]
  out
}

#' @export

print.gitcreds <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export

format.gitcreds <- function(x, ...) {
  nms <- names(x)
  vls <- unlist(x, use.names = FALSE)
  vls[nms == "password"] <- "<-- hidden, use $password to print -->"
  c("<gitcreds>", paste0(format(nms), ": ", vls))
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
  ## TODO: should this just fail?
  out <- tryCatch(
    suppressWarnings(system2(
      "git", args, input = input, stdout = TRUE, stderr = null_file()
    )),
    error = function(e) NULL
  )

  ## TODO should this just fail?
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    out <- NULL
  }

  out
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
  tryCatch({
    suppressWarnings(system2(
      "git", "--version",
      stdout = TRUE, stderr = null_file()
    ))
    TRUE
  }, error = function(e) FALSE)
}

parse_gitcreds_output <- function(txt) {
  if (is.null(txt)) return(NULL)
  if (txt[1] == "protocol=dummy") return(NULL)
  nms <- sub("=.*$", "", txt)
  vls <- sub("^[^=]+=", "", txt)
  structure(as.list(vls), names = nms, class = "gitcreds")
}

gitcreds_username <- function(url = NULL) {
  if (!check_for_git()) stop("You need to install git")
  gitcreds_username_for_url(url) %||% gitcreds_username_generic()
}

gitcreds_username_for_url <- function(url) {
   git_run(c(
    "config", "--get-urlmatch", "credential.username", shQuote(url)
  ))
}

gitcreds_username_generic <- function() {
  git_run(c("config", "credential.username"))
}

default_username <- function() {
  if (.Platform$OS.type == "windows") "PersonalAccessToken" else "token"
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
