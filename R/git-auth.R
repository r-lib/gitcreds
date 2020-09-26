
# ------------------------------------------------------------------------
# Public API
# ------------------------------------------------------------------------

#' Query and set git credentials
#'
#' This manual page is for _users_ of packages that depend on gitcreds
#' for managing tokens or passwords to GitHub or other git repositories.
#' If you are a package author and want to import gitcreds for this
#' functionality, see `vignette("package", package = "gitcreds")`.
#' Otherwise please start at 'Basics' below.
#'
#' # Basics
#'
#' `gitcreds_get()` queries git credentials. It is typically used by package
#' code that needs to authenticate to GitHub or another git repository.
#' The end user might call it to checks that credentials are properly set
#' up.
#'
#' `gitcreds_set()` add or updates git credentials in the credential store.
#' It is typically called by the user, and it only works in interactive
#' sessions. It always asks for acknowledgement before it overwrites
#' existing credentials.
#'
#' `gitcreds_delete()` deletes git credentials from the credential store.
#' It is typically called by the user, and it only works in interactive
#' sessions. It always asks for acknowledgement.
#'
#' `gitcreds_list_helpers()` lists the active credential helpers.
#'
#' These functions use the `git credential` system command to query and set
#' git credentials. They need an external git installation. You can
#' download git from https://git-scm.com/downloads. A recent version, but
#' at least git 2.9 is suggested.
#'
#' If you want to avoid installing git, see 'Environment variables' below.
#'
#' ## GitHub
#'
#' ### New setup
#'
#' To set up password-less authentication to GitHub:
#' 1. create a personal access token (PAT). See
#'    https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token.
#' 2. Call `gitcreds_set()` and give this token as the password.
#' 3. Run `gitcreds_get(use_cache = FALSE)` to check that the new
#'    PAT is set up. To see the token, you can run
#'    `gitcreds_get(use_cache = FALSE)$password`.
#'
#' ### Migrating from the `GITHUB_PAT` environment variable
#'
#' If you already have a GitHub token, and use the `GITHUB_PAT` or
#' `GITHUB_TOKEN` environment variable in your `.Renviron` file or
#' elsewhere, no changes are neccessary. gitcreds will automatically use
#' this variable.
#'
#' However, we still suggest that you add your token to the git credential
#' store with `gitcreds_set()` and remove `GITHUB_PAT` from your
#' `.Renviron` file. The credential store is more secure than storing
#' tokens in files.
#'
#' # Advanced topics
#'
#' ## Cached credentials
#'
#' Because querying the git credential store might not be very fast,
#' `gitcreds_get()` caches credentials in environment variables by default.
#' Credentials for different URLs are stored in different environment
#' variables. The name of the environment variable is calculated with
#' [gitcreds_cache_envvar()].
#'
#' To remove the cache, remove this environment variable with
#' [Sys.unsetenv()].
#'
#' ## Environment variables
#'
#' If you want to avoid installing git, or using the credential store for
#' some reason, you can supply credentials in environment variables, e.g.
#' via the `.Renviron` file. Use [gitcreds_cache_envvar()] to query the
#' environment variable you need to set for a URL:
#'
#' 1. You can set this environment variable to the token or password itself.
#' 2. If you also need a user name, then use the `user:password` form, i.e.
#'    separate them with a colon. (If your user name or passwrd has `:`
#'    characters, then you need to escape them with a preceding backslash.)
#'
#' ## Credential helpers
#'
#' git credential helpers are an extensible, configurable mechanism to
#' store credentials. Different git installations have different credentials
#' helpers. On Windows the default helper stores credentials in the system
#' credential store. On macOS, it stores them in the macOS Keychain.
#' Other helpers cache credentials in a server process or in a file on the
#' file system.
#'
#' gitcreds only works if a credential helper is configured. For the current
#' git version (2.28.0), this is the case by default on Windows and macOS
#' (for git from HomeBrew), but most Linux distributions do not set up a
#' default credential helper.
#'
#' You can use `gitcreds_list_helpers()` to see the _active_ credential
#' helper(s) for a repository.
#'
#' ## The current working directory
#'
#' git allows repository specific configuration, via the `.git/config` file.
#' The `config` file might specify a different credential helper, a
#' different user name, etc. This means that `gitcreds_get()` etc. will
#' potentially work differently depending on the current working
#' directory. This is especially relevant for package code that changes
#' the working directory temporarily.
#'
#' ## Non-GitHub accounts
#'
#' Non-GitHub URLs work mostly the same way as GitHub URLs.
#' `gitcreds_get()` and `gitcreds_set()` default to GitHub, so you'll need
#' to explicitly set their `url` argument.
#'
#' Some credential helpers, e.g. Git Credential Manager for Windows
#' (`manager`) and Git Credential Manager Core (`manager-core`) work
#' slightly differently for GitHub and non-GitHub URLs, see their
#' documentation for details.
#'
#' ## Multiple accounts
#'
#' The various credential helpers support multiple accounts in different
#' ways. Here are our recommendations.
#'
#' ### macOS
#'
#' 1. Use the (currently default) `osxkeychain` credential helper.
#' 2. In Keychain Access, remove all your current credentials for the
#'    host(s) you are targeting. E.g. for GitHub, search for github.com
#'    Internet Passwords.
#' 3. Then add the credential that you want to use for "generic access".
#'    This is the credential that will be used for URLs without user
#'    names. The user name for this credential does not matter, but you
#'    can choose something descriptive, e.g. "token", or "generic".
#'4.  Configure git to use this username by default. E.g. if you chose
#'    "generic", then run
#'
#'        git config --global crendetial.username generic
#'
#' 5.  Add all the other credentials, with appropriate user names. These
#'     are the user names that you need to put in the URLs for the
#'     repositories or operations you want to use them for. (GitHub does
#'     not actually use the user names if the password is a PAT, but they
#'     are used to look up the correct token in the credential store.)
#'
#' ### Windows
#'
#' #### A single GitHub account
#'
#' If you only need to manage a single github.com credential, together with
#' possibly multiple credentials to other hosts (including GitHub
#' Enterprise hosts), then you can use the default `manager` helper, and
#' get away with the default auto-detected GCM authority setting.
#'
#' In this case, you can add you github.com credential with an arbitrary
#' user name, and for each other host you can configure a default user
#' name, and/or include user names in the URLs to these hosts. This is how
#' to set a default user name for a host:
#'
#' ```
#' git config --global credential.https://example.com.username myusername
#' ```
#'
#' #### Multiple GitHub credentials
#'
#' If you need to manage multiple github.com credentials, then you can
#' still use the `manager` helper, but you need to change the GCM authority
#' by setting an option or an environment variable, see
#' <https://github.com/microsoft/Git-Credential-Manager-for-Windows/blob/master/Docs/Configuration.md#authority.>
#' Once
#' <https://github.com/microsoft/Git-Credential-Manager-for-Windows/pull/891>
#' is merged, you won't need to do this. (At least in recent git versions,
#' that contain a GCM build with the fix.)
#'
#' This is how to change GCM authority in the config:
#'
#' ```
#' git config --global credential.authority Basic
#' ```
#'
#' You can also change it only for github.com:
#'
#' ```
#' git config --global credential.github.com.authority Basic
#' ```
#'
#' Then you can configure a default user name, this will be used for URLs
#' without a user name:
#'
#' ```
#' git config --global credential.username generic
#' ```
#'
#' Now you can add you credentials, the default one with the "generic" user
#' name, and all the others with their specific user and host names.
#'
#' ## Multiple credential helpers
#'
#' It is possible to configure multiple credential helpers. If multiple
#' helpers are configured for a repository, then `gitcreds_get()` will
#' go over them until a credential is found. `gitcreds_set()` will set the
#' new credentials in _every_ configured credential helper.
#'
#' You can use [gitcreds_list_helpers()] to list all configured helpers.
#'
#' @param url URL to get, set or delete credentials for. It may contain a
#' user name, which is typically (but not always) used by the credential
#' helpers. It may also contain a path, which is typically (but not always)
#' ignored by the credential helpers.
#' @param use_cache Whether to try to use the environment variable cache
#' before turning to git to look up the credentials for `url`.
#' See [gitcreds_cache_envvar()].
#' @param set_cache Whether to set the environment variable cache after
#' receiving the credentials from git. See [gitcreds_cache_envvar()].
#'
#' @return `gitcreds_get()` returns a `gitcreds` object, a named list
#' of strings, the fields returned by the git credential handler.
#' Typically the fields are `protocol`, `host`, `username`, `password`.
#' Some credential helpers support path-dependent credentials and also
#' return a `path` field.
#'
#' `gitcreds_set()` returns nothing.
#'
#' `gitcreds_delete()` returns `FALSE` if it did not find find any
#' credentials to delete, and thus it did not call `git credential reject`.
#' Otherwise it returns `TRUE`.
#'
#' `gitcreds_get()` errors if git is not installed, no credential helpers
#' are configured or no credentials are found. `gitcreds_set()` errors if
#' git is not installed, or setting the new credentials fails.
#' `gitcreds_delete()` errors if git is not installed or the git calls fail.
#' See `vignette("package", package = "gitcreds")` if you want to handle
#' these errors.
#'
#' @aliases gitcreds
#' @export
#' @examples
#' \dontrun{
#' gitcreds_get()
#' gitcreds_get("https://github.com")
#' gitcreds_get("https://myuser@github.com/myorg/myrepo")
#' }

gitcreds_get <- function(url = "https://github.com", use_cache = TRUE,
                         set_cache = TRUE) {

  stopifnot(
    is_string(url), has_no_newline(url),
    is_flag(use_cache),
    is_flag(set_cache)
  )

  cache_ev <- gitcreds_cache_envvar(url)
  if (use_cache && !is.null(ans <- gitcreds_get_cache(cache_ev))) {
    return(ans)
  }

  check_for_git()

  out <- gitcreds_fill(list(url = url), dummy = TRUE)
  creds <- gitcreds_parse_output(out, url)

  if (set_cache) {
    gitcreds_set_cache(cache_ev, creds)
  }

  creds
}

#' @export
#' @rdname gitcreds_get

gitcreds_set <- function(url = "https://github.com") {
  if (!is_interactive()) {
    throw(new_error(
      "gitcreds_not_interactive_error",
      message = "`gitcreds_set()` only works in interactive sessions"
    ))
  }
  stopifnot(is_string(url), has_no_newline(url))
  check_for_git()

  current <- tryCatch(
    gitcreds_get(url, use_cache = FALSE, set_cache = FALSE),
    gitcreds_no_credentials = function(e) NULL
  )

  if (!is.null(current)) {
    gitcreds_set_replace(url, current)
  } else {
    gitcreds_set_new(url)
  }

  msg("-> Removing credetials from cache...")
  gitcreds_delete_cache(gitcreds_cache_envvar(url))

  msg("-> Done.")
  invisible()
}

#' Replace credentials with new ones
#'
#' It only works interactively, because of `menu()` in `ack()` and
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
  if (!ack(url, current, "Replace")) {
    throw(new_error("gitcreds_abort_replace_error"))
  }

  cat("\n")
  pat <- readline("? Enter new password or token: ")

  username <- get_url_username(url) %||%
    gitcreds_username(url) %||%
    current$username

  msg("-> Removing current credentials...")
  gitcreds_reject(current)

  msg("-> Adding new credentials...")
  gitcreds_approve(list(url = url, username = username, password = pat))

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
  cat("\n")
  pat <- readline("? Enter password or token: ")

  username <- get_url_username(url) %||%
    gitcreds_username(url) %||%
    default_username()

  msg("-> Adding new credentials...")
  gitcreds_approve(list(url = url, username = username, password = pat))

  invisible()
}

#' @export
#' @rdname gitcreds_get

gitcreds_delete <- function(url = "https://github.com") {
  if (!is_interactive()) {
    throw(new_error(
      "gitcreds_not_interactive_error",
      message = "`gitcreds_delete()` only works in interactive sessions"
    ))
  }
  stopifnot(is_string(url))
  check_for_git()

  current <- tryCatch(
    gitcreds_get(url, use_cache = FALSE, set_cache = FALSE),
    gitcreds_no_credentials = function(e) NULL
  )

  if (is.null(current)) {
    return(invisible(FALSE))
  }

  if (!ack(url, current, "Delete")) {
    throw(new_error("gitcreds_abort_delete_error"))
  }

  msg("-> Removing current credentials...")
  gitcreds_reject(current)

  msg("-> Removing credetials from cache...")
  gitcreds_delete_cache(gitcreds_cache_envvar(url))

  msg("-> Done.")

  invisible(TRUE)
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

#' Environment variable to cache the password for a URL
#'
#' `gitcreds_get()` caches credentials in environment variables.
#' `gitcreds_cache_envvar()` calculates the environment variaable name
#' that is used as the cache, for a URL.
#'
#' @param url Character vector of URLs, they may contain user names
#'   and paths as well. See details below.
#' @return Character vector of environment variables.
#'
#' @seealso [gitcreds_get()].
#'
#' @export
#' @examples
#' gitcreds_cache_envvar("https://github.com")
#' gitcreds_cache_envvar("https://api.github.com/path/to/endpoint")
#' gitcreds_cache_envvar("https://jane@github.com")
#' gitcreds_cache_envvar("https://another.site.github.com")

gitcreds_cache_envvar <- function(url) {
  pcs <- parse_url(url)

  proto <- sub("^https?_$", "", paste0(pcs$protocol, "_"))
  user <- ifelse(pcs$username != "", paste0(pcs$username, "_AT_"), "")
  host0 <- sub("^api[.]github[.]com$", "github.com", pcs$host)
  host1 <- gsub("[.:]+", "_", host0)
  host <- gsub("[^a-zA-Z0-9_-]", "x", host1)

  slug1 <- paste0(proto, user, host)

  # fix the user name ambiguity, not that it happens often...
  slug2 <- ifelse(grepl("^AT_", slug1), paste0("AT_", slug1), slug1)

  # env vars cannot start with a number
  slug3 <- ifelse(grepl("^[0-9]", slug2), paste0("AT_", slug2), slug2)

  paste0("GITHUB_PAT_", toupper(slug3))
}

gitcreds_get_cache <- function(ev) {
  val <- Sys.getenv(ev, NA_character_)
  if (is.na(val) && ev == "GITHUB_PAT_GITHUB_COM") {
    val <- Sys.getenv("GITHUB_PAT", NA_character_)
  }
  if (is.na(val) && ev == "GITHUB_PAT_GITHUB_COM") {
    val <- Sys.getenv("GITHUB_TOKEN", NA_character_)
  }
  if (is.na(val) || val == "") {
    return(NULL)
  }

  unesc <- function(x) {
    gsub("\\\\(.)", "\\1", x)
  }

  # split on `:` that is not preceded by a `\`
  spval <- strsplit(val, "(?<!\\\\):", perl = TRUE)[[1]]
  spval0 <- unesc(spval)

  # Single field, then the token
  if (length(spval) == 1) {
    return(new_gitcreds(
      protocol = NA_character_,
      host = NA_character_,
      username = NA_character_,
      password = unesc(val)
    ))
  }

  # Two fields? Then it is username:password
  if (length(spval) == 2) {
    return(new_gitcreds(
      protocol = NA_character_,
      host = NA_character_,
      username = spval0[1],
      password = spval0[2]
    ))
  }

  # Otherwise a full record
  if (length(spval) %% 2 == 1) {
    warning("Invalid gitcreds credentials in env var `", ev, "`. ",
            "Maybe an unescaped ':' character?")
    return(NULL)
  }

  creds <- structure(
    spval0[seq(2, length(spval0), by = 2)],
    names = spval[seq(1, length(spval0), by = 2)]
  )
  do.call("new_gitcreds", as.list(creds))
}

gitcreds_set_cache <- function(ev, creds) {
  esc <- function(x) gsub(":", "\\:", x, fixed = TRUE)
  keys <- esc(names(creds))
  vals <- esc(unlist(creds, use.names = FALSE))
  value <- paste0(keys, ":", vals, collapse = ":")
  do.call("set_env", list(structure(value, names = ev)))
  invisible(NULL)
}

gitcreds_delete_cache <- function(ev) {
  Sys.unsetenv(ev)
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

#' Access the low level credential API
#'
#' These function are primarily for package authors, who want more
#' control over the user interface, so they want to avoid calling
#' [gitcreds_get()] and [gitcreds_set()] directly.
#'
#' `gitcreds_fill()` calls `git credential fill` to query git
#' credentials.
#'
#' @param input Named list to pass to `git credential fill`.
#' @param args Extra args, used _before_ `fill`, to allow
#' `git -c ... fill`.
#' @param dummy Whether to append a dummy credential helper to the
#' list of credential helpers.
#' @return The standard output of the `git` command, line by line.
#'
#' @seealso [gitcreds_parse_output()] to parse the output of
#' `gitcreds_fill()`.
#'
#' @rdname gitcreds-api
#' @export

gitcreds_fill <- function(input, args = character(), dummy = TRUE) {
  if (dummy) {
    helper <- paste0(
      "credential.helper=\"! echo protocol=dummy;",
      "echo host=dummy;",
      "echo username=dummy;",
      "echo password=dummy\""
    )
    args <- c(args, "-c", helper)
  }

  gitcreds_run("fill", input, args)
}

#' @details `gitcreds_approve()` calls `git credential approve`
#' to add new credentials.
#'
#' @param creds `gitcreds` object (named list) to add or remove.
#'
#' @rdname gitcreds-api
#' @export

gitcreds_approve <- function(creds, args = character()) {
  gitcreds_run("approve", creds, args)
}

#' `gitcreds_reject()` calls `git credential reject` to remove
#' credentials.
#'
#' @rdname gitcreds-api
#' @export

gitcreds_reject <- function(creds, args = character()) {
  gitcreds_run("reject", creds, args)
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
#' @export

gitcreds_parse_output <- function(txt, url) {
  if (is.null(txt) || txt[1] == "protocol=dummy") {
    throw(new_error("gitcreds_no_credentials", url = url))
  }
  nms <- sub("=.*$", "", txt)
  vls <- sub("^[^=]+=", "", txt)
  structure(as.list(vls), names = nms, class = "gitcreds")
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

#' Request confirmation from the user, to replace or delete credentials
#'
#' This function only works in interactive sessions.
#'
#' @param url URL to delete or set new credentials for.
#' @param current The current credentials.
#' @return `FALSE` is the user changed their mind, to keep the current
#' credentials. `TRUE` for replacing/deleting them.
#'
#' @seealso [gitcreds_set()].
#' @noRd

ack <- function(url, current, what = "Replace") {
  msg("\n-> Your current credentials for ", squote(url), ":\n")
  msg(paste0(format(current, header = FALSE), collapse = "\n"), "\n")

  choices <- c(
    "Keep these credentials",
    paste(what, "these credentials"),
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
#' These user names are typical for some git tools, e.g.
#' [Git Credential Manager for Windows](http://microsoft.github.io/Git-Credential-Manager-for-Windows/)
#' (`manager`) and
#' [Git Credential Manager Core](https://github.com/Microsoft/Git-Credential-Manager-Core)
#' (`manager-core`).
#'
#' @noRd

default_username <- function() {
  "PersonalAccessToken"
}

new_gitcreds <- function(...) {
  structure(list(...), class = "gitcreds")
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
    gitcreds_abort_delete_error = "User aborted deleting credentials",
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
  current <- Sys.getenv(names(envs), NA_character_, names = TRUE)
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
    "^(?<protocol>[a-zA-Z0-9]+)://",
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

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
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
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    base::interactive()
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
