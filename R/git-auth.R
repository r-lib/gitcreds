
# TODO: if the user has GIT_CONFIG set, then this probably breaks,
#   because we make a copy of the GIT_CONFIG file, and amend it,
#   but git will still use the original GIT_CONFIG. It is not clear how
#   to fix this, maybe we should unset GIT_CONFIG temporarily, and
#   document that it is ignored by us.

#' @export

gitcreds_get <- function(url = "https://github.com") {

  if (!check_for_git()) stop("You need to install git")

  env <- gitcreds_env()
  oenv <- set_env(env)
  on.exit(set_env(oenv), add = TRUE)

  ## Now we are ready to query the credential
  input <- gitcreds_input(url)

  helper <- paste0(
    "credential.helper=\"! echo protocol=dummy;",
    "echo host=dummy;",
    "echo username=dummy;",
    "echo password=dummy\""
  )

  ## TODO: should this just fail?
  tryCatch(
    out <- suppressWarnings(system2(
      "git", c("-c", helper, "credential", "fill"),
      input = input, stdout = TRUE, stderr = null_file()
    )),
    error = function(e) NULL
  )

  ## TODO should this just fail?
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    out <- NULL
  }

  parse_credentials(out)
}

gitcreds_input <- function(url, ...) {
  args <- list(url = url, ...)
  paste0(
    paste0(names(args), "=", args, collapse = "\n"),
    "\n\n"
  )
}

gitcreds_env <- function() {
  ## Avoid interactivity with some common credential helpers
  c(
    GCM_INTERACTIVE = "Never",
    GCM_MODAL_PROMPT = "false",
    GCM_VALIDATE = "false"
  )
}

#' @export

gitcreds_list_helpers <- function() {
  if (!check_for_git()) stop("You need to install git")
  out <- system2("git", c("config", "--get-all", "credential.helper"),
                 stdout = TRUE, stderr = null_file())
  out
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

null_file <- function() {
  if (.Platform$OS.type == "windows") "nul:" else "/dev/null"
}

parse_credentials <- function(txt) {
  if (is.null(txt)) return(NULL)
  if (txt[1] == "protocol=dummy") return(NULL)
  nms <- sub("=.*$", "", txt)
  vls <- sub("^[^=]+=", "", txt)
  structure(as.list(vls), names = nms)
}

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
