
# TODO: if the user has GIT_CONFIG set, then this probably breaks,
#   because we make a copy of the GIT_CONFIG file, and amend it,
#   but git will still use the original GIT_CONFIG. It is not clear how
#   to fix this, maybe we should unset GIT_CONFIG temporarily, and
#   document that it is ignored by us.

#' @export

gitcreds_get <- function(url = "https://github.com") {

  tmprepo <- gitcreds_setup()
  if (is.null(tmprepo)) return(NULL)
  on.exit(unlink(tmprepo, recursive = TRUE), add = TRUE)

  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(tmprepo)

  env <- gitcreds_env()
  oenv <- set_env(env)
  on.exit(set_env(oenv), add = TRUE)

  ## Now we are ready to query the credential
  input <- gitcreds_input(url)

  ## TODO: should this just fail?
  tryCatch(
    out <- system2("git", c("credential", "fill"), input = input,
                   stdout = TRUE, stderr = null_file()),
    error = function(e) NULL
  )

  parse_credentials(out)
}

gitcreds_get_async <- function(url = "https://github.com") {
  force(url)

  async <- asNamespace("pkgcache")$async
  run_process <- asNamespace("pkgcache")$run_process

  tmp <- tempfile()
  input <- gitcreds_input(url)
  env <- gitcreds_env()
  oldwd <- getwd()
  tmpwd <- oldwd

  async(gitcreds_setup)()$
    then(function(wd) {
      if (is.null(wd)) return(NULL)
      tmpwd <<- wd
      cat(input, file = tmp)
      run_process("git", c("credential", "fill"), stdin = tmp, env = env,
                  error_on_status = FALSE, windows_hide_window = TRUE,
                  wd = wd)
    })$
    then(function(out) {
      if (!is.null(out) && out$status == 0) {
        parse_credentials(strsplit(out$stdout, "\r?\n")[[1]])
      } else {
        NULL
      }
    })$
    finally(function() {
      unlink(tmpwd, recursive = TRUE)
    })
}

gitcreds_setup <- function() {
  ## TODO: find git if not on the path? It would make sense on Windows.
  if (!check_for_git()) return(NULL)

  # Query the local config file, before we change the working dir
  local_config <- find_local_git_config()

  ## Create a temporary repoditory, so we can have a custom config
  dir.create(tmprepo <- tempfile())
  create_empty_git_repo(tmprepo)
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(tmprepo)

  ## Use local config if there was one
  if (!is.null(local_config)) {
    file.copy(local_config, ".git/config", overwrite = TRUE)
  }

  ## Create dummy credential helper
  create_dummy_helper("helper.sh")

  ## Amend local config
  cat(
    sep = "",
    "[credential]\n",
    "\thelper = \"! ./helper.sh\"\n",
    file = ".git/config",
    append = TRUE
  )

  invisible(tmprepo)
}

gitcreds_input <- function(url) {
  paste0("url=", url, "\n\n")
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
    system2("git", "--version", stdout = TRUE, stderr = null_file())
    TRUE
  }, error = function(e) FALSE)
}

find_local_git_config <- function(path = ".") {
  if (path != ".") {
    oldwd <- setwd(path)
    on.exit(setwd(oldwd), add = TRUE)
  }

  tryCatch({
    conf <- system2(
      "git", c("config", "-l", "--local", "--show-origin"),
      stdout = TRUE, stderr = null_file()
    )
    con <- textConnection(conf)
    on.exit(close(con), add = TRUE)
    lines <- readLines(con, warn = FALSE)
    if (length(lines) == 0) return(NULL)
    line1 <- lines[1]
    file <- strsplit(line1, "\t", fixed = TRUE)[[1]][1]
    if (!grepl("^file:", file)) return(NULL)
    normalizePath(sub("^file:", "", file))
  }, error = function() NULL)
}

null_file <- function() {
  if (.Platform$OS.type == "windows") "nul:" else "/dev/null"
}

create_empty_git_repo <- function(path = ".") {
  if (path != ".") {
    oldwd <- setwd(path)
    on.exit(setwd(oldwd), add = TRUE)
  }

  if (file.exists(".git")) {
    stop(sprintf("`%s` already has a git repo", path))
  }

  dir.create(".git")
  dir.create(".git/objects")
  dir.create(".git/objects/pack")
  dir.create(".git/objects/info")
  dir.create(".git/info")
  dir.create(".git/hooks")
  dir.create(".git/refs")
  dir.create(".git/refs/heads")
  dir.create(".git/refs/tags")

  cat(sep = "",
      "[core]\n",
      "\trepositoryformatversion = 0\n",
      "\tfilemode = true\n",
      "\tbare = false\n",
      "\tlogallrefupdates = true\n",
      "\tignorecase = true\n",
      "\tprecomposeunicode = true\n",
      file = ".git/config")
  cat("ref: refs/heads/master\n", sep = "", file = ".git/HEAD")
  cat("", sep = "", file = ".git/info/exclude")
  cat(sep = "", "Unnamed repository; edit this file 'description' ",
      "to name the repository.\n", file = ".git/description")
  cat("", sep = "", file = ".git/FETCH_HEAD")

  invisible(path)
}

create_dummy_helper <- function(path) {
  cat(
    sep = "",
    "#! /bin/sh\n",
    "\n",
    "echo protocol=dummy\n",
    "echo host=dummy\n",
    "echo username=dummy\n",
    "echo password=dummy\n",
    file = path
  )

  if (.Platform$OS.type == "unix") Sys.chmod(path, "0777")

  invisible(path)
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
