#' List all credentials stored by a git credential helper
#'
#' This function is meant to be used interactively, to help you when
#' configuring credential helpers. It is especially useful if you have
#' multiple accounts on a host.
#'
#' Note that this function does not use the credential helper itself,
#' so it does not have to be installed. But it may also give false
#' results, so interpret the results with caution, and also use the tool
#' provided by your OS, to look at the credentials: 'Keychain Access'
#' on macOS and 'Credential Manager' on Windows.
#'
#' Only a small number of credential helpers are supported currently.
#' Here is a brief description of each.
#'
#' ## `osxkeychain` on macOS
#'
#' This is the default credential helper on macOS.
#'
#' It has some peculiarities:
#' * If you don't specify a username in the URL, then it will return the
#' _oldest_ credentials that match the specified host name, with an
#' arbitrary user name.
#' * If the user name is specified in the URL, then it is used to look up
#' the credentials.
#'
#' To change or delete the listed credentials, see the oskeyring package
#' or the 'Keychain Access' macOS app.
#'
#' ## `manager`, on Windows
#'
#' This is Git Credential Manager for Windows, see
#' https://github.com/microsoft/Git-Credential-Manager-for-Windows
#'
#' It is currently the default helper on Windows, included in the git
#' installer.
#'
#' It has some oddities, especially with multiple GitHub users:
#' * The `github` authority (which is used by default for `github.com`
#'   URLs) cannot handle multiple users. It always sets the `target_name`
#'   of the Windows credential to `git:<URL>` where `<URL>` does not
#'   contain the user name. Since `target_name` is a primary key, it is
#'   not possible to add multiple GitHub users with the default
#'   configuration.
#' * To support multiple users, switch to the `Basic` authority, e.g. by
#'   setting the `GCM_AUTHORITY` env var to `Basic`. Then the user name
#'   will be included in `target_name`, and everything works fine.
#' * For this helper `gitcreds_list()` lists all records with a matching
#'   host name.
#'
#' ## `manager-core` on Windows
#'
#' This is Git Credential Manager Core, see
#' https://github.com/microsoft/Git-Credential-Manager-Core
#'
#' On Windows it behaves almost the same way as `manager`, with some
#' differences:
#' * Instead of _authorities_, it has providers. `github.com` URLs use the
#'   `github` provider by default. For better support for multiple GitHub
#'   accounts, switch to the `generic` provider by setting the
#'   `GCM_PROVIDER` env var to `generic`.
#' * `gitcreds_list()` will list all credentials with a matching host,
#'   irrespectively of the user name in the input URL.
#'
#' ## `manager-core`, _before_ version 2.0.246-beta, on macOS
#'
#' This is Git Credential Manager Core, see
#' https://github.com/microsoft/Git-Credential-Manager-Core
#'
#' This helper has some peculiarities w.r.t. user names:
#' * If the "github" provider is used (which is the default for
#'   `github.com` URLs), then it completely ignores user names, even if
#'   they are explicitly specified in the query.
#' * For other providers, the user name (if specified) is saved in the
#'   Keychain item.
#' * For this helper, `gitcreds_list()` always lists all records that
#'   match the _host_, even if the user name does not match, because it
#'   is impossible to tell if the user name would be used in a proper
#'   git credential lookup.
#'
#' To change or delete the listed credentials, see the oskeyring package
#' or the 'Keychain Access' macOS app.
#'
#' ## `manager-core`, version 2.0.246-beta or newer, on macOS
#'
#' This is a newer version of Git Credential Manager Core, that supports
#' multiple users better:
#' * if a user name is provided, then it saves it in the credential store,
#'   and it uses this user name for looking up credentials, even for the
#'   `github` provider.
#' * `gitcreds_list()` always lists all records that match the host, even
#'   if the user name does not match.
#' * Credentials that were created by an older version of `manager-core`,
#'   with the `generic` provider, do not work with the newer version of
#'   `manager-core`, because the format of the Keychain item is different.
#'
#' @param credential_helper Credential helper to use. If this is `NULL`,
#'   then the configured credential helper is used. If multiple credential
#'   helpers are configured, then the first one is used, with a warning.
#' @param url URL to list credentials for. If `NULL` then the credentials
#'   are listed for all URLs. Note that for a host the results might be
#'   different if you specify or omit this argument. `gitcreds_list()`
#'   uses heuristics when the `url` is not specified. If is always best to
#'   specify the URL.
#' @param protocol Protocol to list credentials for. If `NULL` and `url`
#'   includes a protocol then that is used. Otherwise `"https"` is used.
#' @return A list of `oskeyring_macos_item` objects. See
#'   [oskeyring::macos_item()].

#' @export

gitcreds_list <- function(
  url = "https://github.com",
  credential_helper = NULL,
  protocol = NULL
) {
  stopifnot(
    is.null(credential_helper) || gitcreds$is_string(credential_helper),
    is.null(url) || gitcreds$is_string(url),
    is.null(protocol) || gitcreds$is_string(protocol)
  )

  credential_helper <- credential_helper %||% gitcreds_list_helpers()
  if (length(credential_helper) == 0) {
    gitcreds$throw(gitcreds$new_error("gitcreds_no_helper"))
  }
  if (length(credential_helper) > 1) {
    gitcreds$throw(gitcreds$new_warning("gitcreds_multiple_helpers"))
    credential_helper <- credential_helper[[1]]
  }

  host <- NULL
  if (!is.null(url)) {
    purl <- gitcreds$parse_url(url)
    if (!is.na(purl$host)) host <- purl$host
    if (!is.na(purl$protocol)) protocol <- purl$protocol
  }
  protocol <- protocol %||% "https"

  switch(
    credential_helper,
    "osxkeychain" = gitcreds_list_osxkeychain(url, host, protocol),
    "manager" = gitcreds_list_manager(url, host, protocol),
    "manager-core" = gitcreds_list_manager_core(url, host, protocol),
    gitcreds$throw(gitcreds$new_error(
      "gitcreds_unknown_helper",
      credential_helper = credential_helper,
      message = sprintf(
        "Unknown credential helper: `%s`, cannot list credentials",
        credential_helper
      )
    ))
  )
}

#' This is how an item, added by git-credential-osxkeychain looks like:
#' * protocol is always present
#' * server is the same as label, but we ignore this when querying a
#'   specific host because the credential helper will ignore it as well
#' * security_domain is never present, similarly ignored for specific hosts. similar reasons
#' @noRd

gitcreds_list_osxkeychain <- function(url, host, protocol) {
  if (!requireNamespace("oskeyring", quietly = TRUE)) {
    stop("Listing `osxkeychain` credentials needs the `oskeyring` package")
  }

  attr <- list()
  if (!is.null(host)) {
    attr$server <- host
    attr$label <- host
  }

  attr$protocol <- protocol

  its <- oskeyring::macos_item_search(
    "internet_password",
    attributes = attr
  )

  Filter(is_osxkeychain_item, its)
}

is_osxkeychain_item <- function(it) {
  !is.null(it$attributes$label) &&
    !is.null(it$attributes$server) &&
    it$attributes$server == it$attributes$label &&
    is.null(it$attributes$security_domain)
}

gitcreds_list_manager_core <- function(url, host, protocol) {
  os <- gitcreds$get_os()
  if (os == "macos") {
    gitcreds_list_manager_core_macos(url, host, protocol)
  } else if (os == "windows") {
    gitcreds_list_manager_core_win(url, host, protocol)
  } else {
    stop("Unsupported OS for `manager-core`")
  }
}

gitcreds_list_manager_core_macos <- function(url, host, protocol) {
  if (!requireNamespace("oskeyring", quietly = TRUE)) {
    stop("Listing `manager-core` credentials needs the `oskeyring` package")
  }

  # We can't filter, need to list all of them, because the 'service'
  # might include the user name, if credential.provider is "github"
  # (or "auto" and the host is github.com).
  its <- oskeyring::macos_item_search("generic_password")

  its <- Filter(
    function(it) is_manager_core_macos_item(it, protocol, host),
    its
  )

  its
}

is_manager_core_macos_item <- function(it, protocol, host) {
  if (is.null(it$attributes$service)) return(FALSE)
  if (!grepl("^git:", it$attributes$service)) return(FALSE)
  if (is.null(host)) return(TRUE)
  iturl <- sub("^git:", "", it$attributes$service)
  piturl <- gitcreds$parse_url(iturl)
  !is.na(piturl$host) &&
    piturl$host == host &&
    !is.na(piturl$protocol) &&
    piturl$protocol == protocol
}

gitcreds_list_manager_core_win <- function(url, host, protocol) {
  if (!requireNamespace("oskeyring", quietly = TRUE)) {
    stop("Listing `manager-core` credentials needs the `oskeyring` package")
  }

  its <- oskeyring::windows_item_enumerate(filter = "git:*")

  its <- Filter(
    function(it) is_manager_core_win_item(it, protocol, host),
    its
  )

  its
}

is_manager_core_win_item <- function(it, protocol, host) {
  if (it$type != "generic") return(FALSE)
  if (!grepl("^git:", it$target_name)) return(FALSE)
  iturl <- sub("^git:", "", it$target_name)
  if (is.null(host)) return(TRUE)
  piturl <- gitcreds$parse_url(iturl)
  !is.na(piturl$host) &&
    piturl$host == host &&
    !is.na(piturl$protocol) &&
    piturl$protocol == protocol
}

gitcreds_list_manager <- function(url, host, protocol) {
  if (!requireNamespace("oskeyring", quietly = TRUE)) {
    stop("Listing `manager` credentials needs the `oskeyring` package")
  }

  its <- oskeyring::windows_item_enumerate(filter = "git:*")

  its <- Filter(
    function(it) is_manager_item(it, protocol, host),
    its
  )

  its
}

# this is the same, apparently

is_manager_item <- is_manager_core_win_item

`%||%` <- function(l, r) if (is.null(l)) r else l
