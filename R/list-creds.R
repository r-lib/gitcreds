
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
#' ## `manager-core` on macOS
#'
#' This is Git Credential Manager Core, see
#' https://github.com/microsoft/Git-Credential-Manager-Core
#'
#' This helper has some peculiarities w.r.t. user names:
#' * If the "github" provider is used, then it completely ignores
#'   user names, even if they are explicitly specified in the query.
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

gitcreds_list <- function(url = "https://github.com",
                          credential_helper = NULL,
                          protocol = NULL) {

  stopifnot(
    is.null(credential_helper) || is_string(credential_helper),
    is.null(url) || is_string(url),
    is.null(protocol) || is_string(protocol)
  )

  credential_helper <- credential_helper %||% gitcreds_list_helpers()
  if (length(credential_helper) == 0) {
    throw(new_error("gitcreds_no_helper"))
  }
  if (length(credential_helper) > 1) {
    throw(new_warning("gitcreds_multiple_helpers"))
    credential_helpers <- credential_helper[[1]]
  }

  switch(
    credential_helper,
    "osxkeychain" = gitcreds_list_osxkeychain(url, protocol),
    "manager-core" = gitcreds_list_manager_core(url, protocol),
    throw(new_error(
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

gitcreds_list_osxkeychain <- function(url = NULL, protocol = NULL) {
  if (!requireNamespace("oskeyring", quietly=TRUE)) {
    stop("Listing `osxkeychain` credentials needs the `oskeyring` package")
  }

  attr <- list()
  if (!is.null(url)) {
    purl <- parse_url(url)
    attr$server <- purl$host
    attr$label <- purl$host
    if (!is.na(purl$protocol)) protocol <- purl$protocol
  }

  protocol <- protocol %||% "https"
  attr$protocol <- protocol

  its <- oskeyring::macos_item_search(
    "internet_password",
    attributes = attr
  )

  if (is.null(url)) {
    its <- Filter(is_osxkeychain_item, its)
  }

  its
}

is_osxkeychain_item <- function(it) {
  !is.null(it$attributes$label) &&
    !is.null(it$attributes$server) &&
    it$attributes$server == it$attributes$label &&
    is.null(it$attributes$security_domain)
}

# TODO: this is now macos only

gitcreds_list_manager_core <- function(url = NULL, protocol = NULL) {
  if (!requireNamespace("oskeyring", quietly=TRUE)) {
    stop("Listing `manager-core` credentials needs the `oskeyring` package")
  }

  # We can't filter, need to list all of them, because the 'service'
  # might include the user name, if credential.provider is "github"
  # (or "auto" and the host is github.com).
  its <- oskeyring::macos_item_search("generic_password")

  host <- NULL
  if (!is.null(url)) {
    purl <- parse_url(url)
    if (!is.na(purl$host)) host <- purl$host
    if (!is.na(purl$protocol)) protocol <- purl$protocol
    protocol <- protocol %||% "https"
  }

  its <- Filter(function(it) is_manager_core_item(it, protocol, host), its)

  its
}

is_manager_core_item <- function(it, protocol, host) {
  if (is.null(it$attributes$service)) return(FALSE)
  if (!grepl("^git:", it$attributes$service)) return(FALSE)
  if (is.null(host)) return(TRUE)
  iturl <- sub("^git:", "", it$attributes$service)
  piturl <- parse_url(iturl)
  !is.na(piturl$host) && piturl$host == host &&
    !is.na(piturl$protocol) && piturl$protocol == protocol
}
