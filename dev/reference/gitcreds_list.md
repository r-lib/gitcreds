# List all credentials stored by a git credential helper

This function is meant to be used interactively, to help you when
configuring credential helpers. It is especially useful if you have
multiple accounts on a host.

## Usage

``` r
gitcreds_list(
  url = "https://github.com",
  credential_helper = NULL,
  protocol = NULL
)
```

## Arguments

- url:

  URL to list credentials for. If `NULL` then the credentials are listed
  for all URLs. Note that for a host the results might be different if
  you specify or omit this argument. `gitcreds_list()` uses heuristics
  when the `url` is not specified. If is always best to specify the URL.

- credential_helper:

  Credential helper to use. If this is `NULL`, then the configured
  credential helper is used. If multiple credential helpers are
  configured, then the first one is used, with a warning.

- protocol:

  Protocol to list credentials for. If `NULL` and `url` includes a
  protocol then that is used. Otherwise `"https"` is used.

## Value

A list of `oskeyring_macos_item` objects. See
[`oskeyring::macos_item()`](https://r-lib.github.io/oskeyring/reference/macos_keychain.html).

## Details

Note that this function does not use the credential helper itself, so it
does not have to be installed. But it may also give false results, so
interpret the results with caution, and also use the tool provided by
your OS, to look at the credentials: 'Keychain Access' on macOS and
'Credential Manager' on Windows.

Only a small number of credential helpers are supported currently. Here
is a brief description of each.

### `osxkeychain` on macOS

This is the default credential helper on macOS.

It has some peculiarities:

- If you don't specify a username in the URL, then it will return the
  *oldest* credentials that match the specified host name, with an
  arbitrary user name.

- If the user name is specified in the URL, then it is used to look up
  the credentials.

To change or delete the listed credentials, see the oskeyring package or
the 'Keychain Access' macOS app.

### `manager`, on Windows

This is Git Credential Manager for Windows, see
https://github.com/microsoft/Git-Credential-Manager-for-Windows

It is currently the default helper on Windows, included in the git
installer.

It has some oddities, especially with multiple GitHub users:

- The `github` authority (which is used by default for `github.com`
  URLs) cannot handle multiple users. It always sets the `target_name`
  of the Windows credential to `git:<URL>` where `<URL>` does not
  contain the user name. Since `target_name` is a primary key, it is not
  possible to add multiple GitHub users with the default configuration.

- To support multiple users, switch to the `Basic` authority, e.g. by
  setting the `GCM_AUTHORITY` env var to `Basic`. Then the user name
  will be included in `target_name`, and everything works fine.

- For this helper `gitcreds_list()` lists all records with a matching
  host name.

### `manager-core` on Windows

This is Git Credential Manager Core, see
https://github.com/microsoft/Git-Credential-Manager-Core

On Windows it behaves almost the same way as `manager`, with some
differences:

- Instead of *authorities*, it has providers. `github.com` URLs use the
  `github` provider by default. For better support for multiple GitHub
  accounts, switch to the `generic` provider by setting the
  `GCM_PROVIDER` env var to `generic`.

- `gitcreds_list()` will list all credentials with a matching host,
  irrespectively of the user name in the input URL.

### `manager-core`, *before* version 2.0.246-beta, on macOS

This is Git Credential Manager Core, see
https://github.com/microsoft/Git-Credential-Manager-Core

This helper has some peculiarities w.r.t. user names:

- If the "github" provider is used (which is the default for
  `github.com` URLs), then it completely ignores user names, even if
  they are explicitly specified in the query.

- For other providers, the user name (if specified) is saved in the
  Keychain item.

- For this helper, `gitcreds_list()` always lists all records that match
  the *host*, even if the user name does not match, because it is
  impossible to tell if the user name would be used in a proper git
  credential lookup.

To change or delete the listed credentials, see the oskeyring package or
the 'Keychain Access' macOS app.

### `manager-core`, version 2.0.246-beta or newer, on macOS

This is a newer version of Git Credential Manager Core, that supports
multiple users better:

- if a user name is provided, then it saves it in the credential store,
  and it uses this user name for looking up credentials, even for the
  `github` provider.

- `gitcreds_list()` always lists all records that match the host, even
  if the user name does not match.

- Credentials that were created by an older version of `manager-core`,
  with the `generic` provider, do not work with the newer version of
  `manager-core`, because the format of the Keychain item is different.
