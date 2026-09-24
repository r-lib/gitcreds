# no_credentials_message() names the url and the env var

    Code
      cat(gitcreds$no_credentials_message("https://codeberg.org"))
    Output
      Could not find any credentials for 'https://codeberg.org'.
      Run `gitcreds_set("https://codeberg.org")`, or set the `GITCREDS_PAT_CODEBERG_ORG` environment variable.

# no_credentials_message() drops the hint for an unparseable url

    Code
      cat(gitcreds$no_credentials_message("foo.bar"))
    Output
      Could not find any credentials for 'foo.bar'

# the no-credentials error carries the url and the env var

    Code
      cat(conditionMessage(err))
    Output
      Could not find any credentials for 'https://codeberg.org'.
      Run `gitcreds_set("https://codeberg.org")`, or set the `GITCREDS_PAT_CODEBERG_ORG` environment variable.

