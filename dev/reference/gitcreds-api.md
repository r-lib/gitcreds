# Access the low level credential API

These function are primarily for package authors, who want more control
over the user interface, so they want to avoid calling
[`gitcreds_get()`](gitcreds_get.md) and
[`gitcreds_set()`](gitcreds_get.md) directly.

## Usage

``` r
gitcreds_fill(input, args = character(), dummy = TRUE)

gitcreds_approve(creds, args = character())

gitcreds_reject(creds, args = character())
```

## Arguments

- input:

  Named list to pass to `git credential fill`.

- args:

  Extra args, used *before* `fill`, to allow `git -c ... fill`.

- dummy:

  Whether to append a dummy credential helper to the list of credential
  helpers.

- creds:

  `gitcreds` object (named list) to add or remove.

## Value

The standard output of the `git` command, line by line.

## Details

`gitcreds_fill()` calls `git credential fill` to query git credentials.

`gitcreds_approve()` calls `git credential approve` to add new
credentials.

## See also

[`gitcreds_parse_output()`](gitcreds_parse_output.md) to parse the
output of `gitcreds_fill()`.
