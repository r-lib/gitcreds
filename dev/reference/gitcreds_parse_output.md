# Parse standard output from `git credential fill`

Parse standard output from `git credential fill`

## Usage

``` r
gitcreds_parse_output(txt, url)
```

## Arguments

- txt:

  Character vector, standard output lines from `git credential fill`.

- url:

  URL we queried, to be able to create a better error message.

## Value

`gitcreds` object.

## Details

For dummy credentials (i.e. the lack of credentials), it throws an error
of class `gitcreds_no_credentials`.
