
#' @export

print.gitcreds <- function(x, header = TRUE, ...) {
  cat(format(x, header = header, ...), sep = "\n")
  invisible(x)
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
