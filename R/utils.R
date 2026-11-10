# Internal helpers.

# NULL-default (base R has %||% only from 4.4; this package supports 4.1+).
`%||%` <- function(a, b) if (is.null(a)) b else a
