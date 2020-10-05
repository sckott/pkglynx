strextract <- function(str, pattern) regmatches(str, gregexpr(pattern, str))
`%||%` <- function(x, y) if (is.null(x)) y else x
has_http_ftp <- function(x) grep("https?://|ftps?://", x, value = TRUE)
