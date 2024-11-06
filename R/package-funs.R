# modified from devtools:::load_pkg_description
load_pkg <- function(path = ".") {
  path_desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(path_desc)) {
    stop("No description at ", path_desc, call. = FALSE)
  }
  info <- read.dcf(path_desc)[1, ]
  Encoding(info) <- "UTF-8"
  desc <- as.list(info)
  names(desc) <- tolower(names(desc))
  desc$path <- path
  structure(desc, class = "pkg")
}
#' @keywords internal
as.pkg <- function(x) UseMethod("as.pkg")
#' @keywords internal
as.pkg.pkg <- function(x) x
#' @keywords internal
as.pkg.character <- function(x) load_pkg(x)
