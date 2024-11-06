#' check links in a package
#' @export
#' @param pkg path to a package
#' @param files (character) vector of files to look in. default:
#' DESCRIPTION, NEWS.md, README.md
#' @details
#' - markdown files are converted to html first with
#' `markdown::markdownToHTML` to make it easier to extract URLs via
#' xpath with the \pkg{xml2} package
#' - for non-markdown files we use regex to extract URLs
#' @examples \dontrun{
#' # within a package
#' # lynx(pkg="~/github/ropensci/rcrossref")
#' # lynx(pkg="~/github/ropensci/rnoaa")
#' }
lynx <- function(
    pkg = ".",
    files = c("DESCRIPTION", "NEWS.md", "README.md")) {
  x <- load_pkg(pkg)
  doc_urls <- gather_doc_urls(x)
  fpaths <- path.expand(file.path(x$path, files))
  other_urls <- stats::setNames(lapply(files, gather_urls, pkg = x), fpaths)
  all_urls <- Filter(length, c(doc_urls, other_urls))
  all_urls <- lapply(all_urls, has_http_ftp)
  uniq_urls <- unique(unlist(all_urls))
  res <- get_async(uniq_urls)
  # x=res; all_urls=all_urls; urls=uniq_urls
  lynx_report(x = res, all_urls = all_urls, urls = uniq_urls)
}
