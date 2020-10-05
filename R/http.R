get_async <- function(urls, ...) {
  x <- crul::Async$new(urls = urls, opts = list(followlocation=TRUE, ...))
  res <- x$head()
  return(res)
}
