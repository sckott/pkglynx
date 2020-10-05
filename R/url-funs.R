extract_urls_one <- function(w) {
  m <- strextract(w, "https?://(\\S+)[^\\)^\\}]")
  m <- gsub('\"|\\}|\\{|\\s$|,|\\.$|\\.\\s$|>|\\)$|^\\(', "", unlist(m))
  gsub('\"|\\}|\\{|\\s$|,|\\.$|\\.\\s$|>|\\)$|^\\(', "", m)
}
gather_doc_urls <- function(pkg) {
  man_dir <- file.path(pkg$path, "man")
  if (!dir.exists(man_dir)) stop("'man' directory not found", call.=FALSE)
  ff <- list.files(man_dir, full.names = TRUE, pattern = ".Rd")
  lns <- stats::setNames(lapply(ff, readLines), ff)
  lapply(lns, extract_urls_one)
}
gather_urls <- function(file, pkg) {
  fpath <- file.path(pkg$path, file)
  if (!file.exists(fpath))
    stop(sprintf("'%s' directory not found", file), call.=FALSE)
  ext <- fs::path_ext(fpath)
  if (ext == "md") return(extract_md(fpath))
  lns <- readLines(fpath)
  extract_urls_one(lns)
}
extract_md <- function(path) {  
  tmp <- markdown::markdownToHTML(path)
  html <- xml2::read_html(tmp)
  lks <- xml2::xml_text(xml2::xml_find_all(html, "//a"))
  Filter(nzchar, lks)
}
