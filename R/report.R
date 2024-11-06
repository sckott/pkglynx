prep_report <- function(x, all_urls, urls) {
  stats <- vapply(x, "[[", numeric(1), "status_code")
  df <- data.frame(url = urls, code = stats)
  bad <- df[df$code >= 400 | df$code < 200, ]
  mz <- stats::setNames(lapply(bad$url, function(z) {
    names(all_urls)[vapply(all_urls, function(w) z %in% w, logical(1))]
  }), bad$url)
  mz <- stats::setNames(utils::stack(mz), c("file", "url"))
  merge(bad, mz, by = "url")
}

lynx_report <- function(x, all_urls, urls) {
  rep <- prep_report(x, all_urls, urls)
  cli::cat_rule(" lynx report", line = 2, line_col = "blue", width = 30)
  if (NROW(rep) == 0) {
    cli::cat_line(
      crayon::style(cli::symbol$tick, "blue"),
      "  No bad URLs found"
    )
  } else {
    cli::cat_line(
      crayon::style(cli::symbol$circle_cross, "orange"),
      "  Bad URLs found; check these (status code: URL)"
    )
    bad_byfiles <- split(rep, rep$file)
    for (i in seq_along(bad_byfiles)) {
      cli::cat_line(crayon::style(names(bad_byfiles)[i], "purple"))
      # print(bad_byfiles[[i]][,1:2])
      for (j in seq_len(NROW(bad_byfiles[[i]]))) {
        cli::cat_line(
          paste(bad_byfiles[[i]]$code[j],
            bad_byfiles[[i]]$url[j],
            sep = ": "
          )
        )
      }
      cli::cat_line("\n")
    }
  }
}
