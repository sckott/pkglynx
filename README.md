pkglynx
=======

<!-- README.md is generated from README.Rmd. Please edit that file -->



[![R-CMD-check](https://github.com/sckott/pkglynx/actions/workflows/R-check.yml/badge.svg)](https://github.com/sckott/pkglynx/actions/workflows/R-check.yml)


pkglynx: Check Package URLs

__Why?__ CRAN maintainers check urls in your R package on submission, see https://cran.r-project.org/web/packages/URL_checks.html - I don't think submissions should be held up by bad URLs in documentation when there can e.g., be a critical bug fix waiting to be deployed. Other package registries don't do this. I think URL checking docs is not a role CRAN should play. But it's still nice to have working URLs in package documentation. 

`pkglynx` is an R package specific URL checker.

## Installation

```r
pak::pak("sckott/pkglynx")
```

## Use


``` r
library(pkglynx)
lynx("path to some package")
# OR
lynx() # the current working dir
```

or on the command line

```
Rscript -e "pkglynx::lynx()"
```

## Meta

* Please [report any issues or bugs](https://github.com/sckott/pkglynx/issues).
* License: MIT
* Please note that the pkglynx project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
