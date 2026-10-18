# Changelog

## accessr 1.1.4

### New features

- In
  [`rmd2html()`](https://github.com/paulnorthrop/accessr/reference/rmd2html.md)
  the new argument `bookdown` can be used to render using
  [`bookdown::html_document2()`](https://pkgs.rstudio.com/bookdown/reference/html_document2.html),
  rather than
  [`rmarkdown::html_document()`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html).

### Bug fixes and minor improvements

- In
  [`rmd2word()`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md)
  the call to `accessr::ext_img()` has been removed. The release of
  `officedown 0.4.3` makes this unnecessary and problematic. See
  [Issue](https://github.com/paulnorthrop/accessr/issues/2)
  [\#2](https://github.com/paulnorthrop/accessr/issues/2).
- In
  [`rmd2html()`](https://github.com/paulnorthrop/accessr/reference/rmd2html.md)
  the argument `...` in the call to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  has been removed to avoid errors when passing additional arguments to
  [`bookdown::html_document2()`](https://pkgs.rstudio.com/bookdown/reference/html_document2.html)
  or
  [`rmarkdown::html_document()`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html).

## accessr 1.1.3

CRAN release: 2025-03-30

### Bug fixes and minor improvements

Revert back (effectively to v1.1.1) now that the issue in
officedown::get_fun() has been fixed.

## accessr 1.1.2

CRAN release: 2025-03-15

### Bug fixes and minor improvements

This is a temporary patch to fix the issues at
<https://cran.r-project.org/web/checks/check_results_accessr.html>,
which stem from a call to strsplit() in officedown:::get_fun()

## accessr 1.1.1

CRAN release: 2025-01-15

### New features

- In
  [`rmd2word()`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md)
  the argument `pdf_args` may be used to pass [command line
  switches](https://github.com/cognidox/OfficeToPDF?tab=readme-ov-file#command-line-switches)
  to [OfficeToPDF](https://github.com/cognidox/OfficeToPDF) when
  creating PDF documents. The default setting creates bookmarks in the
  PDF file using the headings in the input Word document.

### Bug fixes and minor improvements

- `ext_img` now also works if the file extension of an input image is
  capitalised, that is, “PNG”, “JPG” or “JPEG”. In version 1.0.1 it only
  worked for lower case file extensions.
- CRAN installation details added to README
- CRAN downloads badges added to README
