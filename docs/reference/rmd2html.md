# Converts R markdown code to html documents

Creates accessible html documents using either
[`html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)
(the default) or
[`html_document2`](https://pkgs.rstudio.com/bookdown/reference/html_document2.html)
as the `output_format` argument to
[`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
Zip archives of the html files may be created.

## Usage

``` r
rmd2html(
  x,
  bookdown = FALSE,
  zip = if (length(x) == 1 & !add) FALSE else TRUE,
  pdf = FALSE,
  pdf_args = list(),
  zip_pdf = zip,
  add = FALSE,
  quiet = TRUE,
  rm_html = FALSE,
  rm_pdf = FALSE,
  params = NULL,
  ...
)
```

## Arguments

- x:

  A character vector containing the names (**no extension**) of the
  `.Rmd` files to convert if they are in the current working directory,
  or paths to the files, either absolute or relative to the current
  working directory, e.g., `DIRECTORY/file1`. The `.html` files are
  created in the same directory as their respective `.Rmd` file. If `x`
  is missing then an html file is created from each of the `.Rmd` files
  in the current working directory.

- bookdown:

  A logical scalar. If `bookdown = TRUE` then
  [`html_document2`](https://pkgs.rstudio.com/bookdown/reference/html_document2.html)
  as the `input` argument to
  [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
  Otherwise,
  [`html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)
  is used.

- zip:

  A logical scalar or character vector indicating whether html files
  should be put into a zip archive. If `zip = FALSE` then no zip archive
  is created. Otherwise, an archive is created in each unique directory
  involved in `x`. If `zip = TRUE` each archive of html files is named
  `accessr_html.zip`. If `zip` is a character vector of zip file names
  (no extension) then these names are used to name the zip archives. The
  names are recycled to the length of the number of unique directories,
  if necessary.

- pdf:

  A logical scalar. If `pdf = TRUE` then each html file is printed to a
  PDF file using
  [`chrome_print`](https://rdrr.io/pkg/pagedown/man/chrome_print.html).
  Google Chrome (or an alternative browser specified in `pdf_args` by
  the `browser` argument to
  [`chrome_print`](https://rdrr.io/pkg/pagedown/man/chrome_print.html))
  must be installed prior to use of this option. An error message like
  `Error in servr::random_port(NULL) : Cannot find an available TCP port`
  means that the `random_port` function in the `servr` package could not
  find an internet connection that Chrome considers secure. Perhaps you
  are using a coffee shop's wifi.

- pdf_args:

  A list of arguments passed to
  [`chrome_print`](https://rdrr.io/pkg/pagedown/man/chrome_print.html).
  `input` cannot be passed because it is set inside `rmd2html`.

- zip_pdf:

  As `zip`, but relates to the creation of zip archives for any PDF
  files created. If `zip_pdf = TRUE` then each archive is named
  `accessr_html_pdf.zip`.

- add:

  A logical scalar that determines what happens if an output zip file
  already exists. If `add = TRUE` then files are added to the zip file
  and if `add = FALSE` then the zip file is deleted and will only
  contain newly-created files.

- quiet:

  Argument of the same name passed to
  [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  to determine what is printed during rendering from knitr.

- rm_html:

  A logical scalar. If `rm_html = TRUE` and a zip archive of html files
  is produced then the individual html files are deleted. Otherwise,
  they are not deleted.

- rm_pdf:

  A logical scalar. If `rm_pdf = TRUE` and a zip archive of pdf files is
  produced then the individual pdf files are deleted. Otherwise, they
  are not deleted.

- params:

  A list of named parameters to pass as the argument `params` to
  [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

- ...:

  Additional arguments passed to
  [`html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)
  or
  [`html_document2`](https://pkgs.rstudio.com/bookdown/reference/html_document2.html).

## Value

In addition to creating the html files, and perhaps zip files, a list
containing the following (character vector) components is returned
invisibly:

- files :

  (absolute) paths and file names of the files added to a zip file.

- zips :

  (relative) paths and names of all the zip files.

## Details

Information such as `title`, `author`, `lang` etc in the YAML header in
the Rmd file are used but `output` is ignored.

The simplest setup is to have the `.Rmd` files in the current working
directory, in which case `rmd2html()` will create HTML documents from
all these Rmd files, the `.Rmd` files may be in different directories.

The [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
function, with the argument `output_file =`
[`html_document`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)
creates the html files.

## See also

[`rmd2many`](https://github.com/paulnorthrop/accessr/reference/rmd2many.md),
[`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md),
[`rmd2ioslides`](https://github.com/paulnorthrop/accessr/reference/rmd2ioslides.md),
[`rmd2slidy`](https://github.com/paulnorthrop/accessr/reference/rmd2slidy.md)
for other output formats.

The [accessr package page on
Github](https://paulnorthrop.github.io/accessr/).

## Examples

``` r
# Create an HTML document from example.Rmd
got_hux <- requireNamespace("huxtable", quietly = TRUE)
got_flex <- requireNamespace("flextable", quietly = TRUE)
got_pandoc <- rmarkdown::pandoc_available("1.14")
got_all <- got_hux && got_flex && got_pandoc
# This example needs packages huxtable and flextable
if (got_all) {
  ex_file <- system.file(package = "accessr", "examples", "example.Rmd")
  file.copy(ex_file, tdir <- tempdir(check = TRUE), overwrite = TRUE)
  ex_file <- list.files(tdir, pattern = "example.Rmd", full.names = TRUE)
  ex_file <- sub(".Rmd", "", ex_file)
  rmd2html(ex_file)
}
```
