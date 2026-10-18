# Create content in multiple formats

From a single R markdown file create HTML slides, PDF slides, Word and
PDF documents.

## Usage

``` r
rmd2many(
  x,
  outputs = c("ioslides", "word"),
  slide_level = 1,
  css = "black",
  add18 = TRUE,
  pdf = TRUE,
  highlight = list(word = "monochrome", ioslides = NULL, slidy = NULL, html = NULL),
  params = NULL,
  zip = TRUE,
  ...
)
```

## Arguments

- x:

  A character vector containing the names (**no extension**) of the
  `.Rmd` files to convert if they are in the current working directory,
  or paths to the files, either absolute or relative to the current
  working directory, e.g., `DIRECTORY/file1`. The output files are
  created in the same directory as their respective `.Rmd` file.

- outputs:

  A character vector. Specifies the output formats required. A subset of
  `c("word", "ioslides", "slidy", "html")`. If more than one of
  `"ioslides"`, `"slidy"` and `"html"` are present then only one of
  these is used with the order of preference `"ioslides"`, `"slidy"`
  then `"html"`.

- slide_level:

  Passed to
  [`rmd2ioslides`](https://github.com/paulnorthrop/accessr/reference/rmd2ioslides.md)
  via `...`. The default `slide_level = 1` means that a level one header
  \# create a new non-segue slide for an ioslides presentation.

- css:

  The argument `css` passed to
  [`ioslides_presentation`](https://pkgs.rstudio.com/rmarkdown/reference/ioslides_presentation.html)
  or
  [`slidy_presentation`](https://pkgs.rstudio.com/rmarkdown/reference/slidy_presentation.html).
  If `css = "black"` then `accessr`'s css file `black.css` is used,
  which results in black text being used in the slides. `css` is not
  used if `outputs = html`.

- add18:

  A logical scalar. If `TRUE` then we also create Word documents with
  18pt text.

- pdf:

  A logical scalar. If `TRUE` then we use
  [`chrome_print`](https://rdrr.io/pkg/pagedown/man/chrome_print.html)
  to print PDF versions of HTML files produced using the output
  `"ioslides"` or `"slidy"`. and/or `OfficeToPDF.exe` to create PDF
  files from any Word documents that are produced.

- highlight:

  A named list, with names a subset of `c("word", "ioslides", "slidy")`,
  providing the respective syntax highlighting styles passed to Pandoc
  for the output formats. Any syntax highlighting provided in `css` will
  take precedence. `highlight` is not used if `outputs = html`.

- params:

  A list of named parameters to pass as the argument `params` to
  [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
  In the example below, the file `example.Rmd` has a parameter `hide`.
  If `hide = TRUE` then parts of the output are hidden using the `knitr`
  chunk options `echo` and `eval`.

- zip:

  A logical scalar or character vector indicating whether PDF files
  should be put into a zip archive. If `zip = FALSE` then no zip archive
  is created. Otherwise, an archive is created in each unique directory
  involved in `x`. If `zip = TRUE` then any archive created is named
  after the first filename in `x` from the relevant directory. If `zip`
  is a character vector of zip file names (no extension) then these
  names are used to name the zip archives. The names are recycled to the
  length of the number of unique directories if necessary.

- ...:

  Additional arguments to be passed to
  [`rmd2ioslides`](https://github.com/paulnorthrop/accessr/reference/rmd2ioslides.md),
  [`rmd2slidy`](https://github.com/paulnorthrop/accessr/reference/rmd2slidy.md),
  [`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md)
  or
  [`rmd2html`](https://github.com/paulnorthrop/accessr/reference/rmd2html.md).

## Value

A list containing the following components:

- files :

  names of all the files created.

- zips :

  names of all zip files created (if `zip = TRUE`).

## Details

The default setting creates, for each valid `filename` in `x`, the
following files

- `filename.html`: lecture slides in `ioslides` format.

- `filename_slides.pdf`: a PDF document containing the content in
  `filename.html`.

- `filename.pdf`: a PDF document created from a Word document produced
  by `rmd2word`.

- `filename.docx`: a Word document.

- `filename18pt.docx`: a Word document. If `add18 = TRUE` then a
  template Word document with 18pt bold text is used.

- `filename.zip`: a zip file containing all the files produced.

## See also

[`install_otp`](https://github.com/paulnorthrop/accessr/reference/install_otp.md)
to install [OfficeToPDF](https://github.com/cognidox/OfficeToPDF).

[`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md),
[`rmd2ioslides`](https://github.com/paulnorthrop/accessr/reference/rmd2ioslides.md),
[`rmd2slidy`](https://github.com/paulnorthrop/accessr/reference/rmd2slidy.md),
[`rmd2html`](https://github.com/paulnorthrop/accessr/reference/rmd2html.md).

The [accessr package page on
Github](https://paulnorthrop.github.io/accessr/).

## Examples

``` r
# Create documents from example.Rmd
got_hux <- requireNamespace("huxtable", quietly = TRUE)
got_flex <- requireNamespace("flextable", quietly = TRUE)
got_pandoc <- rmarkdown::pandoc_available("1.14")
got_all <- got_hux && got_flex && got_pandoc
# This example needs packages huxtable and flextable
# We pass pdf = FALSE because OfficeToPDF is needed to convert Word to PDF
# and this is only relevant on a Windows Operating System.
#
if (got_all) {
  ex_file <- system.file(package = "accessr", "examples", "example.Rmd")
  file.copy(ex_file, tdir <- tempdir(check = TRUE), overwrite = TRUE)
  ex_file <- list.files(tdir, pattern = "example.Rmd", full.names = TRUE)
  ex_file <- sub(".Rmd", "", ex_file)
  rmd2many(ex_file, params = list(hide = TRUE), pdf = FALSE, zip = TRUE)
}
#> $files
#> [1] "C:/Users/Paul/AppData/Local/Temp/RtmpCikUyI/example.html"    
#> [2] "C:/Users/Paul/AppData/Local/Temp/RtmpCikUyI/example18pt.docx"
#> [3] "C:/Users/Paul/AppData/Local/Temp/RtmpCikUyI/example.docx"    
#> 
#> $zips
#> [1] "C:/Users/Paul/AppData/Local/Temp/RtmpCikUyI/example.zip"
#> 
```
