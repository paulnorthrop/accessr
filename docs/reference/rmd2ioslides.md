# Converts R markdown code to ioslides html presentations

Creates accessible html ioslides presentations using R markdown's
[`ioslides_presentation`](https://pkgs.rstudio.com/rmarkdown/reference/ioslides_presentation.html)
argument to
[`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
Zip archives of the html files may be created.

## Usage

``` r
rmd2ioslides(
  x,
  zip = if (length(x) == 1 & !add) FALSE else TRUE,
  pdf = FALSE,
  zip_pdf = zip,
  pdf_args = list(),
  add = FALSE,
  quiet = TRUE,
  rm_html = FALSE,
  rm_pdf = FALSE,
  inc_rmd = FALSE,
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

- zip:

  A logical scalar or character vector indicating whether html files
  should be put into a zip archive. If `zip = FALSE` then no zip archive
  is created. Otherwise, an archive is created in each unique directory
  involved in `x`. If `zip = TRUE` then any archive created has the name
  `accessr_ioslides.zip`. If `zip` is a character vector of zip file
  names (no extension) then these names are used to name the zip
  archives. The names are recycled to the length of the number of unique
  directories, if necessary.

- pdf:

  A logical scalar. If `pdf = TRUE` then each html file is printed to a
  PDF file using
  [`chrome_print`](https://rdrr.io/pkg/pagedown/man/chrome_print.html).
  Google Chrome (or an alternative browser specified in `pdf_args` by
  the `browser` argument to
  [`chrome_print`](https://rdrr.io/pkg/pagedown/man/chrome_print.html)
  must be installed prior to use of this option. An error message like
  `Error in servr::random_port(NULL) : Cannot find an available TCP port`
  means that the `random_port` function in the `servr` package could not
  find an internet connection that Chrome considers secure. Perhaps you
  are using a coffee shop's wifi.

- zip_pdf:

  As `zip`, but relates to the creation of zip archives for any PDF
  files created. If `zip_pdf = TRUE` then each archive is named
  `accessr_ioslides_pdf.zip`.

- pdf_args:

  A list of arguments passed to
  [`chrome_print`](https://rdrr.io/pkg/pagedown/man/chrome_print.html).
  `input` cannot be passed because it is set inside `rmd2html`.

- add:

  A logical scalar that determines what happens if the output zip file
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

- inc_rmd:

  A logical scalar. If `inc_rmd = TRUE` then the source Rmd files are
  included in the zip file created. Otherwise, they are not included.

- params:

  A list of named parameters to pass as the argument `params` to
  [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

- ...:

  Additional arguments passed to
  [`ioslides_presentation`](https://pkgs.rstudio.com/rmarkdown/reference/ioslides_presentation.html).
  Pass (the default) `slide_level = 1` if you want a level one header \#
  to create a new non-segue slide.

  If `css = "black"` is passed then `accessr`'s css file `black.css` is
  used, which results in black text being used in the slides.

  This function is **not** vectorised with respect to arguments in
  `...`.

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
directory, in which case `rmd2ioslides()` will create ioslides
presentations from all these Rmd files, but the `.Rmd` files may be in
different directories.

The [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
function, with the argument `output_file =`
[`ioslides_presentation`](https://pkgs.rstudio.com/rmarkdown/reference/ioslides_presentation.html)
creates the ioslides html files.

The function
[`ioslides_presentation`](https://pkgs.rstudio.com/rmarkdown/reference/ioslides_presentation.html)
has an argument `slide_level` that sets the header level used as a slide
separator. The Lua filter `ioslides_presentation.lua` in the
[`rmarkdown`](https://pkgs.rstudio.com/rmarkdown/reference/rmarkdown-package.html)
package uses any content between headers of level `slide_level` to
create a segue slide, which has a grey background and is intended only
to contain a section heading.

In particular, under the default, `slide_level = 2`, content between a
level one header \# and the next level two header \## is formatted as a
separate grey segue slide. If we do not want segue slides then we must
avoid using level one headers. However, for reasons of document
accessibility, we may want to use level one headers to separate slides.
For example, if we wish to create an ioslides presentation and a Word
document from the same source Rmd file then the Word document will only
meet fully accessibility requirements if the headings in the document
start at level one.

In rmarkdown version 2.26, passing `slide_level = 1` to
[`ioslides_presentation`](https://pkgs.rstudio.com/rmarkdown/reference/ioslides_presentation.html)
does **not** force a new non-segue slide when a level one header \# is
used: it places all content between \# and the next \## on a grey segue
slide and the behaviour content of the resulting slides is not as
intended. Passing `slide_level = 1` to `rmd2ioslides()` replaces
`rmarkdown`'s Lua filter `ioslides_presentation.lua` with one that has
been modified, so that passing `slide_level = 1` **will** start a new
non-segue slide. A modified `default.css` css file is also used that
adjusts the font sizes for the header levels accordingly, so that the
level one header has a larger font than the level two header. For values
of `slide_level` greater than or equal to 2,
[`ioslides_presentation`](https://pkgs.rstudio.com/rmarkdown/reference/ioslides_presentation.html)
will behave as usual.

If `slide_level` is not supplied then `slide_level = 1` is used.

## See also

[`rmd2many`](https://github.com/paulnorthrop/accessr/reference/rmd2many.md),
[`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md),
[`rmd2slidy`](https://github.com/paulnorthrop/accessr/reference/rmd2slidy.md),
[`rmd2html`](https://github.com/paulnorthrop/accessr/reference/rmd2html.md)
for other output formats.

The [accessr package page on
Github](https://paulnorthrop.github.io/accessr/).

## Examples

``` r
# Create an ioslides presentation from example.Rmd
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
  rmd2ioslides(ex_file)
}
```
