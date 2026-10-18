# Converts R markdown code to Word and PDF documents

Creates Word documents from input R markdown documents. On a Windows
Operating System, accessible PDF documents may be created from these
Word files if the software
[OfficeToPDF](https://github.com/cognidox/OfficeToPDF) is installed. The
convenience function
[`install_otp`](https://github.com/paulnorthrop/accessr/reference/install_otp.md)
can be used to install this software. Zip archives of the Word and/or
PDF files may be created.

## Usage

``` r
rmd2word(
  x,
  doc = "accessr",
  pdf = isTRUE(.Platform$OS.type == "windows"),
  pdf_args = c("/bookmarks"),
  dir,
  zip = if (length(x) == 1 & !add) FALSE else TRUE,
  add = FALSE,
  quiet = TRUE,
  rm_word = FALSE,
  rm_pdf = FALSE,
  inc_word = FALSE,
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

- doc:

  An optional character vector (**including the file extension**) to
  specify template Word documents on which to base the style of the
  respective output Word documents. This determines what is passed as
  the argument `reference_docx` to
  [`word_document`](https://pkgs.rstudio.com/rmarkdown/reference/word_document.html),
  via
  [`officedown::rdocx_document`](https://davidgohel.github.io/officedown/reference/rdocx_document.html).
  Different templates may be used for different files.
  `rep_len(doc, length(x))` is used to force `length(doc)` to have the
  same length as `x`. See **Details** for some built-in options.

- pdf:

  A logical scalar. Should `OfficeToPDF.exe` be used to create PDF files
  from the Word documents that are produced? If `pdf = FALSE` then any
  zip archives created contain only Word files. PDF files will only be
  produced if the Operating System is `"windows"`, that is,
  `.Platform$OS.type == "windows"`.

- pdf_args:

  A character vector of arguments to be used when calling
  `OfficeToPDF.exe`. See [Command line
  switches](https://github.com/cognidox/OfficeToPDF?tab=readme-ov-file#command-line-switches)
  at [OfficeToPDF](https://github.com/cognidox/OfficeToPDF) for a list
  of optional arguments. Including `"/bookmarks"` (the default) in
  `pdf_args` will create bookmarks in the PDF using the headings in the
  corresponding Word document, which users may find useful for
  navigation.

- dir:

  A path to the directory in which the file `OfficeToPDF.exe` sits. This
  is not needed if this file sits in the current working directory or a
  directory in the list returned by
  [`searchpaths()`](https://rdrr.io/r/base/search.html). Otherwise, it
  may be a path relative to the current working directory or an absolute
  path. If `dir` is missing then `rmd2word` will look in
  `system.file(package = "accessr")`, which is the default installation
  location of
  [`install_otp`](https://github.com/paulnorthrop/accessr/reference/install_otp.md).

- zip:

  A logical scalar or character vector indicating whether PDF files
  should be put into a zip archive. If `zip = FALSE` then no zip archive
  is created. Otherwise, an archive is created in each unique directory
  involved in `x`. If `zip = TRUE` then any archive created has the name
  `accessr_word.zip`. If `zip` is a character vector of zip file names
  (no extension) then these names are used to name the zip archives. The
  names are recycled to the length of the number of unique directories
  if necessary.

- add:

  A logical scalar that determines what happens if the output zip file
  already exists. If `add = TRUE` then files are added to the zip file
  and if `add = FALSE` then the zip file is deleted and will only
  contain newly-created files.

- quiet:

  A logical scalar. Passed to
  [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
  as the argument `quiet`. The default, `quite = TRUE` suppresses all
  printing during rendering of the document.

- rm_word:

  A logical scalar. If `rm_word = TRUE` then all the Word files created
  are deleted. Otherwise, they are not deleted.

- rm_pdf:

  A logical scalar. If `rm_pdf = TRUE` and a zip archive of PDF files is
  produced then the individual PDF files are deleted. Otherwise, they
  are not deleted.

- inc_word:

  A logical scalar. If `inc_word = TRUE` then the Word files are
  included in the zip file created. Otherwise, they are not included.

- params:

  A list of named parameters to pass as the argument `params` to
  [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

- ...:

  Additional arguments passed to
  [`word_document`](https://pkgs.rstudio.com/rmarkdown/reference/word_document.html).

## Value

In addition to creating the Word and PDF files, and perhaps zip files, a
list containing the following vector components is returned invisibly:

- error_codes :

  If `pdf = TRUE`, numeric values returned from
  [`system`](https://rdrr.io/r/base/system.html). If `wait = FALSE` then
  these values will be 0 (the success value) even if some of the PDF
  files could not be produced. The error code 17234 indicates that a PDF
  file was open in another application.

- files :

  (absolute) paths and file names of all files created.

- zips :

  (relative) paths and names of all zip files created (if `zip = TRUE`).

## Details

Information such as `title`, `author`, `lang` etc in the YAML header in
the Rmd file are used but `output` is ignored.

The simplest setup is to have the `.Rmd` files in the current working
directory, in which case `rmd2word()` will create Word documents from
all these Rmd files, but the `.Rmd` files may be in different
directories.

It is possible to have the `.Rmd` files in different directories, but
any non-`"default"` values in `doc` must be such that the
`reference_docx` argument of
[`word_document`](https://pkgs.rstudio.com/rmarkdown/reference/word_document.html)
finds a template Word file. If the template is in the same directory as
its respective `.Rmd` component in `x` then the filename, e.g.
`"template.docx"` will suffice. Otherwise, a path to the template should
be given, either relative to the directory in which the `.Rmd` file
sits, or an absolute path.

For information on how to create a template Word document see Richard
Layton's guide [Happy collaboration with Rmd to
docx](https://rmarkdown.rstudio.com/articles_docx.html).

There are some built-in template options:

- `doc = "officedown"`: uses
  [`rdocx_document`](https://davidgohel.github.io/officedown/reference/rdocx_document.html)'s
  default,

- `doc = "accessr"`: similar to `"officedown"` but with narrower margins
  and black text for titles and darker hyperlinks, to avoid contrast
  issues,

- `doc = "18"`: like `"accessr"` but with 18pt text,

- `doc = "exam"`: creates a Word file with a header "Examination paper
  for STAT0002" on the left and "Page x of n" on the right.

To use your own template(s), provide their filename(s). A component
equal to `"officedown"` chooses
[`rdocx_document`](https://davidgohel.github.io/officedown/reference/rdocx_document.html)'s
default. A component equal to `"accessr"` chooses `accessr`'s internal
template file, which has narrower margins and darker blue fonts for
titles and hyperlinks, to avoid contrast issues. To use your own
template(s), provide their filenames. See **Details** for more
information.

The [`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
function creates a Word file from each input `.Rmd` file. Then
[OfficeToPDF](https://github.com/cognidox/OfficeToPDF) is used to
convert the Word file to a PDF file. The file `OfficeToPDF.exe` needs to
be downloaded from the [OfficeToPDF
releases](https://github.com/cognidox/OfficeToPDF/releases) page and
placed in the directory specified by the argument `dir`, or in a
directory that is in the list returned by
[`searchpaths`](https://rdrr.io/r/base/search.html). If
`OfficeToPDF.exe` cannot be found then an error is thrown. A warning
will be given if any of the PDF files could not be produced. This will
occur if there is an existing PDF file of the same name open in another
application.

## References

Layton, Richard. (2015) Happy collaboration with Rmd to docx. R Markdown
from RStudio article. <https://rmarkdown.rstudio.com/articles_docx.html>

## See also

[`install_otp`](https://github.com/paulnorthrop/accessr/reference/install_otp.md)
to install [OfficeToPDF](https://github.com/cognidox/OfficeToPDF).

[`rmd2many`](https://github.com/paulnorthrop/accessr/reference/rmd2many.md),
[`rmd2ioslides`](https://github.com/paulnorthrop/accessr/reference/rmd2ioslides.md),
[`rmd2slidy`](https://github.com/paulnorthrop/accessr/reference/rmd2slidy.md),
[`rmd2html`](https://github.com/paulnorthrop/accessr/reference/rmd2html.md)
for other output formats.

The [accessr package page on
Github](https://paulnorthrop.github.io/accessr/).

## Examples

``` r
# Create a Word file from example.Rmd
got_hux <- requireNamespace("huxtable", quietly = TRUE)
got_flex <- requireNamespace("flextable", quietly = TRUE)
got_pandoc <- rmarkdown::pandoc_available("1.14")
got_all <- got_hux && got_flex && got_pandoc
# This example needs packages huxtable and flextable
# We pass pdf = FALSE because OfficeToPDF is needed to convert Word to PDF
# and this is only relevant on a Windows Operating System.
if (got_all) {
  ex_file <- system.file(package = "accessr", "examples", "example.Rmd")
  file.copy(ex_file, tdir <- tempdir(check = TRUE), overwrite = TRUE)
  ex_file <- list.files(tdir, pattern = "example.Rmd", full.names = TRUE)
  ex_file <- sub(".Rmd", "", ex_file)
  rmd2word(ex_file, pdf = FALSE)
}
```
