# External images for Word output

This is a modified version of the
[`external_img`](https://davidgohel.github.io/officer/reference/external_img.html)
function from the `officer` package for use in
[`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md).
The modification is to allow the use of the
[`knitr`](https://rdrr.io/pkg/knitr/man/knitr-package.html) chunk
options `out.width` and/or `out.height` to set the dimensions of a
figure (R-generated or external image) when the output format is a Word
document. This functionality is not normally available.

## Usage

``` r
ext_img(src, width = 0.5, height = 0.2, alt = "", ref_docx_dim)
```

## Arguments

- src:

  image file path

- width:

  width in inches

- height:

  height in inches

- alt:

  alternative text for images

- ref_docx_dim:

  A list containing the components `page` (a named numeric vector
  containing the `width` and `height`), `landscape` (a logical vector)
  and `margins` (a named numeric vector containing the `top`, `bottom`,
  `left`, `right`, `header` and `footer` margins). See **Details**.

## Value

An object of class `c("external_img", "cot", "run")` with attributes
`"dims"` (a named list containing the figure's `width` and `height`) and
`"alt"` (a character scalar containing the alternative text for the
figure).

## Details

If in the current R code chunk `out.width` or `out.height` have been set
then the corresponding values of `fig.width` and `fig.height` are
inferred from the dimensions of the figure and the page dimensions of
the output Word document. The former are obtained using
[`readPNG`](https://rdrr.io/pkg/png/man/readPNG.html) or
[`readJPEG`](https://rdrr.io/pkg/jpeg/man/readJPEG.html) as appropriate.
If only one of `out.width` or `out.height` are set then the aspect ratio
of the figure is preserved.

`ref_docx_dim` can be produced using
`officer::docx_dim(officer::read_docx(doc))`, where `doc` is an
[`docx_dim`](https://davidgohel.github.io/officer/reference/docx_dim.html)
object and
[`docx_dim`](https://davidgohel.github.io/officer/reference/docx_dim.html)
and
[`read_docx`](https://davidgohel.github.io/officer/reference/read_docx.html)
are functions in the `officer` package.

## Note

This function has been designed for use inside
[`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md)
but the object returned has the same structure as that returned from
[`external_img`](https://davidgohel.github.io/officer/reference/external_img.html).
Outside the context of a `knitr` R code chunk the outputs from
`external_img` and `ext_img` are identical.

## Examples

``` r
# Example from the officer::external_img() documentation
# Wrap R logo with external_img ----
srcfile <- file.path( R.home("doc"), "html", "logo.jpg" )
extimg <- ext_img(src = srcfile, height = 1.06 / 2, width = 1.39 / 2)
```
