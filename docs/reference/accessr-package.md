# accessr: Command Line Tools to Produce Accessible Documents using R markdown

Provides functions to produce accessible HTML and PDF documents from
input R markdown files. Currently, **accessr** only provides the option
to produce accessible PDF files on a Windows Operating System. One
aspect of accessibility is providing a headings structure that is
recognised by a screen reader, providing a navigational tool for a blind
or partially-sighted person. A key aim is to produce documents of
different formats from each of a collection of R markdown source files.
A user-supplied template Word document can be used to determine the
formatting of an output Word document. Similar functions produce HTML
slides and HTML documents. A zip file containing multiple files can be
produced. The option to print HTML output to (non-accessible) PDF files
is also available.

## Details

See the [accessr package page on
Github](https://paulnorthrop.github.io/accessr/) for more information.
An example Rmd file is available at
`system.file(package = "accessr", "examples", "example.Rmd")`.

On a Windows Operating System, Accessible PDF documents are produced by
creating Word documents from R markdown files and then PDF documents
from these Word documents. The first step uses the
[`render`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
function from the [rmarkdown
package](https://cran.r-project.org/package=rmarkdown) and the
[`rdocx_document`](https://davidgohel.github.io/officedown/reference/rdocx_document.html)
function from the officedown package. The second step uses
[OfficeToPDF](https://github.com/cognidox/OfficeToPDF).

The main functions in `accessr` are:

- [`rmd2many`](https://github.com/paulnorthrop/accessr/reference/rmd2many.md):
  create HTML slides, PDF slides, Word and PDF documents from a single R
  markdown file.

- [`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md):
  create Word documents and accessible PDF files.

  [`install_otp`](https://github.com/paulnorthrop/accessr/reference/install_otp.md):
  convenience function to install OfficeToPDF, to create PDF files from
  Word documents in
  [`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md).

- [`rmd2ioslides`](https://github.com/paulnorthrop/accessr/reference/rmd2ioslides.md):
  create ioslides presentations and perhaps print to (non-accessible)
  PDF documents.

- [`rmd2slidy`](https://github.com/paulnorthrop/accessr/reference/rmd2slidy.md):
  create slidy presentations and perhaps print to (non-accessible) PDF
  documents..

- [`rmd2html`](https://github.com/paulnorthrop/accessr/reference/rmd2html.md):
  create html documents and perhaps print to (non-accessible) PDF
  documents.

The `rmd2?` functions provide the option to create a zip archive
containing the output files. All the .Rmd files in a directory can be
processed with one function call. Information such as `title`, `author`,
`lang` etc in the YAML header in the Rmd file are used but `output` is
ignored.

## References

David Gohel and Noam Ross (2021). officedown: Enhanced 'R Markdown'
Format for 'Word' and 'PowerPoint'. R package version 0.3.1.
<https://CRAN.R-project.org/package=officedown>

JJ Allaire, Yihui Xie, Christophe Dervieux, Jonathan McPherson, Javier
Luraschi, Kevin Ushey, Aron Atkins, Hadley Wickham, Joe Cheng, Winston
Chang, and Richard Iannone (2024). rmarkdown: Dynamic Documents for R. R
package version 2.26, <https://rmarkdown.rstudio.com>.

## See also

[`install_otp`](https://github.com/paulnorthrop/accessr/reference/install_otp.md),
[`rmd2many`](https://github.com/paulnorthrop/accessr/reference/rmd2many.md),
[`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md),
[`rmd2ioslides`](https://github.com/paulnorthrop/accessr/reference/rmd2ioslides.md),
[`rmd2slidy`](https://github.com/paulnorthrop/accessr/reference/rmd2slidy.md),
[`rmd2html`](https://github.com/paulnorthrop/accessr/reference/rmd2html.md).

## Author

**Maintainer**: Paul J. Northrop <p.northrop@ucl.ac.uk> \[copyright
holder\]

Authors:

- Paul J. Northrop <p.northrop@ucl.ac.uk> \[copyright holder\]
