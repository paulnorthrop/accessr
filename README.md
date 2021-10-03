
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/paulnorthrop/stat0002?branch=master&svg=true)](https://ci.appveyor.com/project/paulnorthrop/stat0002)

## accessr

This package provides functions to produce accessible html and PDF
documents from input R markdown files. A main aim is to enable documents
of different formats to be produced from a single R markdown source file
using a single function call. A zip file containing multiple files can
be produced to make it easier to send files to others. The option to
print html output to (non-accessible) PDF files is also available.

[rmarkdown package](https://cran.r-project.org/package=rmarkdown)
[officedown package](https://cran.r-project.org/package=officedown)

PDF documents are produced from Word documents using
[OfficeToPDF](https://github.com/cognidox/OfficeToPDF), which must be
installed if this functionality is required.

### Installation

``` r
# Install package remotes, if necessary
install.packages("remotes")
remotes::install_github("paulnorthrop/accessr")
```

If you are using RStudio then it may be a good idea to close RStudio and
re-open it.

### Getting started

``` r
library(accessr)
?accessr
```

-   `rmd2html()`
-   `rmd2ioslides()`
-   `rmd2slidy()`
-   `rmd2word()`

Each function provides the option to create a zip archive containing the
output files.

### An example

Suppose that in your current working directory you have the R markdown
files `file1.Rmd` and `file2.Rmd`, a template Word file
`your_template.docx` and that the file `OfficeToPDF.exe` downloaded from
[OfficeToPDF releases](https://github.com/cognidox/OfficeToPDF/releases)
is either in your working directory or in a directory listed in
`searchpaths()`. Then the following will create files `file1.docx`,
`file2.docx`, `file1.pdf` and `file2.pdf` in your working directory.

``` r
rmd2pdf(c("file1", "file2"), doc = "your_template.docx")
```

A path to the Word template document can be provided using the `doc`
argument. If `doc` is not provided then a default template is used. A
path to the `OfficeToPDF.exe` file can be provided using an argument
`dir`.
