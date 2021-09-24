
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/paulnorthrop/stat0002?branch=master&svg=true)](https://ci.appveyor.com/project/paulnorthrop/stat0002)

## accessr

Accessible PDF documents are produced by creating Word documents from R
markdown files and then PDF documents from these Word documents. The
first step uses the `render` function from the [rmarkdown
package](https://cran.r-project.org/package=rmarkdown). The second step
uses [https://github.com/cognidox/OfficeToPDF](OfficetoPDF).

### Installation

``` r
install.packages("remotes")
```

You only need to do this once.

``` r
remotes::install_github("paulnorthrop/accessr")
```

### Getting started

``` r
library(accessr)
?accessr
```

## An example

Suppose that in your current working directory you have the R markdown
files `file1.Rmd` and `file2.Rmd`, a template Word file `template.docx`
and that the file `officetopdf.exe` downloaded from
[https://github.com/cognidox/OfficeToPDF/releases](OfficeToPDF%20releases)
is either in your working directory or in a directory listed in
`searchpaths()`. Then the following will create files `file1.docx`,
`file2.docx`, `file1.pdf` and `file2.pdf` in your working directory.

``` r
rmd2pdf(c("file1", "file2"), doc = "template.docx")
```

A path to the Word template document can be provided using the `doc`
argument. If `doc` is not provided then a default template is used. A
path to the `officetopdf.exe` file can be provided using an argument
`dir`.
