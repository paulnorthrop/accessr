#' accessr: Command Line Tools to Produce Accessible Documents using R
#' markdown
#'
#' This package provides functions to produce accessible html and PDF documents
#' from input R markdown files.  A main aim is to enable documents of different
#' formats to be produced from a single R markdown source file using a single
#' function call.  The function \code{\link{rmd2word}} enables the use of the
#' the \code{knitr} chunk options \code{out.width} and/or \code{out.height} to
#' set the dimensions a figure when the output format is a Word document. A
#' user-supplied template Word document can be used to determine the formatting
#' of the output Word document. Similar functions produce html slides and html
#' documents. A zip file containing multiple files can be
#' produced. The option to print html output to (non-accessible) PDF files is
#' also available.
#'
#' @details The main functions in \code{accessr} are:
#'
#' \itemize{
#'   \item \code{\link{install_otp}}: convenience function to install
#'     OfficeToPDF.
#'   \item \code{\link{rmd2word}}: create word documents and accessible PDF
#'    files.
#'   \item \code{\link{rmd2ioslides}}: create isoslides presentations and
#'    perhaps print to (non-accessible) PDF documents.
#'   \item \code{\link{rmd2slidy}}: create slidy presentations and perhaps
#'    print to (non-accessible) PDF documents..
#'   \item \code{\link{rmd2html}}: create html documents and perhaps print to
#'    (non-accessible) PDF documents.
#'   \item \code{\link{ext_img}}: a function to enable the \code{knitr} chunk
#'    options \code{out.width} and/or \code{out.height} to work when the output
#'    format is a Word document.
#' }
#'
#' Each function provides the option to create a zip archive containing the
#' output files.
#'
#' Accessible PDF documents are produced by creating Word documents from R
#' markdown files and then PDF documents from these Word documents.  The first
#' step uses the \code{\link[rmarkdown]{render}} function from the
#' \href{https://cran.r-project.org/package=rmarkdown}{rmarkdown package}
#' and the \code{\link[officedown]{rdocx_document}} function from the
#' [officedown package](https://cran.r-project.org/package=officedown).
#' The second step uses
#' \href{https://github.com/cognidox/OfficeToPDF}{OfficetoPDF}.
#'
#' @references David Gohel and Noam Ross (2021). officedown: Enhanced
#'   'R Markdown' Format for 'Word' and 'PowerPoint'. R package version 0.2.2.
#'   \url{https://CRAN.R-project.org/package=officedown}
#' @references JJ Allaire, Yihui Xie, Jonathan McPherson, Javier Luraschi,
#'   Kevin Ushey, Aron Atkins, Hadley Wickham, Joe Cheng, Winston Chang and
#'   Richard Iannone (2021). rmarkdown: Dynamic Documents for R. R package
#'   version 2.9. \url{https://rmarkdown.rstudio.com}.
#' @seealso \code{\link{install_otp}}, \code{\link{rmd2word}},
#'   \code{\link{rmd2html}}, \code{\link{rmd2ioslides}},
#'   \code{\link{rmd2slidy}}, \code{\link{ext_img}}.
#' @docType package
#' @name accessr
NULL
