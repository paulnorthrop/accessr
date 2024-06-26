#' accessr: Command Line Tools to Produce Accessible Documents using R
#' markdown
#'
#' Provides functions to produce accessible HTML and PDF documents from input
#' R markdown files. Currently, \strong{accessr} only provides the option to
#' produce accessible PDF files on a Windows Operating System. One aspect of
#' accessibility is providing a headings structure that is recognised by a
#' screen reader, providing a navigational tool for a blind or
#' partially-sighted person. A key aim is to produce documents of different
#' formats from each of a collection of R markdown source files. A
#' user-supplied template Word document can be used to determine the
#' formatting of an output Word document. Similar functions produce HTML slides
#' and HTML documents. A zip file containing multiple files can be produced.
#' The option to print HTML output to (non-accessible) PDF files is also
#' available.
#'
#' @details See the \href{https://paulnorthrop.github.io/accessr/}{accessr
#'   package page on Github} for more information.  An example Rmd file is
#'   available at
#'   \code{system.file(package = "accessr", "examples", "example.Rmd")}.
#'
#' On a Windows Operating System, Accessible PDF documents are produced by
#' creating Word documents from R markdown files and then PDF documents from
#' these Word documents.  The first step uses the
#' \code{\link[rmarkdown]{render}} function from the
#' \href{https://cran.r-project.org/package=rmarkdown}{rmarkdown package}
#' and the \code{\link[officedown]{rdocx_document}} function from the
#' officedown package. The second step uses
#' \href{https://github.com/cognidox/OfficeToPDF}{OfficeToPDF}.
#'
#' The main functions in \code{accessr} are:
#'
#' \itemize{
#'   \item \code{\link{rmd2many}}: create HTML slides, PDF slides, Word and
#'     PDF documents from a single R markdown file.
#'   \item \code{\link{rmd2word}}: create Word documents and accessible PDF
#'    files.
#'
#'    \code{\link{install_otp}}: convenience function to install
#'     OfficeToPDF, to create PDF files from Word documents in
#'     \code{\link{rmd2word}}. \code{\link{ext_img}}: a function to enable the
#'     \code{knitr} chunk options \code{out.width} and/or \code{out.height} to
#'     work when the output format is a Word document
#'   \item \code{\link{rmd2ioslides}}: create ioslides presentations and
#'    perhaps print to (non-accessible) PDF documents.
#'   \item \code{\link{rmd2slidy}}: create slidy presentations and perhaps
#'    print to (non-accessible) PDF documents..
#'   \item \code{\link{rmd2html}}: create html documents and perhaps print to
#'    (non-accessible) PDF documents.
#' }
#'
#' The `rmd2?` functions provide the option to create a zip archive containing
#' the output files.  All the .Rmd files in a directory can be processed with
#' one function call. Information such as \code{title}, \code{author},
#' \code{lang} etc in the YAML header in the Rmd file are used but
#' \code{output} is ignored.
#'
#' @references David Gohel and Noam Ross (2021). officedown: Enhanced
#'   'R Markdown' Format for 'Word' and 'PowerPoint'. R package version 0.3.1.
#'   \url{https://CRAN.R-project.org/package=officedown}
#' @references JJ Allaire, Yihui Xie, Christophe Dervieux, Jonathan McPherson,
#'   Javier Luraschi, Kevin Ushey, Aron Atkins, Hadley Wickham, Joe Cheng,
#'   Winston Chang, and Richard Iannone (2024). rmarkdown: Dynamic Documents
#'   for R. R package version 2.26, \url{https://rmarkdown.rstudio.com}.
#' @seealso \code{\link{install_otp}}, \code{\link{rmd2many}},
#'   \code{\link{rmd2word}}, \code{\link{rmd2ioslides}},
#'   \code{\link{rmd2slidy}}, \code{\link{rmd2html}}.
#' @docType package
"_PACKAGE"
