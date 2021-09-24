#' accessr: Produce Accessible PDF Documents using R markdown
#'
#' Accessible PDF documents are produced by creating Word documents from R
#' markdown files and then PDF documents from these Word documents.  The first
#' step uses the \code{\link[rmarkdown]{render}} function from the
#' \href{https://cran.r-project.org/package=rmarkdown}{rmarkdown package}
#' The second step uses
#' \href{https://github.com/cognidox/OfficeToPDF}{OfficetoPDF}.
#'
#' @details See \code{\link{rmd2pdf}}.
#' @references JJ Allaire, Yihui Xie, Jonathan McPherson, Javier Luraschi,
#'   Kevin Ushey, Aron Atkins, Hadley Wickham, Joe Cheng, Winston Chang and
#'   Richard Iannone (2021). rmarkdown: Dynamic Documents for R. R package
#'   version 2.9. \url{https://rmarkdown.rstudio.com}.
#' @seealso \code{\link{rmd2pdf}}.
#' @docType package
#' @name accessr
NULL
