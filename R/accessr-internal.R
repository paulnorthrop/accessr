#' Internal accessr functions
#'
#' Internal accessr functions
#' @details
#' These functions are not intended to be called by the user.
#' @name accessr-internal
#' @keywords internal
NULL

#' @keywords internal
#' @rdname accessr-internal
pns <- function(x, ...) {
  rmd2pdf(x, doc = "template.docx", dir = "C:/Users/Paul/", ...)
}

#' @keywords internal
#' @rdname accessr-internal
pnd <- function(x, ...) {
  rmd2pdf(x, doc = "template.docx",
          dir = "C:/Users/paul/Documents/R_PACKAGES/", ...)
}
