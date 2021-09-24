# ================================ rmd2pdf ====================================

#' Converts rmarkdown code to Word and PDF documents
#'
#' Creates accessible PDF documents by creating Word documents using
#' rmarkdown code and then producing PDF files.
#'
#' @param x A logical vector containing the names of the files to convert
#'   (no extension).  May be a path and filename relative to the current
#'   working directory, e.g., `DIRECTORY/file1`.
#' @param dir A path to the directory in which the file \code{officetopdf.exe}
#'   sits.  This is not needed if this file sits in the current working
#'   directory or a directory in the list returned by \code{searchpaths()}.
#'   See \strong{Details}.
#' @param doc The name of a template Word document from which the fonts,
#'   margins etc in the output Word document will based.  If this is not in the
#'   working directory then also give the path.
#' @param ... Additional arguments to be passed to \code{\link{system}}.
#'   The argument \code{wait} determines whether or not R will wait for the
#'   PDF files to be produced.  In the default \code{wait = TRUE} case a
#'   warning will be given if any of the PDF files could not be produced.
#'   This will occur if there is an existing PDF file of the same name open
#'   in another application.  If \code{wait = FALSE} then no warnings will be
#'   produced.
#' @details The \code{\link[rmarkdown]{render}} function is used to create
#'   a Word file in the current working directory.
#'
#'   Then
#'   \href{https://github.com/cognidox/OfficeToPDF}{OfficeToPDF} is used to
#'   convert the Word file to a PDF file.  The file \code{officetopdf.exe}
#'   needs to be downloaded from the
#'   \href{https://github.com/cognidox/OfficeToPDF/releases}{OfficeToPDF releases}
#'   page and placed in the directory specified by the argument \code{dir}, or
#'   in a directory that is in the list returned by \code{\link{searchpaths}}.
#'   If \code{officetopdf.exe} cannot be found then an error is thrown.
#'
#'   \code{pns} and \code{pnd} are convenience functions for useonly by the
#'   package maintainer.
#' @return A vector containing the values returned from \code{\link{system}}
#'   is returned invisibly. Note that if \code{wait = FALSE} then these values
#'   will be 0 (the success value) even if some of the PDF files could not be
#'   produced.  The error code 17234 indicates that a PDF file was open in
#'   another application.
#' @examples
#' \dontrun{
#'   rmd2pdf(c("file1", "file2"), doc = "template.docx")
#' }
#' @name rmd2pdf
#' @export
rmd2pdf <- function(x, dir = NULL, doc = NULL, ...) {
  # Just in case there are repeated file names
  x <- unique(x)
  # Path to the officetopdf executable
  if (is.null(dir)) {
    exefile <- "officetopdf.exe"
  } else {
    exefile <- paste0(dir, "/officetopdf.exe")
  }
  # If no template word document has been supplied then use the default
  if (is.null(doc)) {
    doc <- "default"
  }
  # Function for Rmd to Word to PDF
  fun <- function(x) {
    # Convert .Rmd file to Word document
    rmarkdown::render(paste0(x, ".Rmd"),
                      rmarkdown::word_document(reference_docx = doc))
    # Convert Word document to PDF document
    print(exefile)
    print(paste(exefile, paste0(x, ".docx"), paste0(x, ".pdf")))
    system(paste(exefile, paste0(x, ".docx"), paste0(x, ".pdf")), ...)
  }
  res <- sapply(x, fun)
  # Error codes
  # 127 officetopdf.exe could not be found
  # 17234 file open in another application
  if (any(res == 127)) {
    stop("officetopdf.exe could not be found")
  }
  if (any(res) != 0) {
    which(res != 0)
    x[res != 0]
    warning(paste0(x[res != 0], ".pdf "), "could not be written")
  }
  invisible(res)
}

#' @name rmd2pdf
#' @export
pns <- function(x, ...) {
  rmd2pdf(x, dir = "C:/Users/Paul/", doc = "template.docx", ...)
}

#' @name rmd2pdf
#' @export
pnd <- function(x, ...) {
  rmd2pdf(x, dir = "C:/Users/paul/Documents/R_PACKAGES/",
      doc = "template.docx", ...)
}
