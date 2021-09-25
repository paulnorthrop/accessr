# ================================ rmd2pdf ====================================

#' Converts rmarkdown code to Word and PDF documents
#'
#' Creates accessible PDF documents by creating Word documents using
#' rmarkdown code and then producing PDF files.
#'
#' @param x A character vector containing the names of the \code{.Rmd} files to
#'   convert (no extension) if they are in the current working directory, or
#'   paths to the files, either absolute or relative to the current working
#'   directory, e.g., \code{DIRECTORY/file1}.  The \code{.docx} and \code{.pdf}
#'   files are created in the same directory as their respective \code{.Rmd}
#'   file.
#' @param doc An optional argument to identify a template Word document, or
#'   respective documents for the files in \code{x}, from which the fonts,
#'   margins etc in the output Word documents will based.  If \code{doc} is not
#'   supplied then a default is used.  Different templates may be used for
#'   different files.  \code{rep_len(doc, length(x))} is used to force
#'   \code{length(doc)} to have the same length as \code{x}. A component equal
#'   to \code{"default"} may be used to use the default Word template.
#'   See \strong{Details} for information about how to identify the locations
#'   of the Word template files.
#' @param dir A path to the directory in which the file \code{officetopdf.exe}
#'   sits.  This is not needed if this file sits in the current working
#'   directory or a directory in the list returned by \code{searchpaths()}.
#'   Otherwise, it may be a path relative to the current working directory
#'   or an absolute path.
#' @param ... Additional arguments to be passed to \code{\link{system}}.
#'   The argument \code{wait} determines whether or not R will wait for the
#'   PDF files to be produced.  In the default \code{wait = TRUE} case a
#'   warning will be given if any of the PDF files could not be produced.
#'   This will occur if there is an existing PDF file of the same name open
#'   in another application.  If \code{wait = FALSE} then no warnings will be
#'   produced.
#' @details The simplest setup is to have the \code{.Rmd} files and the Word
#'   template (if used) and \code{officetopdf.exe} in the current working
#'   directory.
#'
#'   It is possible to have the \code{.Rmd} files in different
#'   directories, but if any non-\code{"default"} values in \code{doc} must be
#'   such that the \code{reference_docx} argument of
#'   \code{\link[rmarkdown]{word_document}} finds a template Word file.
#'   If the template is in the same directory as its respective \code{.Rmd}
#'   component in  \code{x} then the filename, e.g. \code{template.docx} will
#'   suffice.  Otherwise, a path to the template should be given, either
#'   relative to the directory in which the \code{.Rmd} file sits, or an
#'   absolute path.
#'
#'   The \code{\link[rmarkdown]{render}} function creates a Word file from
#'   each input \code{.Rmd} file.  Then
#'   \href{https://github.com/cognidox/OfficeToPDF}{OfficeToPDF} is used to
#'   convert the Word file to a PDF file.  The file \code{officetopdf.exe}
#'   needs to be downloaded from the
#'   \href{https://github.com/cognidox/OfficeToPDF/releases}{OfficeToPDF releases}
#'   page and placed in the directory specified by the argument \code{dir}, or
#'   in a directory that is in the list returned by \code{\link{searchpaths}}.
#'   If \code{officetopdf.exe} cannot be found then an error is thrown.
#'
#'   \code{pns} and \code{pnd} are convenience functions for the
#'   package maintainer.
#' @return A vector containing the values returned from \code{\link{system}}
#'   is returned invisibly. Note that if \code{wait = FALSE} then these values
#'   will be 0 (the success value) even if some of the PDF files could not be
#'   produced.  The error code 17234 indicates that a PDF file was open in
#'   another application.
#' @examples
#' \dontrun{
#' # All files in the current working directory
#' rmd2pdf(c("file1", "file2"), doc = "template.docx")
#' }
#' @name rmd2pdf
#' @export
rmd2pdf <- function(x, doc = NULL, dir = NULL, ...) {
  # Path to the officetopdf executable
  if (is.null(dir)) {
    exefile <- "officetopdf.exe"
  } else {
    exefile <- paste0(dir, "/officetopdf.exe")
  }
  # If no template word document has been supplied then use the default
  if (length(doc) == 0 && is.null(doc)) {
    doc <- "default"
  }
  # Make doc the same length as x
  lenx <- length(x)
  doc <- rep_len(doc, lenx)
  # Function for Rmd to Word to PDF
  fun <- function(i) {
    # Convert .Rmd file to Word document
    rmarkdown::render(paste0(x[i], ".Rmd"),
                      rmarkdown::word_document(reference_docx = doc[i]))
    # Convert Word document to PDF document
    system(paste(exefile, paste0(x[i], ".docx"), paste0(x[i], ".pdf")), ...)
  }
  res <- sapply(1:lenx, fun)
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

