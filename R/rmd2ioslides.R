# ============================== rmd2ioslides =================================

#' Converts R markdown code to ioslides html presentations
#'
#' Creates accessible html ioslides presentations using R markdown's
#' \code{\link[rmarkdown]{ioslides_presentation}} argument to
#' \code{\link[rmarkdown]{render}}. Zip archives of the html files may be
#' created.
#'
#' @param x A character vector containing the names (no extension) of the
#'   \code{.Rmd} files to convert  if they are in the current working directory, or
#'   paths to the files, either absolute or relative to the current working
#'   directory, e.g., \code{DIRECTORY/file1}.  The \code{.html}
#'   files are created in the same directory as their respective \code{.Rmd}
#'   file.  If \code{x} is missing then an html file is created from each of
#'   the \code{.Rmd} files in the current working directory.
#' @param zip A logical scalar or character vector indicating whether html
#'   files should be put into a zip archive.  If \code{zip = FALSE} then no
#'   zip archive is created.  Otherwise, an archive is created in each unique
#'   directory involved in \code{x}.  If \code{zip = TRUE} then any archive
#'   created has the name \code{accessr_ioslides.zip}.  If \code{zip} is a
#'   character vector of zip file names (no extension) then these names are
#'   used to name the zip archives.  The names are recycled to the length of
#'   the number of unique directories, if necessary.
#' @param pdf A logical scalar.  If \code{pdf = TRUE} then each html file is
#'   printed to a PDF file using \code{\link[pagedown]{chrome_print}}.
#'   Google Chrome (or an alternative browser specified by the \code{browser}
#'   argument to \code{\link[pagedown]{chrome_print}}) must be installed prior
#'   to use of this option.
#' @param zip_pdf Works in the same way as \code{zip}, but relates to the
#'   creation of zip archives for any PDF files created. If
#'   \code{zip_pdf = TRUE} then each archive is named
#'   \code{accessr_html_pdf.zip}.
#' @param pdf_args A list of arguments to be passed to
#'   \code{\link[pagedown]{chrome_print}}. \code{input} cannot be passed
#'   because it is set inside \code{rmd2html}.
#' @param add A logical scalar that determines what happens if the output
#'   zip file already exists.  If \code{add = TRUE} then files are added to the
#'   zip file and if \code{add = FALSE} then the zip file is deleted and will
#'   only contain newly-created files.
#' @param quiet Argument of the same name passed to
#'   \code{\link[rmarkdown]{render}} to determine what is printed during
#'   rendering from knitr.
#' @param rm_html A logical scalar.  If \code{rm_html = TRUE} and a zip archive
#'   of html files is produced then the individual html files are deleted.
#'   Otherwise, they are not deleted.
#' @param rm_pdf A logical scalar.  If \code{rm_pdf = TRUE} and a zip archive
#'   of pdf files is produced then the individual pdf files are deleted.
#'   Otherwise, they are not deleted.
#' @param ... Additional arguments to be passed to
#'   \code{\link[rmarkdown]{ioslides_presentation}}.
#' @details Information such as \code{title}, \code{author}, \code{lang} etc in
#'   the YAML header in the Rmd file are used but \code{output} is ignored.
#'
#' The simplest setup is to have the \code{.Rmd} files in the current
#'   working directory, but it is possible to have the \code{.Rmd} files in
#'   different directories.
#'
#'   The \code{\link[rmarkdown]{render}} function, with the argument
#'   \code{output_file =} \code{\link[rmarkdown]{ioslides_presentation}}
#'   creates the ioslides html files.
#' @return In addition to creating the html files, and perhaps zip files,
#'   a list containing the following (character vector) components is
#'   returned invisibly:
#'   \item{files }{(absolute) paths and file names of the files added to a zip
#'     file.}
#'   \item{zips }{(relative) paths and names of all the zip files.}
#' @examples
#' \dontrun{
#' # All files in the current working directory
#' rmd2ioslides(c("TEST/file1", "TEST/file2"))
#' }
#' @export
rmd2ioslides <- function(x, zip = TRUE, pdf = FALSE, zip_pdf = zip,
                         pdf_args = list(), add = FALSE, quiet = TRUE,
                         rm_html = FALSE, rm_pdf = FALSE, ...) {
  rmd2presentation(x = x, format = "ioslides", zip = zip, pdf = pdf,
                   zip_pdf = zip_pdf, pdf_args= pdf_args, add = add,
                   quiet = quiet, rm_html = rm_html, ...)
}

