# =============================== rmd2slidy ==================================

#' Converts R markdown code to slidy html presentations
#'
#' Creates accessible html slidy presentations using R markdown's
#' \code{\link[rmarkdown]{slidy_presentation}} argument to
#' \code{\link[rmarkdown]{render}}. Zip archives of the html files may be
#' created.
#'
#' @param x A character vector containing the names (\strong{no extension}) of
#'   the \code{.Rmd} files to convert  if they are in the current working
#'   directory, or paths to the files, either absolute or relative to the
#'   current working directory, e.g., \code{DIRECTORY/file1}.  The \code{.html}
#'   files are created in the same directory as their respective \code{.Rmd}
#'   file.  If \code{x} is missing then an html file is created from each of
#'   the \code{.Rmd} files in the current working directory.
#' @param zip A logical scalar or character vector indicating whether html
#'   files should be put into a zip archive.  If \code{zip = FALSE} then no
#'   zip archive is created.  Otherwise, an archive is created in each unique
#'   directory involved in \code{x}.  If \code{zip = TRUE} then any archive
#'   created has the name \code{accessr_slidy.zip}.  If \code{zip} is a
#'   character vector of zip file names (no extension) then these names are
#'   used to name the zip archives.  The names are recycled to the length of
#'   the number of unique directories, if necessary.
#' @param pdf A logical scalar.  If \code{pdf = TRUE} then each html file is
#'   printed to a PDF file using \code{\link[pagedown]{chrome_print}}.
#'   Google Chrome (or an alternative browser specified in \code{pdf_args} by
#'   the \code{browser} argument to \code{\link[pagedown]{chrome_print}} must
#'   be installed prior to use of this option.  An error message like
#'   \code{Error in servr::random_port(NULL) : Cannot find an available TCP
#'   port} means that the \code{random_port} function in the \code{servr}
#'   package could not find an internet connection that Chrome considers
#'   secure.  Perhaps you are using a coffee shop's wifi.
#' @param zip_pdf As \code{zip}, but relates to the
#'   creation of zip archives for any PDF files created. If
#'   \code{zip_pdf = TRUE} then each archive is named
#'   \code{accessr_html_pdf.zip}.
#' @param pdf_args A list of arguments passed to
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
#' @param inc_rmd A logical scalar.  If \code{inc_rmd = TRUE} then the source
#'   Rmd files are included in the zip file created.  Otherwise, they are not
#'   included.
#' @param params A list of named parameters to pass as the argument
#'   \code{params} to \code{\link[rmarkdown]{render}}.
#' @param ... Additional arguments passed to
#'   \code{\link[rmarkdown]{slidy_presentation}}.
#'
#'   If \code{css = "black"} is passed then \code{accessr}'s css file
#'   \code{black.css} is used, which results in black text being used in the
#'   slides.
#'
#'   This function is \strong{not} vectorised with respect to arguments in
#'   \code{...}.
#' @details Information such as \code{title}, \code{author}, \code{lang} etc in
#'   the YAML header in the Rmd file are used but \code{output} is ignored.
#'
#'   The simplest setup is to have the \code{.Rmd} files in the current
#'   working directory, in which case `rmd2slidy()` will create slidy
#'   presentations from all these Rmd files, but the \code{.Rmd} files may be
#'   in different directories.
#'
#'   The \code{\link[rmarkdown]{render}} function, with the argument
#'   \code{output_file =} \code{\link[rmarkdown]{slidy_presentation}}
#'   creates the slidy html files.
#' @return In addition to creating the html files, and perhaps zip files,
#'   a list containing the following (character vector) components is
#'   returned invisibly:
#'   \item{files }{(absolute) paths and file names of the files added to a zip
#'     file.}
#'   \item{zips }{(relative) paths and names of all the zip files.}
#' @seealso \code{\link{rmd2word}}, \code{\link{rmd2word}},
#'   \code{\link{rmd2ioslides}}, \code{\link{rmd2html}} for other output
#'   formats.
#' @seealso The \href{https://paulnorthrop.github.io/accessr/}{accessr
#'   package page on Github}.
#' @examples
#' # Create a slidy presentation from example.Rmd
#' got_hux <- requireNamespace("huxtable", quietly = TRUE)
#' got_flex <- requireNamespace("flextable", quietly = TRUE)
#' got_pandoc <- rmarkdown::pandoc_available("1.14")
#' got_all <- got_hux && got_flex && got_pandoc
#' # This example needs packages huxtable and flextable
#' if (got_all) {
#'   ex_file <- system.file(package = "accessr", "examples", "example.Rmd")
#'   file.copy(ex_file, tdir <- tempdir(check = TRUE), overwrite = TRUE)
#'   ex_file <- list.files(tdir, pattern = "example.Rmd", full.names = TRUE)
#'   ex_file <- sub(".Rmd", "", ex_file)
#'   rmd2slidy(ex_file)
#' }
#' @export
rmd2slidy <- function(x, zip = if (length(x) == 1 & !add) FALSE else TRUE,
                      pdf = FALSE, zip_pdf = zip, pdf_args = list(),
                      add = FALSE, quiet = TRUE, rm_html = FALSE,
                      rm_pdf = FALSE, inc_rmd = FALSE, params = NULL, ...) {
  dots <- list(...)
  # If dots contains any instances of "black" then set the correct path
  # to accessr's black.css file
  if (!is.null(dots$css) && dots$css == "black") {
    dots$css <- system.file(package = "accessr", "examples", "black.css")
  }
  # Create a list of arguments to pass to rmd2presentation()
  arguments <- list(x = x, format = "slidy", zip = zip, pdf = pdf,
                    zip_pdf = zip_pdf, pdf_args = pdf_args, add = add,
                    quiet = quiet, rm_html = rm_html, inc_rmd = inc_rmd,
                    params = params)
  arguments <- c(arguments, dots)
  val <- do.call(rmd2presentation, arguments)
  return(invisible(val))
}

