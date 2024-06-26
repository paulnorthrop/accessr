# ============================== rmd2ioslides =================================

#' Converts R markdown code to ioslides html presentations
#'
#' Creates accessible html ioslides presentations using R markdown's
#' \code{\link[rmarkdown]{ioslides_presentation}} argument to
#' \code{\link[rmarkdown]{render}}. Zip archives of the html files may be
#' created.
#'
#' @param x A character vector containing the names (\strong{no extension}) of
#'   the \code{.Rmd} files to convert if they are in the current working
#'   directory, or paths to the files, either absolute or relative to the
#'   current working directory, e.g., \code{DIRECTORY/file1}.  The \code{.html}
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
#'   \code{accessr_ioslides_pdf.zip}.
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
#'   \code{\link[rmarkdown]{ioslides_presentation}}. Pass (the default)
#'   \code{slide_level = 1} if you want a level one header # to create a new
#'   non-segue slide.
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
#'   working directory, in which case `rmd2ioslides()` will create ioslides
#'   presentations from all these Rmd files, but the \code{.Rmd} files may be
#'   in different directories.
#'
#'   The \code{\link[rmarkdown]{render}} function, with the argument
#'   \code{output_file =} \code{\link[rmarkdown]{ioslides_presentation}}
#'   creates the ioslides html files.
#'
#' The function \code{\link[rmarkdown]{ioslides_presentation}} has an
#' argument \code{slide_level} that sets the header level used as a slide
#' separator. The Lua filter \code{ioslides_presentation.lua} in
#' the \code{\link[rmarkdown]{rmarkdown}} package uses
#' any content between headers of level \code{slide_level} to create a segue
#' slide, which has a grey background and is intended only to contain a section
#' heading.
#'
#' In particular, under the default, \code{slide_level = 2}, content
#' between a level one header # and the next level two header ## is formatted
#' as a separate grey segue slide. If we do not want segue slides then we must
#' avoid using level one headers. However, for reasons of document
#' accessibility, we may want to use level one headers to separate slides.
#' For example, if we wish to create an ioslides presentation and a Word
#' document from the same source Rmd file then the Word document will only meet
#' fully accessibility requirements if the headings in the document start at
#' level one.
#'
#' In rmarkdown version 2.26, passing \code{slide_level = 1} to
#' \code{\link[rmarkdown]{ioslides_presentation}} does \strong{not} force a new
#' non-segue slide when a level one header # is used: it places
#' all content between # and the next ## on a grey segue slide and the
#' behaviour content of the resulting slides is not as intended. Passing
#' `slide_level = 1` to `rmd2ioslides()` replaces \code{rmarkdown}'s Lua filter
#' \code{ioslides_presentation.lua} with one that has been modified, so that
#' passing \code{slide_level = 1} **will** start a new non-segue slide.
#' A modified \code{default.css} css file is also used
#' that adjusts the font sizes for the header levels accordingly, so that the
#' level one header has a larger font than the level two header.
#' For values of \code{slide_level} greater than or equal to 2,
#' \code{\link[rmarkdown]{ioslides_presentation}} will behave as usual.
#'
#' If `slide_level` is not supplied then `slide_level = 1` is used.
#'
#' @return In addition to creating the html files, and perhaps zip files,
#'   a list containing the following (character vector) components is
#'   returned invisibly:
#'   \item{files }{(absolute) paths and file names of the files added to a zip
#'     file.}
#'   \item{zips }{(relative) paths and names of all the zip files.}
#' @seealso \code{\link{rmd2many}}, \code{\link{rmd2word}},
#'   \code{\link{rmd2slidy}}, \code{\link{rmd2html}} for other output formats.
#' @seealso The \href{https://paulnorthrop.github.io/accessr/}{accessr
#'   package page on Github}.
#' @examples
#' # Create an ioslides presentation from example.Rmd
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
#'   rmd2ioslides(ex_file)
#' }
#' @export
rmd2ioslides <- function(x, zip = if (length(x) == 1 & !add) FALSE else TRUE,
                         pdf = FALSE, zip_pdf = zip, pdf_args = list(),
                         add = FALSE, quiet = TRUE, rm_html = FALSE,
                         rm_pdf = FALSE, inc_rmd = FALSE, params = NULL, ...) {
  dots <- list(...)
  # If slide_level has not been supplied in ... then set slide_level = 1
  if (is.null(dots$slide_level)) {
    dots$slide_level <- 1
  }
  # If dots contains any instances of "black" then set the correct path
  # to accessr's black.css file
  if (!is.null(dots$css) && dots$css == "black") {
    dots$css <- system.file(package = "accessr", "examples", "black.css")
  }
  # Create a list of arguments to pass to rmd2presentation()
  arguments <- list(x = x, format = "ioslides", zip = zip, pdf = pdf,
                    zip_pdf = zip_pdf, pdf_args = pdf_args, add = add,
                    quiet = quiet, rm_html = rm_html, inc_rmd = inc_rmd,
                    params = params)
  arguments <- c(arguments, dots)
  val <- do.call(rmd2presentation, arguments)
  return(invisible(val))
}

