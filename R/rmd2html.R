# =============================== rmd2html ====================================

#' Converts R markdown code to html documents
#'
#' Creates accessible html documents using R markdown's
#' \code{\link[rmarkdown]{html_document}} argument to
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
#'   directory involved in \code{x}.  If \code{zip = TRUE} each archive of html
#'   files is named \code{accessr_html.zip}.  If \code{zip} is a character
#'   vector of zip file names (no extension) then these names are used to name
#'   the zip archives.  The names are recycled to the length of the number of
#'   unique directories, if necessary.
#' @param pdf A logical scalar.  If \code{pdf = TRUE} then each html file is
#'   printed to a PDF file using \code{\link[pagedown]{chrome_print}}.
#'   Google Chrome (or an alternative browser specified in \code{pdf_args} by
#'   the \code{browser} argument to \code{\link[pagedown]{chrome_print}} must
#'   be installed prior to use of this option. An error message like
#'   \code{Error in servr::random_port(NULL) : Cannot find an available TCP
#'   port} means that the \code{random_port} function in the \code{servr}
#'   package could not find an internet connection that Chrome considers
#'   secure.  Perhaps you are using a coffee shop's wifi.
#' @param pdf_args A list of arguments passed to
#'   \code{\link[pagedown]{chrome_print}}. \code{input} cannot be passed
#'   because it is set inside \code{rmd2html}.
#' @param zip_pdf As \code{zip}, but relates to the
#'   creation of zip archives for any PDF files created. If
#'   \code{zip_pdf = TRUE} then each archive is named
#'   \code{accessr_html_pdf.zip}.
#' @param add A logical scalar that determines what happens if an output
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
#' @param ... Additional arguments passed to
#'   \code{\link[rmarkdown]{html_document}} or \code{\link[rmarkdown]{render}}.
#' @details Information such as \code{title}, \code{author}, \code{lang} etc in
#'   the YAML header in the Rmd file are used but \code{output} is ignored.
#'
#'   The simplest setup is to have the \code{.Rmd} files in the current
#'   working directory, in which case `rmd2html()` will create HTML documents
#'   from all these Rmd files, the \code{.Rmd} files may be in different
#'   directories.
#'
#'   The \code{\link[rmarkdown]{render}} function, with the argument
#'   \code{output_file =} \code{\link[rmarkdown]{html_document}}
#'   creates the html files.
#' @return In addition to creating the html files, and perhaps zip files,
#'   a list containing the following (character vector) components is
#'   returned invisibly:
#'   \item{files }{(absolute) paths and file names of the files added to a zip
#'     file.}
#'   \item{zips }{(relative) paths and names of all the zip files.}
#' @seealso \code{\link{rmd2many}}, \code{\link{rmd2word}},
#'   \code{\link{rmd2ioslides}}, \code{\link{rmd2slidy}}  for other output
#'   formats.
#' @seealso The \href{https://paulnorthrop.github.io/accessr/}{accessr
#'   package page on Github}.
#' @examples
#' # Create an HTML document from example.Rmd
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
#'   rmd2html(ex_file)
#' }
#' @export
rmd2html <- function(x, zip = if (length(x) == 1 & !add) FALSE else TRUE,
                     pdf = FALSE, pdf_args = list(), zip_pdf = zip,
                     add = FALSE, quiet = TRUE, rm_html = FALSE,
                     rm_pdf = FALSE, ...) {
  # If x is missing then find all the .Rmd files in the working directory
  # If x is a directory then find all the . RMd files in that directory
  if (missing(x)) {
    rmd_files <- list.files(pattern = "Rmd")
    x <- sub(".Rmd", "", rmd_files)
    html_files <- sub(".Rmd", ".html", rmd_files)
    pdf_files <- sub(".Rmd", ".pdf", rmd_files)
  } else if (length(x) == 1 && dir.exists(x)) {
    rmd_files <- list.files(x, pattern = "Rmd")
    rmd_files <- paste0(x, "/", rmd_files)
    x <- sub(".Rmd", "", rmd_files)
    html_files <- paste0(x, ".html")
    pdf_files <- paste0(x, ".pdf")
  } else {
    rmd_files <- paste0(x, ".Rmd")
    html_files <- paste0(x, ".html")
    pdf_files <- paste0(x, ".pdf")
  }
  # Make doc the same length as x
  lenx <- length(x)
  # Function for Rmd to html
  render_fun <- function(i) {
    # Render the .Rmd file as an html document
    rmarkdown::render(input = rmd_files[i],
                      output_format = rmarkdown::html_document(...),
                      quiet = quiet, ...)
  }
  res <- sapply(1:lenx, render_fun)
  res <- list(files = res)
  # Create pdf files, if required
  if (pdf) {
    if (!requireNamespace("pagedown", quietly = TRUE)) {
      stop("The 'pagedown' package is required. Please install it.",
           call.= FALSE)
    }
    pdf_fun <- function(i) {
      # Print to pdf
      pdf_args$input <- html_files[i]
      do.call(pagedown::chrome_print, pdf_args)
    }
    res_pdf <- sapply(1:lenx, pdf_fun)
  }
  # Identify the different directories in x
  dnames <- dirname(rmd_files)
  # Unique directories
  udnames <- unique(dirname(rmd_files))
  # Create html zip file(s), if required
  if (is.character(zip)) {
    zipfile <- rep_len(zip, length(udnames))
    zip <- TRUE
  } else if (is.logical(zip) && zip) {
    zipfile <- rep_len("accessr_html", length(udnames))
  }
  if (zip) {
    res_zip <- accessr_zip(x, dnames, udnames, zipfile, add,
                           extension = ".html")
    res <- c(res, list(zips = res_zip))
    if (rm_html) {
      sapply(html_files, file.remove)
    }
  }
  # Create pdf zip file(s), if required
  if (is.character(zip_pdf)) {
    zipfile <- rep_len(zip_pdf, length(udnames))
    zip_pdf <- TRUE
  } else if (is.logical(zip_pdf) && zip_pdf) {
    zipfile <- rep_len("accessr_html_pdf", length(udnames))
  }
  if (pdf && zip_pdf) {
    res_zip_pdf <- accessr_zip(x, dnames, udnames, zipfile, add,
                               extension = ".pdf")
    res <- c(res, list(pdf_zips = res_zip_pdf))
    if (rm_pdf) {
      sapply(pdf_files, file.remove)
    }
  }
  return(invisible(res))
}

