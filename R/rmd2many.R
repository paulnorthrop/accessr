#' Create content in multiple formats
#'
#' From a single R markdown file create HTML slides, PDF slides, Word
#' and PDF documents.
#'
#' @param x A character vector containing the names (\strong{no extension}) of
#'   the \code{.Rmd} files to convert if they are in the current working
#'   directory, or paths to the files, either absolute or relative to the
#'   current working directory, e.g., \code{DIRECTORY/file1}.  The output
#'   files are created in the same directory as their respective \code{.Rmd}
#'   file.
#' @param outputs A character vector. Specifies the output formats required.
#'   A subset of `c("word", "ioslides", "slidy", "html")`. If more than one of
#'   \code{"ioslides"}, \code{"slidy"} and \code{"html"} are present then only
#'   one of these is used with the order of preference \code{"ioslides"},
#'   \code{"slidy"} then \code{"html"}.
#' @param slide_level Passed to [`rmd2ioslides`][accessr::rmd2ioslides] via
#'   `...`. The default `slide_level = 1` means that a level one header #
#'   create a new non-segue slide for an ioslides presentation.
#' @param css The argument \code{css} passed to
#'   \code{\link[rmarkdown]{ioslides_presentation}} or
#'   \code{\link[rmarkdown]{slidy_presentation}}. If \code{css = "black"}
#'   then \code{accessr}'s css file \code{black.css} is used, which results in
#'   black text being used in the slides.
#'   `css` is not used if `outputs = html`.
#' @param add18  A logical scalar. If `TRUE` then we also create Word documents
#'   with 18pt text.
#' @param pdf A logical scalar. If `TRUE` then we use
#'   [`chrome_print`][pagedown::chrome_print] to print PDF versions of HTML
#'   files produced using the output `"ioslides"` or `"slidy"`. and/or
#'   \code{OfficeToPDF.exe} to create PDF files from any Word documents that
#'   are produced.
#' @param highlight A named list, with names a subset of
#'   `c("word", "ioslides", "slidy")`, providing the respective syntax
#'   highlighting styles passed to Pandoc for the output formats. Any syntax
#'   highlighting provided in `css` will take precedence.
#'   `highlight` is not used if `outputs = html`.
#' @param params A list of named parameters to pass as the argument
#'   \code{params} to \code{\link[rmarkdown]{render}}. In the example below,
#'   the file `example.Rmd` has a parameter `hide`. If `hide = TRUE` then
#'   parts of the output are hidden using the `knitr` chunk options `echo` and
#'   `eval`.
#' @param zip A logical scalar or character vector indicating whether PDF
#'   files should be put into a zip archive.  If \code{zip = FALSE} then no
#'   zip archive is created.  Otherwise, an archive is created in each unique
#'   directory involved in \code{x}.  If \code{zip = TRUE} then any archive
#'   created is named after the first filename in \code{x} from the relevant
#'   directory. If \code{zip} is a character vector of zip file names (no
#'   extension) then these names are used to name the zip archives.  The names
#'   are recycled to the length of the number of unique directories if
#'   necessary.
#' @param ... Additional arguments to be passed to
#'   [`rmd2ioslides`][accessr::rmd2ioslides],
#'   [`rmd2slidy`][accessr::rmd2slidy],
#'   [`rmd2word`][accessr::rmd2word] or
#'   [`rmd2html`][accessr::rmd2html].
#' @details The default setting creates, for each valid `filename` in `x`, the
#'   following files
#'
#'   * `filename.html`: lecture slides in `ioslides` format.
#'   * `filename_slides.pdf`: a PDF document containing the content in
#'     `filename.html`.
#'   * `filename.pdf`: a PDF document created from a Word document produced by
#'     `rmd2word`.
#'   * `filename.docx`: a Word document.
#'   * `filename18pt.docx`: a Word document. If `add18 = TRUE` then a template
#'     Word document with 18pt bold text is used.
#'   * `filename.zip`: a zip file containing all the files produced.
#' @return A list containing the following components:
#'   \item{files }{names of all the files created.}
#'   \item{zips }{names of all zip files created (if \code{zip = TRUE}).}
#' @seealso \code{\link{install_otp}} to install
#'   \href{https://github.com/cognidox/OfficeToPDF}{OfficeToPDF}.
#' @seealso \code{\link{rmd2word}}, \code{\link{rmd2ioslides}},
#'   \code{\link{rmd2slidy}}, \code{\link{rmd2html}}.
#' @examples
#' # Create documents from example.Rmd
#' got_hux <- requireNamespace("huxtable", quietly = TRUE)
#' got_flex <- requireNamespace("flextable", quietly = TRUE)
#' got_pandoc <- rmarkdown::pandoc_available("1.14")
#' got_all <- got_hux && got_flex && got_pandoc
#' # This example needs packages huxtable and flextable
#' # We pass pdf = FALSE because OfficeToPDF is needed to convert Word to PDF
#' # and this is only relevant on  Windows Operating System.
#' #
#' if (got_all) {
#'   ex_file <- system.file(package = "accessr", "examples", "example.Rmd")
#'   ex_file <- sub(".Rmd", "", ex_file)
#'   rmd2many(ex_file, params = list(hide = TRUE), pdf = FALSE, zip = TRUE)
#' }
#' @export
rmd2many <- function(x, outputs =  c("ioslides", "word"), slide_level = 1,
                    css = "black", add18 = TRUE, pdf = TRUE,
                    highlight = list(word = "monochrome", ioslides = NULL,
                                     slidy = NULL, html = NULL),
                    params = NULL, zip = TRUE, ...) {
  # A vector in which to store the file names
  files <- NULL
  # If ioslides presentations are required then create them
  if (is.element("ioslides", outputs)) {
    val <- accessr::rmd2ioslides(x, slide_level = slide_level, css = css,
                                 highlight = highlight$ioslides,
                                 params = params, zip = FALSE, pdf = pdf, ...)
    files <- c(files, val$files)
    # If pdf slides have been produced then append "slides" to the filename
    if (pdf) {
      filename <- tools::file_path_sans_ext(val$files)
      from <- paste0(filename, ".pdf")
      to <- paste0(filename, "_slides.pdf")
      file.rename(from = from, to = to)
      files <- c(files, to)
    }
  }
  # If slidy presentations are required then create them
  if (is.element("slidy", outputs) & !is.element("ioslides", outputs)) {
    val <- accessr::rmd2slidy(x, css = css, highlight = highlight$slidy,
                              params = params, zip = FALSE, pdf = pdf, ...)
    files <- c(files, val$files)
    # If pdf slides have been produced then append "slides" to the filename
    if (pdf) {
      filename <- tools::file_path_sans_ext(val$files)
      from <- paste0(filename, ".pdf")
      to <- paste0(filename, "_slidy_slides.pdf")
      file.rename(from = from, to = to)
      files <- c(files, to)
    }
  }
  # If html documents are required then create them
  if (is.element("html", outputs) & !is.element("ioslides", outputs)
      & !is.element("slidy", outputs)) {
    # If css = "black" the find the correct path to accessr's black.css file
    if (css == "black") {
      css <- system.file(package = "accessr", "examples", "black.css")
    }
    val <- accessr::rmd2html(x, params = params, zip = FALSE, pdf = pdf, ...)
    files <- c(files, val$files)
  }
  # If Word/PDF files are required then create them
  if (is.element("word", outputs)) {
    # Perhaps create an 18pt Word file
    # Use pdf = FALSE to avoid writing over the PDF version
    if (add18) {
      val <- accessr::rmd2word(x, doc = "18", pdf = FALSE,
                               highlight = highlight$word, params = params,
                               zip = FALSE, ...)
      # Append "18pt" to the filename
      filename <- tools::file_path_sans_ext(val$files)
      from <- paste0(filename, ".docx")
      to <- paste0(filename, "18pt.docx")
      file.rename(from = from, to = to)
      files <- c(files, to)
    }
    val <- accessr::rmd2word(x, pdf = pdf, highlight = highlight$word,
                             params = params, zip = FALSE,  ...)
    files <- c(files, val$files)
  }
  # If a zip file is required then create it
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("The 'zip' package is required. Please install it.",
         call.= FALSE)
  }
  # Identify the different directories in x
  dnames <- dirname(files)
  # Unique directories
  udnames <- unique(dirname(files))
  # Directory identifiers for the files
  which_dir <- charmatch(x = dnames, table = udnames)
  # Create zip file(s), if required
  if (is.character(zip)) {
    zipfile <- rep_len(zip, length(udnames))
    zip <- TRUE
  } else if (is.logical(zip) && zip) {
    zipfile <- rep_len(basename(x), length(udnames))
  }
  # Function to create a zip file for files in a given directory
  zip_fun <- function(i) {
    zipname <- paste0(dnames[i], "/", zipfile[i], ".zip")
    which_files <- files[which_dir == i]
    res_zip <- zip::zip(zipfile = zipname, files = which_files,
                        mode = "cherry-pick")
    return(res_zip)
  }
  # Create the zip file(s) if required
  if (zip) {
    res_zip <- sapply(unique(which_dir), zip_fun)
    val <- list(files = files, zips = res_zip)
  } else {
    val <- list(files = files)
  }
  return(val)
}
