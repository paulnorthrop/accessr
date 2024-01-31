#' Create content in multiple formats
#'
#' From a single R markdown file create HTML slides, PDF slides, Word
#' and PDF documents
#'
#' @param x A numeric vector. Contains the numbers of the lectures for which
#'   to create files. Must be a subset of `1:20`.
#' @param outputs A character vector. Specifies the output formats required.
#'   A subset of `c("word", "isoslides", "slidy", "html")`.
#' @param slide_level Passed to [`rmd2ioslides`][accessr::rmd2ioslides] via
#'   `...`. The default `slide_level = 1` means that a level one header #
#'   create a new non-segue slide for an ioslides presentation.
#' @param css The argument \code{css} to be passed to
#'   \code{\link[rmarkdown]{ioslides_presentation}}. If \code{css = "black"}
#'   then \code{accessr}'s css file \code{black.css} is used, which results in
#'   black text being used in the slides.
#' @param add18  A logical scalar. If `TRUE` then we also create Word documents
#'   with 18pt text.
#' @param pdf A logical scalar. If `TRUE` then we use
#'   [`chrome_print`][pagedown::chrome_print] to print `lecture?.html` to
#'   `lecture?slides.pdf`.
#' @param highlight A named list, with names a subset of
#'   `c("word", "isoslides", "slidy", "html")`, providing the respective syntax
#'   highlighting styles passed to Pandoc for the output formats. Any syntax
#'   highlighting provided in `css` will take precedence.
#' @param ... Additional arguments passed to
#'   [`rmd2ioslides`][accessr::rmd2ioslides],
#'   [`rmd2slidy`][accessr::rmd2slidy],
#'   [`rmd2word`][accessr::rmd2word] or
#'   [`rmd2html`][accessr::rmd2html].
#'
#'   For example, the argument `params` can passed to
#'   \code{\link[rmarkdown]{render}}.
#' @details The source `.Rmd` files must be in a directory called
#'   `lecture` in the current working directory.
#'
#' The default setting creates, for each valid value in `x`, the following
#' files
#'
#'   * `lecture?.html`: lecture slides is `ioslides` format.
#'   * `lecture?.pdf`: a PDF document containing the content of the lecture
#'     slides, produced from a Word file `lecture?.docx`.
#'   * `lecture?draft.pdf`: a version of `lecture?.pdf` in which selected
#'     content is removed.
#'   * `lecture?.docx`: a Word document containing the content of the lecture
#'     slides using an 18pt text template Word file. If `add18 = FALSE` then
#'     this is the Word file used to create `lecture?.pdf`.
#' @return Nothing is returned.
#' @examples
#' lecture(7:8)
#' @export
rmd2many <- function(x, outputs =  c("ioslides", "word"), slide_level = 1,
                    css = "black", add18 = TRUE, pdf = TRUE,
                    highlight = list(word = "monochrome", ioslides = NULL,
                                     slidy = NULL, html = NULL), ...) {
  # Create a vector containing the names of the source Rmd files
  rmd <- paste0("lectures/lecture", x)
  # If lecture slides are required then create them
  # Always pass hide = FALSE
  if (is.element("ioslides", outputs)) {
    val <- accessr::rmd2ioslides(rmd, slide_level = slide_level, css = css,
                                 highlight = highlight$ioslides,
                                 params = list(hide = FALSE), zip = FALSE,
                                 pdf = pdf, ...)
    # If pdf slides have been produced then rename to lecture?slides.pdf
    if (pdf) {
      filename <- tools::file_path_sans_ext(val$files)
      from <- paste0(filename, ".pdf")
      to <- paste0(filename, "slides.pdf")
      file.rename(from = from, to = to)
    }
  }
  # If Word/PDF lecture slide summaries are required then create them
  # Create versions with selected content hidden (hide = TRUE) and all content
  # (hide = FALSE)
  if (is.element("word", outputs)) {
    # hide = TRUE
    val <- accessr::rmd2word(rmd, highlight = highlight$word,
                             params = list(hide = TRUE), zip = FALSE, ...)
    # Copy ?.docx and ?.pdf files to draft?.docx and draft?.pdf
    from <- val$files
    to <- paste0(tools::file_path_sans_ext(from), "draft.",
                 tools::file_ext(from))
    file.rename(from = from, to = to)
    # hide = FALSE
    accessr::rmd2word(rmd, highlight = highlight$word,
                      params = list(hide = FALSE), zip = FALSE,  ...)
    # Perhaps add an 18pt Word file
    # Use pdf = FALSE to avoid writing over the PDF version
    if (add18) {
      accessr::rmd2word(rmd, doc = "18", pdf = FALSE,
                        highlight = highlight$word,
                        params = list(hide = FALSE), zip = FALSE, ...)
    }
  }
  # Move files to the relevant destination directory lecture?
  move <- function(number) {
    # Check that destination directory exist and create if not
    directory <- paste0("lectures/lec", number)
    if (!dir.exists(directory)) {
      dir.create(directory)
    }
    exts <- c("html", "docx", "pdf")
    files <- tools::list_files_with_exts("lectures", exts)
    files <- grep(paste0("lecture", number), files, value = TRUE)
    file.rename(from = files, to = paste0(directory, "/", basename(files)))
  }
  # Move the files, for each value in x
  sapply(x, move)
  return(invisible())
}
