# ================================ rmd2word ===================================

#' Converts R markdown code to Word and PDF documents
#'
#' Creates Word documents from input R markdown documents. On a Windows
#' Operating System, accessible PDF documents may be created from these Word
#' files if the software
#' \href{https://github.com/cognidox/OfficeToPDF}{OfficeToPDF} is installed.
#' The convenience function \code{\link{install_otp}} can be used to install
#' this software. Zip archives of the Word and/or PDF files may be created.
#'
#' @param x A character vector containing the names (\strong{no extension}) of
#'   the \code{.Rmd} files to convert  if they are in the current working
#'   directory, or paths to the files, either absolute or relative to the
#'   current working directory, e.g., \code{DIRECTORY/file1}.  The \code{.html}
#'   files are created in the same directory as their respective \code{.Rmd}
#'   file.  If \code{x} is missing then an html file is created from each of
#'   the \code{.Rmd} files in the current working directory.
#' @param doc An optional character vector (\strong{including the file
#'   extension}) to specify template Word documents on which to base the style
#'   of the respective output Word documents. This determines what is passed as
#'   the argument \code{reference_docx} to
#'   \code{\link[rmarkdown]{word_document}}, via
#'   \code{officedown::rdocx_document}.
#'   Different templates may be used for different files.
#'   \code{rep_len(doc, length(x))} is used to force \code{length(doc)} to have
#'   the same length as \code{x}. See \strong{Details} for some built-in
#'   options.
#' @param pdf A logical scalar.  Should \code{OfficeToPDF.exe} be used to
#'   create PDF files from the Word documents that are produced?  If
#'   \code{pdf = FALSE} then any zip archives created contain only Word files.
#'   PDF files will only be produced if the Operating System is
#'   \code{"windows"}, that is, \code{.Platform$OS.type == "windows"}.
#' @param dir A path to the directory in which the file \code{OfficeToPDF.exe}
#'   sits.  This is not needed if this file sits in the current working
#'   directory or a directory in the list returned by \code{searchpaths()}.
#'   Otherwise, it may be a path relative to the current working directory
#'   or an absolute path.  If \code{dir} is missing then \code{rmd2word} will
#'   look in \code{system.file(package = "accessr")}, which is the default
#'   installation location of \code{\link{install_otp}}.
#' @param zip A logical scalar or character vector indicating whether PDF
#'   files should be put into a zip archive.  If \code{zip = FALSE} then no
#'   zip archive is created.  Otherwise, an archive is created in each unique
#'   directory involved in \code{x}.  If \code{zip = TRUE} then any archive
#'   created has the name \code{accessr_word.zip}.  If \code{zip} is a
#'   character vector of zip file names (no extension) then these names are
#'   used to name the zip archives.  The names are recycled to the length of
#'   the number of unique directories if necessary.
#' @param add A logical scalar that determines what happens if the output
#'   zip file already exists.  If \code{add = TRUE} then files are added to the
#'   zip file and if \code{add = FALSE} then the zip file is deleted and will
#'   only contain newly-created files.
#' @param quiet A logical scalar.  Passed to \code{\link[rmarkdown]{render}} as
#'   the argument \code{quiet}.  The default, \code{quite = TRUE} suppresses
#'   all printing during rendering of the document.
#' @param rm_word A logical scalar.  If \code{rm_word = TRUE} then all the Word
#'   files created are deleted.  Otherwise, they are not deleted.
#' @param rm_pdf A logical scalar.  If \code{rm_pdf = TRUE} and a zip archive
#'   of PDF files is produced then the individual PDF files are deleted.
#'   Otherwise, they are not deleted.
#' @param inc_word A logical scalar.  If \code{inc_word = TRUE} then the Word
#'   files are included in the zip file created.  Otherwise, they are not
#'   included.
#' @param params A list of named parameters to pass as the argument
#'   \code{params} to \code{\link[rmarkdown]{render}}.
#' @param ... Additional arguments passed to
#'   \code{\link[rmarkdown]{word_document}}.
#' @details Information such as \code{title}, \code{author}, \code{lang} etc in
#'   the YAML header in the Rmd file are used but \code{output} is ignored.
#'
#'   The simplest setup is to have the \code{.Rmd} files in the current
#'   working directory, in which case `rmd2word()` will create Word documents
#'   from all these Rmd files, but the \code{.Rmd} files may be in different
#'   directories.
#'
#'   It is possible to have the \code{.Rmd} files in different
#'   directories, but any non-\code{"default"} values in \code{doc} must be
#'   such that the \code{reference_docx} argument of
#'   \code{\link[rmarkdown]{word_document}} finds a template Word file.
#'   If the template is in the same directory as its respective \code{.Rmd}
#'   component in  \code{x} then the filename, e.g. \code{"template.docx"} will
#'   suffice.  Otherwise, a path to the template should be given, either
#'   relative to the directory in which the \code{.Rmd} file sits, or an
#'   absolute path.
#'
#'   For information on how to create a template Word document see Richard
#'   Layton's guide
#'   \href{https://rmarkdown.rstudio.com/articles_docx.html}{Happy collaboration
#'     with Rmd to docx}.
#'
#'   There are some built-in template options:
#'
#'   * \code{doc = "officedown"}: uses
#'     \code{\link[officedown]{rdocx_document}}'s default,
#'   * \code{doc = "accessr"}: similar to \code{"officedown"} but with
#'       narrower margins and black text for titles and darker hyperlinks, to
#'       avoid contrast issues,
#'   * \code{doc = "18"}: like \code{"accessr"} but with 18pt text,
#'   * \code{doc = "exam"}: creates a Word file with a header
#'       "Examination paper for STAT0002" on the left and "Page x of n" on the
#'       right.
#'
#'   To use your own template(s), provide their filename(s).
#'   A component equal to \code{"officedown"} chooses
#'   \code{\link[officedown]{rdocx_document}}'s default.
#'   A component equal to \code{"accessr"} chooses \code{accessr}'s internal
#'   template file, which has narrower margins and darker blue fonts for titles
#'   and hyperlinks, to avoid contrast issues.  To use your own template(s),
#'   provide their filenames.  See \strong{Details} for more information.
#'
#'   The \code{\link[rmarkdown]{render}} function creates a Word file from
#'   each input \code{.Rmd} file.  Then
#'   \href{https://github.com/cognidox/OfficeToPDF}{OfficeToPDF} is used to
#'   convert the Word file to a PDF file.  The file \code{OfficeToPDF.exe}
#'   needs to be downloaded from the
#'   \href{https://github.com/cognidox/OfficeToPDF/releases}{OfficeToPDF
#'   releases} page and placed in the directory specified by the argument
#'   \code{dir}, or in a directory that is in the list returned by
#'   \code{\link{searchpaths}}.
#'   If \code{OfficeToPDF.exe} cannot be found then an error is thrown.
#'   A warning will be given if any of the PDF files could not be produced.
#'   This will occur if there is an existing PDF file of the same name open
#'   in another application.
#' @return In addition to creating the Word and PDF files, and perhaps zip
#'   files, a list containing the following vector components is returned
#'   invisibly:
#'   \item{error_codes }{If \code{pdf = TRUE}, numeric values returned from
#'   \code{\link{system}}.
#'   If \code{wait = FALSE} then these values will be 0 (the success value)
#'   even if some of the PDF files could not be produced.  The error code 17234
#'   indicates that a PDF file was open in another application.}
#'   \item{files }{(absolute) paths and file names of all files created.}
#'   \item{zips }{(relative) paths and names of all zip files created (if
#'     \code{zip = TRUE}).}
#' @references Layton, Richard. (2015) Happy collaboration with Rmd to docx.
#'   R Markdown from RStudio article.
#'   \url{https://rmarkdown.rstudio.com/articles_docx.html}
#' @seealso \code{\link{install_otp}} to install
#'   \href{https://github.com/cognidox/OfficeToPDF}{OfficeToPDF}.
#' @seealso \code{\link{rmd2many}}, \code{\link{rmd2ioslides}},
#'   \code{\link{rmd2slidy}}, \code{\link{rmd2html}} for other output formats.
#' @seealso The \href{https://paulnorthrop.github.io/accessr/}{accessr
#'   package page on Github}.
#' @examples
#' # Create a Word file from example.Rmd
#' got_hux <- requireNamespace("huxtable", quietly = TRUE)
#' got_flex <- requireNamespace("flextable", quietly = TRUE)
#' got_pandoc <- rmarkdown::pandoc_available("1.14")
#' got_all <- got_hux && got_flex && got_pandoc
#' # This example needs packages huxtable and flextable
#' # We pass pdf = FALSE because OfficeToPDF is needed to convert Word to PDF
#' # and this is only relevant on  Windows Operating System.
#' if (got_all) {
#'   ex_file <- system.file(package = "accessr", "examples", "example.Rmd")
#'   file.copy(ex_file, tdir <- tempdir(check = TRUE), overwrite = TRUE)
#'   ex_file <- list.files(tdir, pattern = "example.Rmd", full.names = TRUE)
#'   ex_file <- sub(".Rmd", "", ex_file)
#'   rmd2word(ex_file, pdf = FALSE)
#' }
#' @export
rmd2word <- function(x, doc = "accessr",
                     pdf = isTRUE(.Platform$OS.type == "windows"), dir,
                     zip = if (length(x) == 1 & !add) FALSE else TRUE,
                     add = FALSE,
                     quiet = TRUE, rm_word = FALSE, rm_pdf = FALSE,
                     inc_word = FALSE, params = NULL, ...) {
  # Warn that PDF files can only be produced on Windows
  if (pdf && isFALSE(.Platform$OS.type == "windows")) {
    warning("'pdf' has been set to FALSE because the OS is not ''windows''.")
    pdf <- FALSE
  }
  # Packages officer and officedown are required
  got_officer <- requireNamespace("officer", quietly = TRUE)
  got_officedown <- requireNamespace("officedown", quietly = TRUE)
  if (!got_officer && !got_officedown) {
    stop("Packages 'officer' and 'officedown' are needed. Please install them.",
         call.= FALSE)
  } else if (!got_officer) {
    stop("The 'officer' package is required. Please install it.",
         call.= FALSE)
  } else if (!got_officedown) {
    stop("The 'officedown' package is required. Please install it.",
         call.= FALSE)
  }
  # If x is missing then find all the .Rmd files in the working directory
  # x is the filepath without extension
  if (missing(x)) {
    rmd_files <- list.files(pattern = "Rmd")
    word_files <- sub(".Rmd", ".docx", rmd_files)
    pdf_files <- sub(".Rmd", ".pdf", rmd_files)
    x <- sub(".Rmd", "", rmd_files)
  } else if (length(x) == 1 && dir.exists(x)) {
    rmd_files <- list.files(x, pattern = "Rmd")
    rmd_files <- paste0(x, "/", rmd_files)
    x <- sub(".Rmd", "", rmd_files)
    word_files <- paste0(x, ".docx")
    pdf_files <- paste0(x, ".pdf")
  } else {
    rmd_files <- paste0(x, ".Rmd")
    word_files <- paste0(x, ".docx")
    pdf_files <- paste0(x, ".pdf")
  }
  # Path to the OfficeToPDF executable (only if pdf = TRUE)
  if (pdf) {
    if (missing(dir)) {
      dir <- system.file(package = "accessr")
      exefile <- paste0(dir, "/OfficeToPDF.exe")
    } else {
      exefile <- paste0(dir, "/OfficeToPDF.exe")
    }
  }
  # If doc contains any instances of "accessr" then set the correct path
  # to accessr's template.docx file
  accessr_doc_path <- system.file(package = "accessr", "examples",
                                  "template.docx")
  doc <- ifelse(doc == "accessr", accessr_doc_path, doc)
  # Do the same for any instances of "18" in doc
  accessr_18_path <- system.file(package = "accessr", "examples",
                                 "template18.docx")
  doc <- ifelse(doc == "18", accessr_18_path, doc)
  # Do the same for any instances of "exam" in doc
  accessr_exam_path <- system.file(package = "accessr", "examples",
                                   "exam.docx")
  doc <- ifelse(doc == "exam", accessr_exam_path, doc)
  # Do the same for any instances of "officedown" in doc
  officedown_path <- system.file(package = "officedown", "examples",
                                   "bookdown", "template.docx")
  doc <- ifelse(doc == "officedown", officedown_path, doc)
  # Make doc the same length as x
  lenx <- length(x)
  doc <- rep_len(doc, lenx)
  # Function for Rmd to Word to PDF
  docx_fun <- function(i) {
    # Convert .Rmd file to a Word document
    res0 <- officedown::rdocx_document(reference_docx = doc[i], ...)
    # Find the page width, height and margins of the reference Word document
    ref_docx_dim <- officer::docx_dim(officer::read_docx(doc[i]))
    find_line <- function(i, text_to_find, x){
      !is.na(pmatch(text_to_find, as.character(x[[i]])))
    }
    # Use a bespoke version of officer::external_img() to enable the chunk
    # option out.width to work
    plot_body <- body(res0$knitr$knit_hooks$plot)
    where_line <- vapply(1:length(plot_body), find_line, FALSE,
                         text_to_find = "external_img",
                         x = plot_body)
    line_to_modify <- which(where_line)
    body(res0$knitr$knit_hooks$plot)[[line_to_modify]] <-
      substitute(img <- accessr::ext_img(src = x[1], width = fig.width,
                                         height = fig.height,
                                         alt = options$fig.alt,
                                         ref_docx_dim = ref_docx_dim))
    # Render the Word file
    res1 <- rmarkdown::render(input = rmd_files[i], output_format = res0,
                              params = params, quiet = quiet)
    return(res1)
  }
  # Create Word documents
  files <- sapply(1:lenx, docx_fun)
  if (inc_word & pdf) {
    files <- c(files, sub(".docx", ".pdf", files))
  } else if (pdf) {
    files <- sub(".docx", ".pdf", files)
  }
  # Convert Word documents to PDF documents
  pdf_fun <- function(i) {
    # Convert Word document to PDF document
    res2 <- system(paste(exefile, word_files[i], pdf_files[i]))
    return(res2)
  }
  if (pdf) {
    error_codes <- sapply(1:lenx, pdf_fun)
    # Error codes
    # 127 OfficeToPDF.exe could not be found
    # 17234 file open in another application
    if (any(error_codes == 127)) {
      stop("OfficeToPDF.exe could not be found")
    }
    if (any(error_codes != 0)) {
      warning(pdf_files[error_codes != 0], " could not be written")
    }
  }
  # Identify the different directories in x
  dnames <- dirname(rmd_files)
  # Unique directories
  udnames <- unique(dirname(rmd_files))
  # Create zip file(s), if required
  if (is.character(zip)) {
    zipfile <- rep_len(zip, length(udnames))
    zip <- TRUE
  } else if (is.logical(zip) && zip) {
    zipfile <- rep_len("accessr_word", length(udnames))
  }
  if (zip) {
    if (pdf) {
      res_zip <- accessr_zip(x, dnames, udnames, zipfile, add,
                             extension = ".pdf")
      res <- list(error_codes = error_codes, files = files, zips = res_zip)
      if (rm_pdf) {
        sapply(pdf_files, file.remove)
      }
    } else {
      res <- list(files = files)
    }
    if (inc_word || !pdf) {
      if (pdf) {
        res_zip <- accessr_zip(x, dnames, udnames, zipfile, add = TRUE,
                               extension = ".docx")
        res <- list(error_codes = error_codes, files = files, zips = res_zip)
      } else {
        res_zip <- accessr_zip(x, dnames, udnames, zipfile, add = FALSE,
                               extension = ".docx")
        res <- list(files = files, zips = res_zip)
      }
    }
  } else {
    if (pdf) {
      res <- list(error_codes = error_codes, files = files)
    } else {
      res <- list(files = files)
    }
  }
  # Remove the Word files, if requested to do so
  if (rm_word) {
    sapply(word_files, file.remove)
  }
  return(invisible(res))
}

