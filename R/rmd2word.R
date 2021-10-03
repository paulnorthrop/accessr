# ================================ rmd2word ===================================

#' Converts R markdown code to Word and PDF documents
#'
#' Creates accessible PDF documents by creating Word documents using
#' R markdown code and then producing PDF files.  Zip archives of the PDF files
#' may be created.
#'
#' @param x A character vector containing the names (no extension) of the
#'   \code{.Rmd} files to convert if they are in the current working
#'   directory, or paths to the files, either absolute or relative to the
#'   current working directory, e.g., \code{DIRECTORY/file1}.  The \code{.docx}
#'   and \code{.pdf} files are created in the same directory as their
#'   respective \code{.Rmd} file.  If \code{x} is missing then a PDF file is
#'   created from each of the \code{.Rmd} files in the current working
#'   directory.
#' @param doc An optional character vector to specify template Word documents
#'   on which to base the style of the respective output Word documents. This
#'   determines what is passed as the argument\code{reference_docx} to
#'   \code{\link[rmarkdown]{word_document}}. Different templates may be used
#'   for different files. \code{rep_len(doc, length(x))} is used to force
#'   \code{length(doc)} to have the same length as \code{x}.A component equal
#'   to \code{"default"} specifies \code{word_document}'s default Word
#'   template. A component equal to \code{"accessr"} specifies \code{accessr}'s
#'   internal template file, which has narrower margins and darker blue fonts
#'   for titles and hyperlinks, to avoid contrast issues.  To use your own
#'   template(s), provide their filenames.  See \strong{Details} for
#'   more information.
#' @param dir A path to the directory in which the file \code{OfficeToPDF.exe}
#'   sits.  This is not needed if this file sits in the current working
#'   directory or a directory in the list returned by \code{searchpaths()}.
#'   Otherwise, it may be a path relative to the current working directory
#'   or an absolute path.
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
#' @param rm_word A logical scalar.  If \code{rm_word = TRUE} then all the Word
#'   files created are deleted.  Otherwise, they are not deleted.
#' @param rm_pdf A logical scalar.  If \code{rm_pdf = TRUE} and a zip archive
#'   of PDF files is produced then the individual PDF files are deleted.
#'   Otherwise, they are not deleted.
#' @param inc_word A logical scalar.  If \code{inc_word = TRUE} then the Word
#'   files are included in the zip file created.  Otherwise, they are not
#'   included.
#' @param ... Additional arguments to be passed to
#'   \code{\link[rmarkdown]{word_document}}.
#' @details Information such as \code{title}, \code{author}, \code{lang} etc in
#'   the YAML header in the Rmd file are used but \code{output} is ignored.
#'
#'   The simplest setup is to have the \code{.Rmd} files and the Word
#'   template (if used) and \code{OfficeToPDF.exe} in the current working
#'   directory.
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
#'   For information on how to create a template Word document
#'   \href{https://rmarkdown.rstudio.com/articles_docx.html}{Happy collaboration with Rmd to docx}.
#'
#'   The \code{\link[rmarkdown]{render}} function creates a Word file from
#'   each input \code{.Rmd} file.  Then
#'   \href{https://github.com/cognidox/OfficeToPDF}{OfficeToPDF} is used to
#'   convert the Word file to a PDF file.  The file \code{OfficeToPDF.exe}
#'   needs to be downloaded from the
#'   \href{https://github.com/cognidox/OfficeToPDF/releases}{OfficeToPDF releases}
#'   page and placed in the directory specified by the argument \code{dir}, or
#'   in a directory that is in the list returned by \code{\link{searchpaths}}.
#'   If \code{OfficeToPDF.exe} cannot be found then an error is thrown.
#'   A warning will be given if any of the PDF files could not be produced.
#'   This will occur if there is an existing PDF file of the same name open
#'   in another application.
#' @return A list containing the following vector components is returned
#'   invisibly:
#'   \item{error_codes }{numeric values returned from \code{\link{system}}.
#'   If \code{wait = FALSE} then these values will be 0 (the success value)
#'   even if some of the PDF files could not be produced.  The error code 17234
#'   indicates that a PDF file was open in another application.}
#'   \item{files }{(absolute) paths and file names of the files added to a zip
#'     file.}
#'   \item{zips }{(relative) paths and names of all the zip files.}
#' @references Layton, Richard. (2015) Happy collaboration with Rmd to docx.
#'   R Markdown from RStudio article.
#'   \url{https://rmarkdown.rstudio.com/articles_docx.html}
#' @examples
#' \dontrun{
#' # All files in the current working directory
#' rmd2word(c("TEST/file1", "TEST/file2"), doc = "template.docx")
#' }
#' @export
rmd2word <- function(x, doc = "accessr", dir, zip = TRUE, add = FALSE,
                     quiet = TRUE, rm_word = FALSE, rm_pdf = FALSE,
                     inc_word = FALSE, ...) {
  # If x is missing then find all the .Rmd files in the working directory
  if (missing(x)) {
    rmd_files <- list.files(pattern = "Rmd")
    word_files <- sub(".Rmd", ".docx", rmd_files)
    pdf_files <- sub(".Rmd", ".pdf", rmd_files)
    x <- sub(".Rmd", "", rmd_files)
  } else {
    rmd_files <- paste0(x, ".Rmd")
    word_files <- paste0(x, ".docx")
    pdf_files <- paste0(x, ".pdf")
  }
  # Path to the OfficeToPDF executable
  if (missing(dir)) {
    exefile <- "OfficeToPDF.exe"
  } else {
    exefile <- paste0(dir, "/OfficeToPDF.exe")
  }
  # If doc contains any instances of "accessr" then set the correct path
  # to accessr's template.docx file
  accessr_doc_path <- system.file(package = "accessr", "rmarkdown",
                                  "templates", "my_template", "template.docx")
  doc <- ifelse(doc == "accessr", accessr_doc_path, doc)
  # Make doc the same length as x
  lenx <- length(x)
  doc <- rep_len(doc, lenx)
  # Function for Rmd to Word to PDF
  docx_fun <- function(i) {
    # officedown::rdocx_document() has arguments that will take precedence
    # over some of the document properties of the argument reference_docx.
    # We use functions from the officer package to extract these properties
    # from the reference document and supply them as arguments to
    # officedown::rdoc_document()
    odoc <- officer::read_docx(doc[i])
    ddim <- officer::docx_dim(odoc)
    page_mar <- as.list(ddim$margins)
    page_size <- list(width = ddim$page["width"], height = ddim$path["height"],
                      orient = ifelse(ddim$landscape, "landscape", "portrait"))
    # Convert .Rmd file to a Word document
    res1 <- rmarkdown::render(input = rmd_files[i], output_format =
                       officedown::rdocx_document(
                         page_margins = do.call(officer::page_mar, page_mar),
                         page_size = do.call(officer::page_size, page_size),
                         reference_docx = doc[i], ...),
                       quiet = quiet)
    return(res1)
  }
  pdf_fun <- function(i) {
    # Convert Word document to PDF document
    res2 <- system(paste(exefile, word_files[i], pdf_files[i]))
    return(res2)
  }
  files <- sapply(1:lenx, docx_fun)
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
  # Remove the Word files, if requested to do so
  if (rm_word) {
    sapply(word_files, file.remove)
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
    res_zip <- accessr_zip(x, dnames, udnames, zipfile, zipname, add,
                           extension = ".pdf")
    res <- list(error_codes = error_codes, files = files, zips = res_zip)
    if (rm_pdf) {
      sapply(pdf_files, file.remove)
    }
    if (inc_word) {
      res_zip <- accessr_zip(x, dnames, udnames, zipfile, zipname, add = TRUE,
                             extension = ".docx")
    }
  }
  invisible(res)
}

