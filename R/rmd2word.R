# ================================ rmd2word ===================================

#' Converts R markdown code to Word and PDF documents
#'
#' Creates accessible PDF documents by creating Word documents using
#' R markdown code and then producing PDF files.  Zip archives of the PDF files
#' may be created.
#'
#' @param x A character vector containing the names (no extension) of the
#'   \code{.Rmd} files to convert  if they are in the current working
#'   directory, or paths to the files, either absolute or relative to the
#'   current working directory, e.g., \code{DIRECTORY/file1}.  The \code{.docx}
#'   and \code{.pdf} files are created in the same directory as their
#'   respective \code{.Rmd} file.  If \code{x} is missing then a PDF file is
#'   created from each of the \code{.Rmd} files in the current working
#'   directory.
#' @param doc An optional character vector to be passed as the argument
#'   \code{reference_docx} to \code{\link[rmarkdown]{word_document}} to
#'   identify a template Word document, or respective documents for the files
#'   in \code{x}, from which the fonts, margins etc in the output Word
#'   documents will based.  If \code{doc} is not supplied then a default is
#'   used.  Different templates may be used for different files.
#'   \code{rep_len(doc, length(x))} is used to force \code{length(doc)} to have
#'   the same length as \code{x}. A component equal to \code{"default"} may be
#'   used to use the default Word template. See \strong{Details} for
#'   information about how to identify the locations of the Word template
#'   files.
#' @param dir A path to the directory in which the file \code{officetopdf.exe}
#'   sits.  This is not needed if this file sits in the current working
#'   directory or a directory in the list returned by \code{searchpaths()}.
#'   Otherwise, it may be a path relative to the current working directory
#'   or an absolute path.
#' @param zip A logical scalar or character vector indicating whether PDF
#'   files should be put into a zip archive.  If \code{zip = FALSE} then no
#'   zip archive is created.  Otherwise, an archive is created in each unique
#'   directory involved in \code{x}.  If \code{zip = TRUE} then any archive
#'   created has the name \code{accessr.zip}.  If \code{zip} is a character
#'   vector of zip file names (no extension) then these names are used to name
#'   the zip archives.  The names are recycled to the length of the number
#'   of unique directories if necessary.
#' @param add A logical scalar that determines what happens if the output
#'   zip file already exists.  If \code{add = TRUE} then files are added to the
#'   zip file and if \code{add = FALSE} then the zip file is deleted and will
#'   only contain newly-created files.
#' @param rm_word A logical scalar.  If \code{rm_word = TRUE} then all the Word
#'   files created are deleted.  Otherwise, they are not deleted.
#' @param rm_pdf A logical scalar.  If \code{rm_pdf = TRUE} and a zip archive
#'   of PDF files is produced then the individual PDF files are deleted.
#'   Otherwise, they are not deleted.
#' @param ... Additional arguments to be passed to
#'   \code{\link[rmarkdown]{word_document}}.
#' @details The simplest setup is to have the \code{.Rmd} files and the Word
#'   template (if used) and \code{officetopdf.exe} in the current working
#'   directory.
#'
#'   It is possible to have the \code{.Rmd} files in different
#'   directories, but any non-\code{"default"} values in \code{doc} must be
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
#'   A warning will be given if any of the PDF files could not be produced.
#'   This will occur if there is an existing PDF file of the same name open
#'   in another application.
#' @return A vector containing the values returned from \code{\link{system}}
#'   is returned invisibly. Note that if \code{wait = FALSE} then these values
#'   will be 0 (the success value) even if some of the PDF files could not be
#'   produced.  The error code 17234 indicates that a PDF file was open in
#'   another application.
#' @examples
#' \dontrun{
#' # All files in the current working directory
#' rmd2word(c("file1", "file2"), doc = "template.docx")
#' }
#' @export
rmd2word <- function(x, doc, dir, zip = TRUE, add = FALSE, quiet = TRUE,
                     rm_word = FALSE, rm_pdf = FALSE, ...) {
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
  # Path to the officetopdf executable
  if (missing(dir)) {
    exefile <- "officetopdf.exe"
  } else {
    exefile <- paste0(dir, "/officetopdf.exe")
  }
  # If no template word document has been supplied then use the default
  if (missing(doc)) {
    doc <- "default"
  }
  # Make doc the same length as x
  lenx <- length(x)
  doc <- rep_len(doc, lenx)
  # Function for Rmd to Word to PDF
  render_fun <- function(i) {
    # Convert .Rmd file to a Word document
    rmarkdown::render(input = rmd_files[i], output_format =
                        rmarkdown::word_document(reference_docx = doc[i], ...),
                      quiet = quiet)
#    rmarkdown::pandoc_convert(input = rmd_files[i], to = "docx")
    # Convert Word document to PDF document
    system(paste(exefile, word_files[i], pdf_files[i]))
  }
  res <- sapply(1:lenx, render_fun)
  # Error codes
  # 127 officetopdf.exe could not be found
  # 17234 file open in another application
  if (any(res == 127)) {
    stop("officetopdf.exe could not be found")
  }
  if (any(res != 0)) {
    warning(pdf_files[res != 0], " could not be written")
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
    # Directory identifiers for the files
    which_dir <- charmatch(dnames, udnames)
    for (i in unique(which_dir)) {
      # Set the directory and filename
      d <- dnames[which(which_dir == i)]
      f <- basename(x[which(which_dir == i)])
      zipname <- paste0(d[1], "/", zipfile[i], ".zip")
      if (!add) {
        if (file.exists(zipname)) {
          file.remove(zipname)
        }
      }
      utils::zip(zipname, paste0(f, ".pdf"))
    }
    if (rm_pdf) {
      sapply(pdf_files, file.remove)
    }
  }
  invisible(res)
}

