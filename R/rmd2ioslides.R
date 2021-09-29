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
#' @param zip A logical scalar or character vector indicating whether PDF
#'   files should be put into a zip archive.  If \code{zip = FALSE} then no
#'   zip archive is created.  Otherwise, an archive is created in each unique
#'   directory involved in \code{x}.  If \code{zip = TRUE} then any archive
#'   created has the name \code{accessr_ioslides.zip}.  If \code{zip} is a
#'   character vector of zip file names (no extension) then these names are
#'   used to name the zip archives.  The names are recycled to the length of
#'   the number of unique directories, if necessary.
#' @param add A logical scalar that determines what happens if the output
#'   zip file already exists.  If \code{add = TRUE} then files are added to the
#'   zip file and if \code{add = FALSE} then the zip file is deleted and will
#'   only contain newly-created files.
#' @param quiet Argument of the same name passed to
#'   \code{\link[rmarkdown]{render}} to determine what is printed during
#'   rendering from knitr.
#' @param ... Additional arguments to be passed to
#'   \code{\link[rmarkdown]{ioslides_presentation}}.
#' @details The simplest setup is to have the \code{.Rmd} files in the current
#'   working directory, but it is possible to have the \code{.Rmd} files in
#'   different directories.
#'
#'   The \code{\link[rmarkdown]{render}} function, with the argument
#'   \code{output_file =} \code{\link[rmarkdown]{ioslides_presentation}}
#'   creates the ioslides html files.
#' @return A character vector containing the paths of the output html files.
#' @examples
#' \dontrun{
#' # All files in the current working directory
#' rmd2ioslides(c("file1", "file2"))
#' }
#' @export
rmd2ioslides <- function(x, zip = TRUE, add = FALSE, quiet = TRUE, ...) {
  # If x is missing then find all the .Rmd files in the working directory
  if (missing(x)) {
    rmd_files <- list.files(pattern = "Rmd")
    x <- sub(".Rmd", "", rmd_files)
  } else if (length(x) == 1 && dir.exists(x)) {
    rmd_files <- list.files(x, pattern = "Rmd")
    rmd_files <- paste0(x, "/", rmd_files)
    x <- sub(".Rmd", "", rmd_files)
  } else {
    rmd_files <- paste0(x, ".Rmd")
  }
  # Make doc the same length as x
  lenx <- length(x)
  # Function for Rmd to ioslides
  fun <- function(i) {
    # Render the .Rmd file as an ioslides presentation
    rmarkdown::render(input = rmd_files[i],
                      output_format = rmarkdown::ioslides_presentation(...),
                      quiet = quiet)
  }
  res <- sapply(1:lenx, fun)
  # Identify the different directories in x
  dnames <- dirname(rmd_files)
  # Unique directories
  udnames <- unique(dirname(rmd_files))
  # Create zip file(s), if required
  if (is.character(zip)) {
    zipfile <- rep_len(zip, length(udnames))
    zip <- TRUE
  } else if (is.logical(zip) && zip) {
    zipfile <- rep_len("accessr_ioslides", length(udnames))
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
        zip::zip(zipname, paste0(d, "/", f, ".html"), mode = "cherry-pick")
      } else {
        zip::zip_append(zipname, paste0(d, "/", f, ".html"),
                        mode = "cherry-pick")
      }
    }
  }
  invisible(res)
}

