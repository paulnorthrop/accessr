#' Internal accessr functions
#'
#' Internal accessr functions
#' @details
#' These functions are not intended to be called by the user.
#' @name accessr-internal
#' @keywords internal
NULL

#' @keywords internal
#' @rdname accessr-internal
accessr_zip <- function(x, dnames, udnames, zipfile, zipname, add, extension) {
  # Directory identifiers for the files
  which_dir <- charmatch(dnames, udnames)
  # Function to create a zip file
  zip_fun <- function(i) {
    # Set the directory and filename
    d <- dnames[which(which_dir == i)]
    f <- basename(x[which(which_dir == i)])
    zipname <- paste0(d[1], "/", zipfile[i], ".zip")
    zip_exists <- file.exists(zipname)
    files_to_add <- paste0(d[1], "/", f, extension)
    f_ext <- paste0(f, extension)
    # First deal with the cases where we are not adding to an existing zip
    if (!(add & zip_exists)) {
      # If the zip file exists then remove it, because add = FALSE
      if (zip_exists) {
        file.remove(zipname)
      }
      res_zip <- zip::zip(zipname, files_to_add, mode = "cherry-pick")
    } else {
      # If we get to here then add = TRUE and the zip already exists
      # zip::zip_append() could result in multiple files of the same name
      # We want to overwrite any existing files with new versions
      #
      # Which files are in the zip?
      in_zip <- zip::zip_list(zipname)$filename
      # Do we need to overwrite some files?
      overwrite <- any(f_ext %in% in_zip)
      # Which new files appear in files_to_add and in the zip
      if (!overwrite) {
        res_zip <- zip::zip_append(zipname, files_to_add, mode = "cherry-pick")
      } else {
        # If all the existing files are to be replaced then use zip::zip().
        # Otherwise, we need to unzip to store the files that need to remain
        # and then write these files and the new ones to a new zip.
        if (all(in_zip %in% f_ext)) {
          res_zip <- zip::zip(zipname, files_to_add, mode = "cherry-pick")
        } else {
          # Unzip zipname
          # Create a temporary directory into which to unzip files
          dir.create(tmp <- tempfile())
          dir.create(file.path(tmp, "mydir"))
          zip::unzip(zipname, exdir = "mydir")
          old_files <- dir("mydir")
          to_stay <- setdiff(old_files, f_ext)
          if (length(to_stay) > 0) {
            to_stay <- paste0("mydir", "/", to_stay)
            res_zip <- zip::zip(zipname, to_stay, mode = "cherry-pick")
            res_zip <- zip::zip_append(zipname, files_to_add, mode = "cherry-pick")
          } else {
            res_zip <- zip::zip(zipname, files_to_add, mode = "cherry-pick")
          }
          unlink("mydir", recursive = TRUE)
        }
      }
    }
    return(res_zip)
  }
  res_zip <- sapply(unique(which_dir), zip_fun)
  return(res_zip)
}

#' @keywords internal
#' @rdname accessr-internal
pns <- function(x, ...) {
  rmd2pdf(x, doc = "template.docx", dir = "C:/Users/Paul/", ...)
}

#' @keywords internal
#' @rdname accessr-internal
pnd <- function(x, ...) {
  rmd2pdf(x, doc = "template.docx",
          dir = "C:/Users/paul/Documents/R_PACKAGES/", ...)
}
