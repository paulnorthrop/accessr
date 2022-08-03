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
rmd2presentation <- function(x, format = c("ioslides", "slidy"), zip = TRUE,
                             pdf = FALSE, zip_pdf = zip, pdf_args = list(),
                             add = FALSE, quiet = TRUE, rm_html = FALSE,
                             rm_pdf = FALSE, ...) {
  format <- match.arg(format)
  # If x is missing then find all the .Rmd files in the working directory
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
  # Function for Rmd to ioslides or slidy
  if (format == "ioslides") {
    output_format <- rmarkdown::ioslides_presentation(...)
  } else {
    output_format <- rmarkdown::slidy_presentation(...)
  }
  render_fun <- function(i) {
    # Render the .Rmd file as an ioslides presentation
    rmarkdown::render(input = rmd_files[i],
                      output_format = output_format,
                      quiet = quiet)
  }
  res <- sapply(1:lenx, render_fun)
  res <- list(files = res)
  # Create pdf files, if required
  if (pdf) {
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
    zipfile <- rep_len(paste0("accessr_", format), length(udnames))
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
    zipfile <- rep_len(paste0("accessr_", format, "_pdf"), length(udnames))
  }
  if (pdf && zip_pdf) {
    res_zip_pdf <- accessr_zip(x, dnames, udnames, zipfile, add,
                               extension = ".pdf")
    res <- c(res, list(pdf_zips = res_zip_pdf))
    if (rm_pdf) {
      sapply(pdf_files, file.remove)
    }
  }
  invisible(res)
}

#' @keywords internal
#' @rdname accessr-internal
accessr_zip <- function(x, dnames, udnames, zipfile, add, extension) {
  # Directory identifiers for the files
  which_dir <- charmatch(x = dnames, table = udnames)
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
      res_zip <- zip::zip(zipfile = zipname, files = files_to_add,
                          mode = "cherry-pick")
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
        res_zip <- zip::zip_append(zipfile = zipname, files = files_to_add,
                                   mode = "cherry-pick")
      } else {
        # If all the existing files are to be replaced then use zip::zip().
        # Otherwise, we need to unzip to store the files that need to remain
        # and then write these files and the new ones to a new zip.
        if (all(in_zip %in% f_ext)) {
          res_zip <- zip::zip(zipfile = zipname, files = files_to_add,
                              mode = "cherry-pick")
        } else {
          # Unzip zipname
          # Create a temporary directory into which to unzip files
          dir.create(tmp <- tempfile())
          dir.create(file.path(tmp, "mydir"))
          zip::unzip(zipfile = zipname, exdir = "mydir")
          old_files <- dir("mydir")
          to_stay <- setdiff(old_files, f_ext)
          if (length(to_stay) > 0) {
            to_stay <- paste0("mydir", "/", to_stay)
            res_zip <- zip::zip(zipfile = zipname, files = to_stay,
                                mode = "cherry-pick")
            res_zip <- zip::zip_append(zipfile = zipname, files = files_to_add,
                                       mode = "cherry-pick")
          } else {
            res_zip <- zip::zip(zipfile = zipname, files = files_to_add,
                                mode = "cherry-pick")
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
