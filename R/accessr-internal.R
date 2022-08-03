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

#' @keywords internal
#' @rdname accessr-internal
ext_img <- function(src, width = 0.5, height = 0.2, alt = "", ref_docx_dim) {
  check_src <- all(grepl("^rId", src)) || all(file.exists(src))
  if (!check_src) {
    stop("src must be a string starting with 'rId' or an existing image filename")
  }
  class(src) <- c("external_img", "cot", "run")
  # If the chunk options out.width and/or out.height have been set then them/it
  # to set fig.width and fig.height.
  # To do this we need to find the dimensions of the
  #   * reference Word document (contained in the argument ref_docx_dim)
  #   * (png or jpg) image
  out_width <- knitr::opts_current$get("out.width")
  out_height <- knitr::opts_current$get("out.height")
  # Function to set the width and height
  find_fig_width_height <- function(imdim) {
    w <- ref_docx_dim$page["width"]
    h <- ref_docx_dim$page["height"]
    mleft <- ref_docx_dim$margins["left"]
    mright <- ref_docx_dim$margins["right"]
    mtop <- ref_docx_dim$margins["top"]
    mbottom <- ref_docx_dim$margins["bottom"]
    asp <- imdim[1] / imdim[2]
    if (!is.null(out_width) && !is.null(out_height)) {
      fw <- (w - mleft - mright) * out_width
      fh <- (h - mtop - mbottom) * out_height
    } else if (!is.null(out_width)) {
      fw <- (w - mleft - mright) * out_width
      fh <- fw * asp
    } else if (!is.null(out_height)) {
      fh <- (h - mtop - mbottom) * out_height
      fw <- fh / asp
    }
    val <- c(fw, fh)
    names(val) <- c("width", "height")
    return(val)
  }
  # Convert out_width and out_height to decimals (if they are non-NULL)
  if (!is.null(out_width)) {
    out_width <- as.numeric(substr(out_width, 1, nchar(out_width) - 1)) / 100
  }
  if (!is.null(out_height)) {
    out_height <- as.numeric(substr(out_height, 1, nchar(out_height) - 1)) / 100
  }
  # Only adjust the width and height if out_width and/or out_height are given
  if (!is.null(out_width) || !is.null(out_height)) {
    # Find the page dimensions of the reference document
    ext <- tools::file_ext(src)
    if (ext == "png") {
      imdim <- dim(png::readPNG(src))
      wh <- find_fig_width_height(imdim)
      width <- wh["width"]
      height <- wh["height"]
    } else if (ext == "jpg" || ext == "jpeg") {
      imdim <- dim(jpeg::readJPEG(src))
      wh <- find_fig_width_height(imdim)
      width <- wh["width"]
      height <- wh["height"]
    }
  }
  attr(src, "dims") <- list(width = width, height = height)
  attr(src, "alt") <- alt
  src
}
