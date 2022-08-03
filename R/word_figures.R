# ================================= ext_img ===================================

#' External images for Word output
#'
#' This is a modified version of \code{\link[officer]{external_img}} for use in
#' \code{\link{rmd2word}}.  The modification is to allow the use of the
#' \code{\link[knitr]{knitr}} chunk options \code{out.width} and/or
#' \code{out.height} to set the dimensions of a figure (R-generated or
#' external image) when the output format is a Word document.  This
#' functionality is not normally available.
#'
#' @param src image file path
#' @param width width in inches
#' @param height height in inches
#' @param alt alternative text for images
#' @param ref_docx_dim A list containing the components \code{page} (a named
#'   numeric vector containing the \code{width} and \code{height}),
#'   \code{landscape} (a logical vector) and \code{margins} (a named numeric
#'   vector containing the \code{top}, \code{bottom}, \code{left},
#'   \code{right}, \code{header} and \code{footer} margins). See
#'   \strong{Details}.
#' @details If in the current R code chunk \code{out.width} or
#'   \code{out.height} have been set then the corresponding values of
#'   \code{fig.width} and \code{fig.height} are inferred from the dimensions
#'   of the figure and the page dimensions of the output Word document.
#'   The former are obtained using \code{\link[png]{readPNG}} or
#'   \code{\link[jpeg]{readJPEG}} as appropriate.  If only one of
#'   \code{out.width} or \code{out.height} are set then the aspect ratio of the
#'   figure is preserved.
#'
#'   \code{ref_docx_dim} can be produced using
#'   \code{officer::docx_dim(officer::read_docx(doc))}, where \code{doc} is an
#'   \code{rdocx} object and \code{\link[officer]{docx_dim}}
#'   and \code{\link[officer]{read_docx}} are functions in the
#'   \code{\link{officer}} package.
#' @note This function has been designed for use inside \code{\link{rmd2word}}
#'   but the object returned has the same structure as that returned from
#'   \code{\link[officer]{external_img}}. Outside the context of a
#'   \code{knitr} R code chunk the outputs from \code{external_img} and
#'   \code{ext_img} are identical.
#' @return An object of class \code{c("external_img", "cot", "run")} with
#'   attributes \code{"dims"} (a named list containing the figure's
#'   \code{width} and \code{height}) and \code{"alt"} (a character scalar
#'   containing the alternative text for the figure).
#' @examples
#' # Example from the officer::external_img() documentation
#' # Wrap R logo with external_img ----
#' srcfile <- file.path( R.home("doc"), "html", "logo.jpg" )
#' extimg <- external_img(src = srcfile, height = 1.06/2,
#'                        width = 1.39/2)
#' @export
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
