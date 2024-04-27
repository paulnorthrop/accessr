#' Install OfficeToPDF
#'
#' Downloads \code{OfficeToPDF.exe} from
#' \url{https://github.com/cognidox/OfficeToPDF/releases} and copies it to
#' a suitable location, by default `system.file(package = "accessr")`.
#'
#' @param dir Directory into which to download the executable file
#'   \code{OfficeToPDF.exe}. **This argument must be supplied**. Pass
#'   `dir = "accessr"` to choose the `accessr` package directory, that is,
#'   \code{system.file(package = "accessr")}. This choice should enable
#'   \code{OfficeToPDF.exe} to be found but there may be a problem if
#'   \code{\link{.libPaths}} refers to a cloud storage directory, such as
#'   OneDrive.
#' @param url URL from which to download \code{OfficeToPDF.exe}.  If this is
#'   missing then
#'   \url{https://github.com/cognidox/OfficeToPDF/releases/download/v1.9.0.2/OfficeToPDF.exe}
#'   is used.
#' @param method Passed to \code{\link[utils]{download.file}}.  If
#'   missing then \code{getOption("download.file.method")} is used to set it.
#' @param quiet Passed to \code{\link[utils]{download.file}}. \code{quiet}
#'   controls whether messages are printed during the download.
#' @param ... Additional arguments passed to
#'   \code{\link[utils]{download.file}}.
#' @details The argument \code{mode = "wb"} to
#'   \code{\link[utils]{download.file}} is hard-coded, so that binary transfer
#'   is forced.
#' @note \code{OfficeToPDF.exe} is used to create PDF files from Word
#'   documents in \code{\link{rmd2word}}.
#' @return See the \strong{Value} section of \code{\link[utils]{download.file}}.
#' @examples
#' # If dir is not supplied install_otp stops and dir = "accessr" is suggested
#' install_otp()
#' @export
install_otp <- function(dir, url, method, quiet = TRUE, ...) {
  if (missing(dir)) {
    stop("You must supply ''dir''. Perhaps choose dir = ''accessr''.")
  }
  if (dir == "accessr") {
    dir <- system.file(package = "accessr")
  }
  if (missing(url)) {
    url <- "https://github.com/cognidox/OfficeToPDF/releases/download/v1.9.0.2"
    url <- paste0(url, "/OfficeToPDF.exe")
  }
  dir <- paste0(dir, "/OfficeToPDF.exe")
  if (missing(method)) {
    method <- getOption("download.file.method")
  }
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("The 'utils' package is required. Have you removed it?",
         call. = FALSE)
  }
  val <- utils::download.file(url = url, destfile = dir, method = method,
                              quiet = quiet, mode = "wb", ...)
  if (val != 0) {
    warning("utils::download.file() returned a non-zero (failure) code")
  }
  return(invisible(val))
}
