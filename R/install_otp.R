#' Install OfficeToPDF
#'
#' Downloads the executable file \code{OfficeToPDF.exe} from
#' \url{https://github.com/cognidox/OfficeToPDF/releases}
#'
#' @param dir Directory into which to download the \code{OfficeToPDF.exe}.
#' @param url URL from which to download \code{OfficeToPDF.exe}.  If this is
#'   missing then
#'   \url{https://github.com/cognidox/OfficeToPDF/releases/download/v1.9.0.2/OfficeToPDF.exe}
#'   is used.
#' @param method,quiet Passed to \code{\link[utils]{download.file}}.  If
#'   \code{method} is missing then it is inferred using
#'   \code{getOption("download.file.method")}.
#' @param ... Additional arguments to be passed to
#'   \code{\link[utils]{download.file}}.
#' @details \code{mode = "wb"} is used so that binary transfer is used.
#' @note \code{OfficeToPDF.exe} is used to create PDFs files from Word
#'   documents in \code{\link{rmd2word}}.
#' @return See the \strong{Value} section of \code{\link[utils]{download.file}}.
#' @seealso \code{\link[utils]{download.file}}, \code{\link{rmd2word}}.
#' @export
install_otp <- function(dir, url, method, quiet = TRUE, ...) {
  if (missing(dir)) {
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
  val <- utils::download.file(url = url, destfile = dir, method = method,
                              quiet = quiet, mode = "wb", ...)
  if (val != 0) {
    warning("")
  }
  return(invisible(val))
}
