% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install_otp.R
\name{install_otp}
\alias{install_otp}
\title{Install OfficeToPDF}
\usage{
install_otp(dir, url, method, quiet = TRUE, ...)
}
\arguments{
\item{dir}{Directory into which to download the \code{OfficeToPDF.exe}.
If \code{dir} is missing then \code{system.file(package = "accessr")} is
used. This should enable \code{OfficeToPDF.exe} to be found but
there may be a problem if \code{\link{.libPaths}} refers to a cloud
storage directory, such as OneDrive.}

\item{url}{URL from which to download \code{OfficeToPDF.exe}.  If this is
missing then
\url{https://github.com/cognidox/OfficeToPDF/releases/download/v1.9.0.2/OfficeToPDF.exe}
is used.}

\item{method}{Passed to \code{\link[utils]{download.file}}.  If
missing then \code{getOption("download.file.method")} is used to set it.}

\item{quiet}{Passed to \code{\link[utils]{download.file}}. \code{quiet}
controls whether messages are printed during the download.}

\item{...}{Additional arguments to be passed to
\code{\link[utils]{download.file}}.}
}
\value{
See the \strong{Value} section of \code{\link[utils]{download.file}}.
}
\description{
Downloads the executable file \code{OfficeToPDF.exe} from
\url{https://github.com/cognidox/OfficeToPDF/releases}.
}
\details{
The argument \code{mode = "wb"} to
  \code{\link[utils]{download.file}} is hard-coded, so that binary transfer
  is forced.
}
\note{
\code{OfficeToPDF.exe} is used to create PDFs files from Word
  documents in \code{\link{rmd2word}}.
}
\examples{
install_otp()
}
