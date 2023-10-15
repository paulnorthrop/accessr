#' Enable ioslides slide_level = 1
#'
#' Enables the argument \code{slide_level = 1} to
#' \code{\link[rmarkdown]{ioslides_presentation}} to set the slide separator as
#' the level one header #.
#'
#' @param one A logical scalar. If \code{one = TRUE} then a new Lua filter is
#' used that enables \code{slide_level = 1} to set the slide separator as
#' the level one header #. If \code{one = FALSE} then the original
#' \code{rmarkdown} version is reinstated.
#' @details The function \code{\link[rmarkdown]{ioslides_presentation}} has an
#' argument \code{slide_level} that sets the header level used as a slide
#' separator. The Lua filter \code{ioslides_presentation.lua} in
#' the \code{\link[rmarkdown]{rmarkdown}} package uses
#' any content between headers of level \code{slide_level} to create a segue
#' slide, which has a grey background and is intended only to contain a section
#' heading.
#'
#' In particular, under the default, \code{slide_level = 2}, content
#' between a level one header # and the next level two header ## is formatted
#' as a separate grey segue slide. If we do not want segue slides then we must
#' avoid using level one headers. However, for reasons of document
#' accessibility, we may want to use level one headers to separate slides.
#' For example, if we wish to create an ioslides presentation and a Word
#' document from the same source Rmd file then the Word document will only meet
#' fully accessibility requirements if the headings in the document start at
#' level one.
#'
#' In rmarkdown version 2.25, passing \code{slide_level= 1} to
#' \code{\link[rmarkdown]{ioslides_presentation}} does \strong{not} force a new
#' non-segue slide when a level one header # is used: it still attempts to
#' place all content between # and the next ## on a grey segue slide and the
#' behaviour content of the resulting slides is not desirable. Executing
#' the \code{ioslides_level} function replaces \code{rmarkdown}'s Lua filter
#' \code{ioslides_presentation.lua} with one that has been modified so that
#' passing \code{slide_level = 1} will have this effect. For values of
#' \code{slide_level} greater than or equal to 2
#' \code{\link[rmarkdown]{ioslides_presentation}} will behave as usual.
#' @return A logical scalar returned from \code{\link[=files]{file.copy}}.
#' @examples
#' ioslides_level()
#' @export
ioslides_level <- function(one = TRUE) {
  original <- paste0(system.file(package = "rmarkdown"),
                     "/rmd/ioslides/ioslides_presentation_original.lua")
  # If we have not previously saved the rmarkdown original then save it
  if (!file.exists(original)) {
    from <- paste0(system.file(package = "rmarkdown"),
                   "/rmd/ioslides/ioslides_presentation.lua")
    to <- original
    val <- file.copy(from = from, to = to, overwrite = TRUE)
  }
  # one = TRUE: replace the original Lua filter with a new one
  # one = FALSE: return to the rmarkdown original Lua filter
  if (one) {
    from <- system.file(package = "accessr", "ioslides",
                        "ioslides_presentation.lua")
    to <- paste0(system.file(package = "rmarkdown"),
                 "/rmd/ioslides/ioslides_presentation.lua")
  } else {
    from <- original
    to <- paste0(system.file(package = "rmarkdown"),
                 "/rmd/ioslides/ioslides_presentation.lua")
  }
  val <- file.copy(from = from, to = to, overwrite = TRUE)
  return(invisible(val))
}
