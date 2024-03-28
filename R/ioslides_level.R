#' Enable ioslides slide_level = 1
#'
#' Enables the argument \code{slide_level = 1} to
#' \code{\link[rmarkdown]{ioslides_presentation}} to set the slide separator as
#' the level one header #.
#'
#' @param one A logical scalar. If \code{one = TRUE} then a new Lua filter is
#' used that enables \code{slide_level = 1} to set the slide separator as
#' the level one header #. A modified \code{default.css} css file is also used
#' that adjusts the font sizes for the header levels accordingly, so that the
#' level one header has a larger font than the level two header. If
#' \code{one = FALSE} then the original \code{rmarkdown} versions of the Lua
#' filter and the css file are reinstated.
#'
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
#' \code{\link[rmarkdown]{ioslides_presentation}} will behave as usual, except
#' that the default font sizes will be altered owing to the new
#' \code{default.css} file. Therefore, it may be better to use
#' \code{ioslides(one = TRUE)} with an Rmd file in which level one headers are
#' used to separate slides.
#'
#' @return A logical scalar returned from \code{\link[=files]{file.copy}}.
#' @examples
#' ioslides_level()
#' @export
ioslides_level <- function(one = TRUE) {
  original_lua <- paste0(system.file(package = "rmarkdown"),
               "/rmd/ioslides/ioslides_presentation_original.lua")
  original_css <- paste0(system.file(package = "rmarkdown"),
               "/rmd/ioslides/ioslides-13.5.1/theme/css/default_original.css")
  original <- c(original_lua, original_css)
  # If we have not previously saved the original files then save them
  if (!file.exists(original_lua) | !file.exists(original_css)) {
    from_lua <- paste0(system.file(package = "rmarkdown"),
                       "/rmd/ioslides/ioslides_presentation.lua")
    from_css <- paste0(system.file(package = "rmarkdown"),
                       "/rmd/ioslides/ioslides-13.5.1/theme/css/default.css")
    from <- c(from_lua, from_css)
    to <- original
    val <- file.copy(from = from, to = to, overwrite = TRUE)
  }
  # one = TRUE: replace the original Lua filter and css with a new one
  # one = FALSE: return to the rmarkdown original Lua filter and css
  if (one) {
    from_lua <- system.file(package = "accessr", "ioslides",
                            "ioslides_presentation.lua")
    from_css <- system.file(package = "accessr", "ioslides",
                            "default.css")
    from <- c(from_lua, from_css)
    to_lua <- paste0(system.file(package = "rmarkdown"),
                     "/rmd/ioslides/ioslides_presentation.lua")
    to_css <- paste0(system.file(package = "rmarkdown"),
                     "/rmd/ioslides/ioslides-13.5.1/theme/css/default.css")
    to <- c(to_lua, to_css)
  } else {
    from <- original
    to_lua <- paste0(system.file(package = "rmarkdown"),
                     "/rmd/ioslides/ioslides_presentation.lua")
    to_css <- paste0(system.file(package = "rmarkdown"),
                     "/rmd/ioslides/ioslides-13.5.1/theme/css/default.css")
    to <- c(to_lua, to_css)
  }
  val <- file.copy(from = from, to = to, overwrite = TRUE)
  return(invisible(val))
}
