#' Import Open Sans font for use in charts
#'
#' @md
#' @note The location of the font directory is displayed. It is highly
#'   recommended that you install them on your system the same way you would any
#'   other font you wish to use in other programs.
#' @export
import_georgia <- function() {

  georgia_font_dir <- system.file("fonts", "georgia", package="ktheme")

  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      georgia_font_dir)
  )

}

#' @rdname Georgia
#' @md
#' @title Georgia font name R variable aliases
#' @description `font_os` == "`Georgia`"
#' @format length 1 character vector
#' @export
font_georgia <- "Georgia"
