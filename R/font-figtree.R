#' Import Figtree font for use in charts
#'
#' @md
#' @note The location of the font directory is displayed. It is highly
#'   recommended that you install them on your system the same way you would any
#'   other font you wish to use in other programs.
#' @export
import_figtree <- function() {

  os_font_dir <- system.file("fonts", "figtree", package="ktheme")

  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      os_font_dir)
  )

}

#' @rdname Figtree
#' @md
#' @title Figtree font name R variable aliases
#' @description `font_figtree` == "`Figtree`"
#' @format length 1 character vector
#' @export
font_figtree <- "Figtree"
