#' Import Open Sans font for use in charts
#'
#' There is an option `ktheme.loadfonts` which -- if set to `TRUE` -- will
#' call `extrafont::loadfonts()` to register non-core fonts with R PDF & PostScript
#' devices. If you are running under Windows, the package calls the same function
#' to register non-core fonts with the Windows graphics device.
#'
#' @md
#' @note This will take care of ensuring PDF/PostScript usage. The location of the
#'   font directory is displayed after the base import is complete. It is highly
#'   recommended that you install them on your system the same way you would any
#'   other font you wish to use in other programs.
#' @export
import_georgia <- function() {

  georgia_font_dir <- system.file("fonts", "georgia", package="ktheme")

  suppressWarnings(suppressMessages(extrafont::font_import(georgia_font_dir, prompt=FALSE)))

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
