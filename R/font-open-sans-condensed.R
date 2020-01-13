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
import_open_sans_condensed <- function() {

  os_font_dir <- system.file("fonts", "open-sans-condensed", package="ktheme")

  suppressWarnings(suppressMessages(extrafont::font_import(os_font_dir, prompt=FALSE)))

  message(
    sprintf(
      "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
      os_font_dir)
  )

}

#' @rdname OpenSansCondensed
#' @md
#' @title Open Sans font name R variable aliases
#' @description `font_osc` == "`Open Sans Condensed`"
#' @format length 1 character vector
#' @export
font_osc <- "Open Sans Condensed"
