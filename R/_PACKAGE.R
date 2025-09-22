#' Theme and Theme Components for ggplot2 for KTH's graphical profile
#'
#' The core themes: `theme_kth_neo` uses Figtree and `theme_kth` uses Open Sans.
#' There is an option `ktheme.loadfonts` which – if set to `TRUE` – will
#' call `extrafont::loadfonts()` to register non‑core fonts with R PDF &
#' PostScript devices.  On Windows the same function registers the fonts
#' with the Windows graphics device.
#'
#' @md
#' @name ktheme
#' @keywords internal
#' @import ggplot2 grid scales extrafont grDevices
#' @importFrom magrittr %>%
#' @importFrom gdtools set_dummy_conf
#' @import rmarkdown knitr htmltools
#' @importFrom tools file_path_sans_ext
NULL
