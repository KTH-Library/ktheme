#' Import fonts in the ktheme package user-wide or system-wide (on Linux)
#'
#' @param font_dst location for fonts, default is "~/.fonts" for user wide installation, use "/usr/local/share/fonts" for
#' system-wide (requires root privs)
#' @md
#' @export
install_fonts_linux <- function(font_dst = "~/.fonts") {
  if (!Sys.info()["sysname"] == "Linux")
    stop("Sorry, not implemented for installing on non-linux os:es (yet)")

  if (Sys.which("fc-cache") == "")
    stop("Couldn't find fc-cache installed on the system...")

  font_src <- system.file("fonts", package = "ktheme")

  stopifnot(all(dir.exists(font_dst), dir.exists(font_src)))

  message("Copying fonts from ", font_src, " to ", font_dst, "...")
  file.copy(from = font_src, to = font_dst, recursive = TRUE, overwrite = TRUE)

  message("Updating font cache in system")
  cmd <- sprintf("fc-cache -fv %s", font_dst)
  system(cmd)
}

#' Import fonts in the ktheme package user-wide or system-wide (on Mac OS)
#'
#' @param font_dst location for fonts, default is "~/Library/Fonts" for user wide installation, use "/Library/Fonts" for
#' system-wide (may require root privs)
#' @md
#' @export
#' @importFrom rlang .data
install_fonts_macos <- function(font_dst = "~/Library/Fonts") {
  if (!Sys.info()["sysname"] == "Darwin")
    stop("Sorry, this function is intended for Darwin os:es")

  if (Sys.which("fc-cache") == "")
    stop("Couldn't find fc-cache installed on the system...")

  font_src <- system.file("fonts", package = "ktheme")

  stopifnot(all(dir.exists(font_dst), dir.exists(font_src)))

  message("Copying fonts from ", font_src, " to ", font_dst, "...")
  #file.copy(from = font_src, to = font_dst, recursive = TRUE, overwrite = TRUE)
  if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
  list.files(font_src, recursive = TRUE) %>% paste(font_src,.data,sep = '/') %>% file.copy(.data,font_dst, overwrite = FALSE)

  message("Updating font cache in system")
  cmd <- sprintf("fc-cache -fv %s", font_dst)
  system(cmd)
  #may need to run this afterwards, as root user
  #extrafonts::ttf_import()
}

