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


