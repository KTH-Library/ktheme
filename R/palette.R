#' Fade color towards light grey, reducing its intensity
#'
#' @param cols vector of rgb codes for colors
#' @param pct percentage value (100, 80, 60, 40, 20 or 0)
#' @importFrom scales show_col seq_gradient_pal
#' @importFrom stats setNames
#' @importFrom purrr map_chr
#' @examples
#' ftg(palette_kth()["blue"], 80)  # fade KTH blue to 80% intensity towards light gray
#' @noRd
ftg <- function(cols, pct) {
  stopifnot(pct %in% c(100, 80, 60, 40, 20, 0))
  steps <- seq(0, 1, length.out = 6)
  fade_to_gray <- function(col) seq_gradient_pal(col, "gray95")(steps)
  step <- (100 - pct) / 100
  i <- which(as.character(step) == as.character(steps))
  res <- purrr::map_chr(cols, function(x) fade_to_gray(x)[i])
  setNames(res, paste0(names(res), pct))
}

#' KTH color palette with 5 qualitative colors
#'
#' This palette is taken from https://intra.kth.se/en/administration/kommunikation/grafiskprofil/profilfarger-1.845077
#'
#' This function aligns its signature with RColorBrewer::color.pal()
#'
#' @param n number of colors to use (1..5), default is to return all five
#' @param name name of palette
#' @param type nature of data, default "qual" for qualitative or "seq" for
#' sequential or "div" for diverging
#' @return named vector with colors and hex codes
#' @importFrom grDevices rgb
#' @importFrom scales alpha show_col seq_gradient_pal
#' @importFrom stats setNames
#' @importFrom purrr map_chr
#' @export
#' @examples
#' palette_kth(1)  # return the primary color in the KTH palette
#' library(scales)
#' alpha(palette_kth(), 0.4)  # return light signature colors (40 % alpha)
#' show_col(alpha(palette_kth(), 0.8))  # return medium light 80% alpha palette
palette_kth <- function(n = 10, name = "KTH", type = c("qual", "seq", "div")) {

  if (name != "KTH")
    stop("Please use RColorBrewer::brewer.pal() for non-KTH palettes")

  p100 <- c(
    blue = rgb(25, 84, 166, maxColorValue = 256),
    lightblue = rgb(36, 160, 216, maxColorValue = 256),
    cerise = rgb(216, 84, 151, maxColorValue = 256),
    olive = rgb(176, 201, 43, maxColorValue = 256),
    gray = rgb(101, 101, 108, maxColorValue = 256)
  )



  p80 <- ftg(p100, 80) #alpha(p100, 0.8)
  #p80 <- setNames(p80, paste0(names(p100), "80"))

  p40 <- ftg(p100, 40) #alpha(p100, 0.4)
  #p40 <- setNames(p40, paste0(names(p100), "40"))

  qual <- c(p100, p40, p80)

  seq <- c(qual["blue"], qual["blue80"], ftg(qual["blue"], 60),
    qual["blue40"], ftg(qual["blue40"], 20))
  seq <- setNames(seq, paste0("blue", 1:5))

  div <- c(seq[1:3], qual["gray40"],
    qual["cerise40"], qual["cerise80"], qual["cerise"])
  div <- setNames(div, c(paste0("H", 3:1), "M", paste0("L", 1:3)))

  switch(match.arg(type),
    qual = ifelse(n %in% 1:15, p <- qual[1:n], stop("max 15 qualitative colors are available")),
    seq = ifelse(n %in% 1:5, p <- seq[1:n], stop("max 5 sequential colors are available")),
    div = ifelse(n %in% 1:7, p <- div[1:n], stop("max 7 diverging colors are available"))
  )

  p

}

#palette_kth(15)

#' KTH color palette information
#'
#' This function aligns its signature with RColorBrewer::color.pal.info()
#'
#' @return data frame with information about the available palette(s)
#' @export
palette_kth_info <- function() {
  data.frame(
    palette_name = rep("KTH", 3),
    maxcolors = c(15, 5, 7),
    type = c("qual", "seq", "div"),
    colorblind = FALSE
  )
}

#' A qualitative color palette with colors from KTH's graphical profile
#'
#' @export
#' @importFrom scales manual_pal
#' @examples
#' library(scales)
#' scales::show_col(kth_pal()(5))
kth_pal <- function() {
  pal <- palette_kth()
  names(pal) <- NULL
  manual_pal(pal)
}

#' Discrete color & fill scales based on the KTH palette
#'
#' See [kth_pal]().
#'
#' @md
#' @inheritDotParams ggplot2::discrete_scale -expand -position
#' @rdname scale_kth
#' @export
scale_colour_kth <- function(...) {
  discrete_scale("colour", "kth", kth_pal(), ...)
}

#' @export
#' @rdname scale_kth
scale_color_kth <- scale_colour_kth

#' @export
#' @rdname scale_kth
scale_fill_kth <- function(...) {
  discrete_scale("fill", "kth", kth_pal(), ...)
}
