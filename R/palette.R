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
#' palette_kth(5)  # return the primary color in the KTH palette
palette_kth <- function(n = 10, name = "KTH", type = c("qual", "seq", "div")) {

  if (name != "KTH")
    stop("Please use RColorBrewer::brewer.pal() for non-KTH palettes")

  p100 <- c(
    blue = rgb(25, 84, 166, maxColorValue = 255),
    lightblue = rgb(36, 160, 216, maxColorValue = 255),
    cerise = rgb(216, 84, 151, maxColorValue = 255),
    olive = rgb(176, 201, 43, maxColorValue = 255),
    # Cool Grey 10 suggested by Martin Krzywinski (see http://mkweb.bcgsc.ca/colorblind)
    gray = rgb(99, 102, 106, maxColorValue = 255)
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

hex_to_rgb <- function(hexcol) {

  if (stringr::str_detect(toupper(hexcol), "#[0-9A-F]{6}$"))
    hexcol <- sprintf("%sFF", hexcol)

  if (!stringr::str_detect(toupper(hexcol), "#[0-9A-F]{8}$"))
    stop("Please provide a valid color hex, got: ", hexcol)

  rgb <- col2rgb(col = hexcol, alpha = TRUE)

  dplyr::tibble(
    red = rgb[ ,1]["red"] %>% as.integer(),
    green = rgb[ ,1]["green"] %>% as.integer(),
    blue = rgb[, 1]["blue"] %>% as.integer(),
    alpha = rgb[, 1]["alpha"] %>% as.integer()
  )
}

#' KTH Digital color palette with 5 qualitative colors
#'
#' This palette is taken from https://intra.kth.se/administration/kommunikation/grafiskprofil/profilfarger-1.845077
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
#' @importFrom purrr pmap_chr map_df map_chr
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_detect str_extract
#' @importFrom dplyr mutate arrange select
#' @export
#' @examples
#' palette_kth_digital(10)  # return the primary color in the KTH palette
palette_kth_digital <- function(n = 10, name = "KTH Digital", type = c("qual", "seq", "div")) {

  hexz <- c("#1954A6", "#007FAE", "#D02F80", "#528400", "#65656C")
  namez <- c("blue", "lightblue", "cerise", "olive", "gray")

  kthd <-
    hexz %>% map_df(hex_to_rgb) %>%
    mutate(rgb = pmap_chr(list(red, green, blue),
                          .f = function(r, g, b) rgb(r, g, b, maxColorValue = 255))) %>%
    mutate(rgb80 = ftg(rgb, 80), rgb60 = ftg(rgb, 60),
           rgb40 = ftg(rgb, 40), rgb20 = ftg(rgb, 20)) %>%
    mutate(color = namez)

  q <-
    kthd %>% select(color, rgb, rgb40, rgb80) %>%
    tidyr::pivot_longer(cols = -color) %>%
    mutate(name2 = paste0(color, ifelse(stringr::str_detect(name, "\\d{2}"), stringr::str_extract(name, "\\d{2}"), "")))

  q <- arrange(q, 1)

  qual <- setNames(q$value, nm = q$name2)

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

#' Show palette
#'
#' @param colours a character vector of colours
#' @param labels boolean, whether to show the hexadecimal representation of the colours in each tile
#' @param borders colour of the borders of the tiles; matches the border argument of graphics::rect(). The default means par("fg"). Use border = NA to omit borders.
#' @param cex_label size of printed labels, works the same as cex parameter of plot()
#' @param ncol number of columns, defaults to the matching the number of colors provided
#' @param nrow number of rows, defaults to 1
#' @importFrom graphics par plot rect text
#' @export
show_pal <- function (colours, labels = TRUE, borders = NULL, cex_label = 1, ncol = length(colours), nrow = 1)
{
  n <- length(colours)
  colours <- c(colours, rep(NA, nrow * ncol - length(colours)))
  colours <- matrix(colours, ncol = ncol, byrow = TRUE)
  old <- par(pty = "m", mar = c(0, 0, 0, 0))
  on.exit(par(old))
  plot(c(0, ncol), c(0, -nrow), type = "n", xlab = "", ylab = "",
       axes = FALSE)
  rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours),
       col = colours, border = borders)
  if (labels) {
    hcl <- farver::decode_colour(colours, "rgb", "hcl")
    label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
    text(col(colours) - 0.5, -row(colours) + 0.5, colours,
         cex = cex_label, col = label_col)
  }
}

#' KTH color palette information for KTH standard color palette
#'
#' This function aligns its signature r RColorBrewer::color.pal.info()
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

#' KTH color palette information for KTH Digital color palette
#'
#' This function aligns its signature with RColorBrewer::color.pal.info()
#'
#' @return data frame with information about the available palette(s)
#' @export
palette_kth_digital_info <- function() {
  data.frame(
    palette_name = rep("KTH Digital", 3),
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
