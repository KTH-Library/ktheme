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

hexes_neo <- function() {
  list(
    blue = "#004791", ## KTH-blå
    indigo = "#000061", # Marinblå
    purple = "#78001A", # Mörk tegel
    pink = "#FFCCC4", # Ljus tegel
    red = "#E86A58", # Tegel
    orange = "#A65900", # Mörkgul
    yellow = "#FFBE00", # Gul
    green = "#4DA061", # Grön
    teal = "#339C9C", # Turkos
    cyan = "#6298D2", # Himmelsblå
    sand = "#EBE5E0", # Sand
    lightblue = "#DEF0FF", # Ljusblå
    digitalblue = "#0029ED", # Digitalblå (undvik)
    darkteal = "#1C434C", # Mörkturkos
    darkred = "#78001A", # Mörk tegel
    darkyellow = "#A65900", # Mörkgul
    darkgray = "#323232", # Mörkgrå
    offblack = "#212121", # Bruten svart
    offwhite = "#FCFCFC", # Bruten vit
    darkblue = "#000061", # Marinblå
    darkgreen = "#0D4A21", # Mörkgrön
    lightgreen = "#C7EBBA", # Ljusgrön
    lightteal = "#B2E0E0", # Ljusturkos
    lightyellow = "#FFF080", # Ljusgul
    lightgray = "#E6E6E6", # Ljusgrå
    gray = "#A5A5A5", # Grå
    darkgray = "#323232" # Mörkgrå
  )
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
#' palette_kth_neo(5)  # return the primary color in the KTH palette
palette_kth_neo <- function(n = 10, name = "KTH", type = c("qual", "seq", "div")) {
  hexes <- hexes_neo()

  if (name != "KTH")
    stop("Please use RColorBrewer::brewer.pal() for non-KTH palettes")

  p100 <- c(
    blue = hexes$blue,
    lightblue = hexes$yellow,
    cerise = hexes$red,
    olive = hexes$green,
    teal = hexes$teal,
    # Cool Grey 10 suggested by Martin Krzywinski (see http://mkweb.bcgsc.ca/colorblind)
    gray = rgb(99, 102, 106, maxColorValue = 255)
  )

  p80 <- ftg(p100, 80)
  p40 <- ftg(p100, 40)
  qual <- c(p100, p40, p80)

#  seq <- c(qual["blue"], qual["blue80"], ftg(qual["blue"], 60),
#    qual["blue40"], ftg(qual["blue40"], 20))

  qual <- c(
    hexes$blue, hexes$green, hexes$teal, hexes$red, hexes$yellow, hexes$gray,
    hexes$lightblue, hexes$lightgreen, hexes$lightteal, hexes$pink, hexes$lightyellow, hexes$sand,
    hexes$darkblue, hexes$darkgreen, hexes$darkteal, hexes$purple, hexes$darkyellow, hexes$darkgray
  )
  qual <- setNames(qual, c(
    "blue", "green", "teal", "red", "yellow", "gray",
    "lightblue", "lightgreen", "lightteal", "lightyellow", "lightgray",
    "darkblue", "darkgreen", "darkteal", "darkred", "darkyellow", "darkgray")
  )

  seq <- c(
    hexes$darkblue, hexes$blue,
    hexes$cyan,
    hexes$lightblue, hexes$offwhite
  )
  seq <- setNames(seq, paste0("blue", 1:5))

  div <- c(
    seq[1:3], # H3:H1
    hexes$offwhite, # M
    hexes$pink, hexes$red, hexes$darkred #L1:L3
  )
  div <- setNames(div, c(paste0("H", 3:1), "M", paste0("L", 1:3)))

  switch(match.arg(type),
    qual = ifelse(n %in% 1:18, p <- qual[1:n], stop("max 18 qualitative colors are available")),
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
#' @param ncol number of columns, defaults to the matching the number of colours provided
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

#' KTH color palette information for KTH standard color palette
#'
#' This function aligns its signature r RColorBrewer::color.pal.info()
#'
#' @return data frame with information about the available palette(s)
#' @export
palette_kth_neo_info <- function() {
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

#' SDG color table from UN
#' @format A data frame with 17 rows and 4 variables:
#' \describe{
#'   \item{\code{goal}}{integer number for the SDG, 1-17}
#'   \item{\code{color}}{character RGB color}
#'   \item{\code{short_desc}}{character Short description of SDG}
#'   \item{\code{long_desc}}{character Long description of SDG}
#'}
#' @importFrom dplyr tibble
#' @export
sdg_colors <- function() {
  # from https://github.com/CMUSustainability/SDGmapR/blob/main/data-raw/DATASET.R
  tibble(
    goal = c(1:17),
    color = c(
      '#e5243b', '#DDA63A', '#4C9F38', '#C5192D', '#FF3A21', '#26BDE2',
      '#FCC30B', '#A21942', '#FD6925', '#DD1367', '#FD9D24', '#BF8B2E',
      '#3F7E44', '#0A97D9', '#56C02B', '#00689D', '#19486A'
    ),
    short_desc = c(
      "No Poverty", "Zero Hunger", "Good Health and Well-being",
      "Quality Education", "Gender Equality", "Clean Water and Sanitation",
      "Affordable and Clean Energy", "Decent Work and Economic Growth",
      "Industry, Innovation and Infrastructure", "Reducing Inequality",
      "Sustainable Cities and Communities",
      "Responsible Consumption and Production", "Climate Action",
      "Life Below Water", "Life On Land",
      "Peace, Justice, and Strong Institutions",
      "Partnerships for the Goals"
    ),
    long_desc = c(
      "End poverty in all its forms everywhere",
      "End hunger, achieve food security and improved nutrition and promote sustainable agriculture",
      "Ensure healthy lives and promote well-being for all at all ages",
      "Ensure inclusive and equitable quality education and promote lifelong learning opportunities for all",
      "Achieve gender equality and empower all women and girls",
      "Ensure availability and sustainable management of water and sanitation for all",
      "Ensure access to affordable, reliable, sustainable and modern energy for all",
      "Promote sustained, inclusive and sustainable economic growth, full and productive employment and decent work for all",
      "Build resilient infrastructure, promote inclusive and sustainable industrialization and foster innovation",
      "Reduce inequality within and among countries",
      "Make cities and human settlements inclusive, safe, resilient and sustainable",
      "Ensure sustainable consumption and production patterns",
      "Take urgent action to combat climate change and its impacts",
      "Conserve and sustainably use the oceans, seas and marine resources for sustainable development",
      "Protect, restore and promote sustainable use of terrestrial ecosystems, sustainably manage forests, combat desertification, and halt and reverse land degradation and halt biodiversity loss",
      "Promote peaceful and inclusive societies for sustainable development, provide access to justice for all and build effective, accountable and inclusive institutions at all levels",
      "Strengthen the means of implementation and revitalize the global partnership for sustainable development"
    )) %>%
    mutate(color = toupper(.data$color)) %>%
    mutate(long_desc = paste0(.data$long_desc, "."))
}
