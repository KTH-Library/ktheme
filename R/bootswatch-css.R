#' @noRd
scss_variables <- function() {

  css_colors <- function(col_name, col_rgb)
    sprintf("$%s: %s !default;", col_name, col_rgb) %>% setNames(col_name)

  gray_light <- palette_kth()["gray40"]
  gray_dark <- palette_kth()["gray"]

  luma <- function(col) {
    rgb <- col2rgb(col)
    luma <- sqrt(0.299 * rgb[1,] ^ 2 + 0.587 * rgb[2,] ^ 2 + 0.114 * rgb[3,] ^ 2)
    setNames(luma, NULL)
  }

  g1 <- round(luma(gray_light) + 40)
  g8 <- round(luma(gray_dark) - 40)
  g9 <- round(luma(gray_dark) - 80)
  gray_lighter <- sprintf("#%x%x%x", g1, g1, g1)
  gray_darker <- sprintf("#%x%x%x", g8, g8, g8)
  gray_darkest <- sprintf("#%x%x%x", g9, g9, g9)

  cols <- c("white", sprintf("gray-%s00", 1:9), "black",
            "blue", "indigo", "purple", "pink", "red", "orange", "yellow", "green", "teal", "cyan")
  pal <- palette_kth(n = 15)
  colors = list(
    css_colors("white", "#fff"),
    css_colors("gray-100", gray_lighter),
    css_colors("gray-200", gray_light),
    css_colors(
      col_name = sprintf("gray-%s00", 3:6),
      col_rgb = scales::grey_pal(start = luma(gray_dark) / 256, end = luma(gray_light) / 256)(4 + 2)[2:5]
    ),
    css_colors("gray-700", gray_dark),
    css_colors("gray-800", gray_darker),
    css_colors("gray-900", gray_darkest),
    css_colors("black", "#000"),
    css_colors("blue", pal["blue"]),
    css_colors("indigo", pal["lightblue80"]),
    css_colors("purple", pal["cerise80"]),
    css_colors("pink", pal["cerise40"]),
    css_colors("red", pal["cerise"]),
    css_colors("orange", pal["olive"]),
    css_colors("yellow", pal["olive40"]),
    css_colors("green", "#007fae"), #    css_colors("green", pal["olive80"]),
    css_colors("teal", pal["lightblue40"]),
    css_colors("cyan", pal["lightblue80"])
  )

  cat(paste0(collapse = "\n", purrr::flatten(colors)))

  # $font-family-sans-serif:      "Lato", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol" !default;
  fonts <- sprintf('$font-family-sans-serif:      "Open Sans", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol" !default;')
  cat("\n", fonts, "\n")

  message("Use the colors and font settings in bootswatch/dist/kth/_variables.scss")
  message("Update the webfont to 'Open Sans' in bootswatch/dist/kth/_variables.scss")
}

