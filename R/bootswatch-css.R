#' @noRd
scss_variables <- function() {

  css_colors <- function(col_name, col_rgb)
    sprintf("$%s: %s !default;", col_name, col_rgb) %>% setNames(col_name)

  gray_light <- palette_kth()["gray40"]
  gray_dark <- palette_kth()["gray"]

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

  message("Use the colors and font settings in _variables.scss")
  message("Update the webfont to 'Open Sans' in _variables.scss")
}

#' @noRd
less_variables <- function() {

    # TODO: complete this work!
    template <-
    '@gray-base:              #AAAAAA;
    @gray-darker:            #161616; // #222
    @gray-dark:              #65656C;   // #333
    @gray:                   #9C9C9C; // #555
    @gray-light:             #7B7B7B;   // #999
    @gray-lighter:           #dfdfdf; // #eee

    @brand-primary:         #007fae;
    @brand-success:         #1954A5;
    @brand-info:            #64AFDD;
    @brand-warning:         #DCE1A7;
    @brand-danger:          #DCE1A7;

    @body-bg:               #fff;
    @text-color:            @gray-darker;
    @link-color:            @brand-success;
    @link-hover-color:      @link-color;
    @link-hover-decoration: underline;

    @font-family-sans-serif:  "Open Sans", "Segoe UI", Roboto, "Helvetica Neue", Ari
    al, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
    @font-family-serif:       Georgia, "Times New Roman", Times, serif;
    //** Default monospace fonts for `<code>`, `<kbd>`, and `<pre>`.
    @font-family-monospace:   Menlo, Monaco, Consolas, "Courier New", monospace;
    @font-family-base:        @font-family-sans-serif;

    @font-size-base:          15px;
    @font-size-large:         ceil((@font-size-base * 1.25)); // ~18px
    @font-size-small:         ceil((@font-size-base * 0.85)); // ~12px

    @font-size-h1:            floor((@font-size-base * 2.6)); // ~36px
    @font-size-h2:            floor((@font-size-base * 2.15)); // ~30px
    @font-size-h3:            ceil((@font-size-base * 1.7)); // ~24px
    @font-size-h4:            ceil((@font-size-base * 1.25)); // ~18px
    @font-size-h5:            @font-size-base;
    @font-size-h6:            ceil((@font-size-base * 0.85)); // ~12px

    @line-height-base:        1.428571429; // 20/14
    @line-height-computed:    floor((@font-size-base * @line-height-base)); // ~20px
    @headings-font-family:    @font-family-base;
    @headings-font-weight:    400;
    @headings-line-height:    1.1;
    @headings-color:          inherit;
    '

    cat("\n", template, "\n")

    message("Use these colors and font settings in (the beginning of) variables.less")
    message("Replace settings in 'flatly' variables.less for color and fonts (up to 'Iconography')")
}

luma <- function(col) {
  rgb <- col2rgb(col)
  luma <- sqrt(0.299 * rgb[1,] ^ 2 + 0.587 * rgb[2,] ^ 2 + 0.114 * rgb[3,] ^ 2)
  setNames(luma, NULL)
}

css_colors <- function(col_name, col_rgb)
  sprintf("$%s: %s !default;", col_name, col_rgb) %>% setNames(col_name)


#' @noRd
scss_variables_bs5 <- function(pal = palette_kth_neo) {

  # gray nuances
  gray_light <- pal()["gray40"]
  gray_dark <- pal()["gray"]

  g1 <- round(luma(gray_light) + 40)
  g8 <- round(luma(gray_dark) - 40)
  g9 <- round(luma(gray_dark) - 80)

  gray_lighter <- sprintf("#%x%x%x", g1, g1, g1)
  gray_darker <- sprintf("#%x%x%x", g8, g8, g8)
  gray_darkest <- sprintf("#%x%x%x", g9, g9, g9)

  profile_cols <- c(
    "blue", "indigo", "purple", "pink", "red",
    "orange", "yellow", "green", "teal", "cyan"
  )

  hexes <- hexes_neo()

  cols <- c(
    "white",
    sprintf("gray-%s00", 1:9),
    "black",
    profile_cols)

  pal <- pal(n = 15)

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

    # profile colors
    css_colors("blue", hexes$blue),
    css_colors("indigo", hexes$indigo),
    css_colors("purple", hexes$purple),
    css_colors("pink", hexes$pink),
    css_colors("red", hexes$red),
    css_colors("orange", hexes$orange),
    css_colors("yellow", hexes$yellow),
    css_colors("green", hexes$green),
    css_colors("teal", hexes$teal),
    css_colors("cyan", hexes$cyan)
  )

  cat(paste0(collapse = "\n", purrr::flatten(colors)))

  font_kth <- "Figtree"

  fonts <- paste0(
    '$font-family-sans-serif:      \"', font_kth, '\", -apple-system, BlinkMacSystemFont, ',
    '"Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif, "Apple Color Emoji", ',
    '"Segoe UI Emoji", "Segoe UI Symbol" !default;')

  cat("\n", fonts, "\n")

  message("Use the colors and font settings in _variables.scss")
  message("Update the webfont in _bootswatch.scss")
}

