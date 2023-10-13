#' KTH favicon
#'
#' Favicon in SVG format.
#'
#' If no logo_file path is provided, the embedded favicon will be used.
#'
#' @param logo_file optional path to custom logo
#' @param as one of "img" or "link"
#' @param ... further arguments sent to HTML img if using as "img"
#' @return HTML
#' @importFrom knitr image_uri
#' @importFrom htmltools HTML a img
#' @export
kth_b64_favicon_svg <- function(
  logo_file = system.file(package = "ktheme", "logo", "favicon.svg"),
  as = c("img", "link", "uri"), ...) {

  iu <- knitr::image_uri(logo_file)

  res <- switch(match.arg(as, several.ok = FALSE),
    uri = iu |> as.character(),
    img = {
      image <- htmltools::browsable(htmltools::img(src = iu, ...))
      image
    },
    link = {
      link <- htmltools::tags$link(
        rel = "icon", type = "image/x-icon", href = iu
      )
      link
    }
  )

  res
}

#' KTH logo
#'
#' Logo in SVG format.
#'
#' @param variant, one of "blue", "black" or "white"
#' @param as return type, one of "img" or "uri"
#' @param ... further arguments for the HTML img tag
#' @return HTML browsable image tag with base64 encoded data embedded or uri
#' @importFrom knitr image_uri
#' @importFrom htmltools browsable img
#' @export
kth_b64_logo_svg <- function(variant = c("blue", "black", "white"),
  as = c("img", "uri"), ...) {

  if (missing(variant))
    variant <- "blue"

  logo_file <- system.file(package = "ktheme", "logo",
    sprintf("kth_logo_%s.svg", variant))

  stopifnot(file.exists(logo_file))

  iu <- knitr::image_uri(logo_file)

  res <- switch(match.arg(as),
    img = htmltools::browsable(htmltools::img(src = iu, ...)),
    uri = iu
  )

  return(res)

}

#' KTH webfont
#'
#' Webfont for CSS styling in woff2 format
#'
#' @param font_name, one of "figtree", "georgia", "open-sans" or
#'   "open-sans-condensed"
#' @param as return type, one of "css" or "uri"
#' @return CSS tag with base64 encoded data embedded or uri
#' @importFrom knitr image_uri
#' @importFrom htmltools includeCSS
#' @export
kth_b64_webfont <- function(font_name =
  c("figtree", "georgia", "open-sans", "open-sans-condensed"),
  as = c("css", "uri")) {

  # woff2 fonts were generated in inst/fonts using
  # npm install -g ttf2woff2
  # cat georgia/georgia.ttf | ttf2woff2 > georgia.woff2

  font_name <- match.arg(font_name)
  fn <- system.file(package = "ktheme", "fonts", paste0(font_name, ".woff2"))
  stopifnot(file.exists(fn))

  iu <- knitr::image_uri(fn)

  res <- switch(match.arg(as),
    uri = iu,
    css = {
      ff <- "@font-face {
        font-family: '%s';
        src: url('%s') format('woff2');
      }" |> sprintf(font_name, iu)
      con <- textConnection(ff)
      on.exit(close(con))
      htmltools::includeCSS(con)
    }
  )
  return(res)
}
