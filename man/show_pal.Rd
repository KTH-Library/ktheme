% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palette.R
\name{show_pal}
\alias{show_pal}
\title{Show palette}
\usage{
show_pal(
  colours,
  labels = TRUE,
  borders = NULL,
  cex_label = 1,
  ncol = length(colours),
  nrow = 1
)
}
\arguments{
\item{colours}{a character vector of colours}

\item{labels}{boolean, whether to show the hexadecimal representation of the colours in each tile}

\item{borders}{colour of the borders of the tiles; matches the border argument of graphics::rect(). The default means par("fg"). Use border = NA to omit borders.}

\item{cex_label}{size of printed labels, works the same as cex parameter of plot()}

\item{ncol}{number of columns, defaults to the matching the number of colours provided}

\item{nrow}{number of rows, defaults to 1}
}
\description{
Show palette
}
