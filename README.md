
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ktheme

<!-- badges: start -->

<!-- badges: end -->

This is an R package providing some styling resources for web content.
It is heavily inspired from <https://github.com/hrbrmstr/hrbrthemes>.

## Content

It includes two fonts used in the graphical profile of KTH:

  - Open Sans (primary font; to be used for web content and for text
    inside plots)
  - Georgia (KTH has a license to use this MS font)

This package installs a number of other styling assets:

  - Two templates for styling `rmarkdown` authoring - one for HTML
    content and one for PDF output. The HTML template uses stylesheet
    (CSS file) harvested from www.kth.se as of 2020-01-08.

  - It also provides a `theme_kth()` function which can be used to style
    ggplots.

  - Finally, there is a set of color palette based on the five KTH
    signature colors (three color palettes for qualitative, sequential
    and diverging data respectively).

## Installation

You can install the development version of `ktheme` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KTH-Library/ktheme")
```
