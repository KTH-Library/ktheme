
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ktheme

<!-- badges: start -->

<!-- badges: end -->

This is an R package providing some styling resources for web content
intended to align with the graphical profile used at KTH - the Royal
Institue of Technology.

It is heavily inspired by <https://github.com/hrbrmstr/hrbrthemes>. Most
of the graphical design considerations, the package structure used and
the functions are either ripped directly or modified slightly from that
package adapting it to [KTH’s graphical
profile](https://intra.kth.se/en/administration/kommunikation/grafiskprofil)

The styling assets and resources are different, though (fonts,
templates, color palettes etc).

## Content

This package installs various styling assets as outlined below.

### Fonts

It includes two fonts used in the graphical profile of KTH:

  - Open Sans (primary font; to be used for web content and for text
    inside plots)
  - Georgia (KTH has a license to use this MS font)

### Templates for rmarkdown content

Two templates are provided for styling `rmarkdown` authored content:

  - one for general HTML content
  - one for PDF output.

### CSS

The HTML template uses a stylesheet (CSS file) harvested from
<https://www.kth.se> on 2020-01-08.

This stylesheet is fairly long and refers to some online images and
assets, so the rendering step requires some patience due to the lag when
the online assets are pulled in from the web.

### Theme for ggplot2

It also provides a `theme_kth()` function which can be used to style
ggplots.

### Color palette

Finally, there is a set of color palettes based on the five [KTH
signature
colors](https://intra.kth.se/en/administration/kommunikation/grafiskprofil/profilfarger-1.845077).

This color palette can be used to color qualitative data, sequential
data and diverging data.

  - qualitative palette for nominal or unordered categorical values
    (using the primary KTH profile color (blue), followed by the four
    secondary profile colors, each in three variants using 100%, 40% and
    80% alpha respectively, thus providing a total of 5 x 3 = 15 colors)
  - sequential palette for quantitative magnitudes - high/low values -
    or for ordered categorical data (the primary color in 5 stepped
    variations - using varying levels of alpha)
  - diverging palette for use with quantitative values centered around
    some point - or centered ordered categorical data (the primary color
    is used on one end of the palette and the closest complement color
    on the other to provide a 7-color palette with three steps of blue,
    a gray midpoint and three steps of red)

## Installation

You can install the development version of `ktheme` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KTH-Library/ktheme")
```

## Usage

Here are usage examples showing plots made using the different KTH color
palettes and the Open Sans font.

``` r
# install from https://github.com/KTH-Library/bibliomatrix
suppressPackageStartupMessages(library(bibliomatrix))  
library(dplyr)
library(ktheme)
library(ggplot2)
library(Cairo)
library(extrafont)

extrafont::loadfonts()
```

A plain vanilla scatter plot:

``` r
ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
    title="KTH styled ggplot2 scatterplot example",
    subtitle="A plot that is only useful for demonstration purposes",
    caption="Caption goes here!") + 
  theme_kth()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Using the KTH palette, qualitative coloring:

``` r

ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot(aes(fill = Species)) +
  scale_fill_kth() +
  theme_kth() +
  theme(legend.position = "top")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + 
  geom_point(aes(color = Species)) +
  scale_color_kth() +
  theme_kth()+
  theme(legend.position = "top")
```

<img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

Another example using demo data:

``` r
ggplot(mpg, aes(displ, hwy)) +
  geom_jitter(aes(color=class, fill=class), size=3, shape=21, alpha=1/2) +
  scale_x_continuous(expand=c(0,0), limits=c(1, 8), breaks=1:8) +
  scale_y_continuous(expand=c(0,0), limits=c(10, 50)) +
  scale_color_kth() +
  scale_fill_kth() +
  facet_wrap(~class, scales="free") +
  labs(
    title="KTH styled plots",
    subtitle="These plots show some example data",
    caption="Source: ktheme R package"
  ) +
  theme_kth(grid="XY", axis="xy") +
  theme(legend.position="none") -> gg

flush_ticks(gg)
#> theme(axis.text.x=element_text(hjust=c(0, rep(0.5, 6), 1))) +
#> theme(axis.text.y=element_text(vjust=c(0, rep(0.5, 3), 1)))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Diverging colors:

``` r
cars <- 
  mtcars %>%
  mutate(brand = rownames(mtcars)) %>%
  mutate(mpg_z_score = (mpg - mean(mpg, na.rm = TRUE)) / sd(mpg)) %>%
  mutate(mpg_type = ifelse(mpg_z_score < 0, "below", "above")) %>%
  mutate(CarBrand = factor(mpg_type, levels = unique(mpg_type)))

pdiv <- palette_kth(n = 7, type = "div")[c(1, 7)]
names(pdiv) <- NULL

ggplot(cars, aes(x=reorder(brand, mpg_z_score), y=mpg_z_score, label=mpg_z_score)) +
  geom_bar(stat='identity', aes(fill=mpg_type)) +
  scale_fill_manual(name="Mileage (deviation)",
    labels = c("Above Average", "Below Average"),
    values = c("above" = pdiv[1], "below" = pdiv[2])) +
  labs(subtitle="Z score (normalised) mileage for mtcars'",
    title= "Horizontal bar graph", caption="KTH styled graph") +
  theme_kth() +
  theme(
    axis.title.y=element_blank(),
    axis.title.x=element_blank(),
    legend.position="none"
  ) +
  coord_flip()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Examples of customized ggplots used in the R package `bibliomatrix`:

``` r

# using bibliomatrix::abm_table fcn to get some data
cf <- abm_table3() %>% filter(interval == "Total") %>% pull(cf)

# this is a ggplot2-based bullet graph
abm_bullet(label = "Field normalized citations (Cf)", 
  value = cf, reference = 1.0, roundto = 2) + 
    theme_kth() + 
    # override some theme settings
    theme(
      plot.title=element_text(size = 12),
      axis.text.x=element_text(size = 8),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.line.y=element_blank(),
      axis.line.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.x=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      aspect.ratio = 0.1
    )
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r
nonuniv_share <- 
  abm_table5() %>% filter(interval == "Total") %>% pull(nonuniv_share)

nonuniv_lbl <- 
  sprintf("Swedish non-university: %d%%", round(100 * nonuniv_share))

# this is a ggplot2-based waffle chart
abm_waffle_pct(nonuniv_share, label = nonuniv_lbl) +
  theme_kth() + 
  # override some theme settings
  theme(
    axis.text.y=element_blank(),
    axis.text.x=element_blank(),
    legend.position="none",
  )
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />
