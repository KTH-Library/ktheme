library(readr)
library(dplyr)
library(purrr)
library(progress)

lines <- read_lines("http://mkweb.bcgsc.ca/colornames/color.names.txt")

records <- lines[(lines %>% stringr::str_starts("#") == FALSE)]

parse_records <- function(records) {

  colz <-
    paste0("row abbrev rgb R G B hex HEX hsv H S V xyz xyz_X xyz_Y xyz_Z lab lab_L lab_A lab_B lch lch_L lch_C lch_H cmyk C M Y K ",
           "NEIGHBOUR_STR NUM_NEIGHBOURS_MAXDE WORD_TAGS") %>% strsplit(" ", fixed = TRUE) %>% unlist()
  ct <- cols(
    .default = col_double(),
    abbrev = col_character(),
    rgb = col_character(),
    hex = col_character(),
    HEX = col_character(),
    hsv = col_character(),
    xyz = col_character(),
    lab = col_character(),
    lch = col_character(),
    cmyk = col_character(),
    NEIGHBOUR_STR = col_character(),
    WORD_TAGS = col_character()
  )

  pb <- progress::progress_bar$new(total = length(records))
  pb$tick(0)

  parse_record <- function(x) {
    pb$tick()
    read_delim(paste0(x, "\n"), delim = " ", col_names = colz, col_types = ct)
  }

  map_df(records, safely(parse_record)) %>%
#    pull("row") %>%
    setNames(nm = colz)
}

martin_cols <- parse_records(records)

martin_cols$row %>% mutate(diff = )
  filter(HEX == "#65656C")
  mutate(is_match = stringr::str_detect(WORD_TAGS, pattern = ".*?Cool.*$")) %>%
  filter(is_match)
