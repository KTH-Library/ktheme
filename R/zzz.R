.onAttach <- function(libname, pkgname) {

  if (.Platform$OS.type == "windows")  { # nocov start
    if (interactive()) packageStartupMessage("Registering Windows fonts with R")
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (getOption("ktheme.loadfonts", default = FALSE)) {
    if (interactive()) packageStartupMessage("Registering PDF & PostScript fonts with R")
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }

  fnt <- extrafont::fonttable()
  if (!any(grepl("Open[ ]Sans|Georgia", fnt$FamilyName))) {
    packageStartupMessage("NOTE: Either Open Sans or Georgia fonts are required to use these themes.")
    packageStartupMessage("      Please use ktheme::import_open_sans() to install Open Sans and")
    packageStartupMessage("      if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow")
  } # nocov end

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
