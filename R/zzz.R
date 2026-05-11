.onAttach <- function(libname, pkgname) {

  fnt <- systemfonts::system_fonts()
  if (!any(grepl("Open[ ]Sans|Georgia|Figtree", fnt$family))) {
    packageStartupMessage("NOTE: Either Open Sans or Georgia fonts or Figtree are required to use these themes.")
    packageStartupMessage("      Please use ktheme::import_open_sans() to install Open Sans and")
    packageStartupMessage("      please use ktheme::import_figtree() to install Figtree and")
    packageStartupMessage("      if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow")
  } # nocov end

}

globalz <- unlist(strsplit(". blue color desc green red rgb40 rgb80", " "))

if(getRversion() >= "2.15.1")  utils::globalVariables(globalz)
