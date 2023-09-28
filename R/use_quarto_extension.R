#' Function to open a KTH quarto template
#'
#' @param file_name a directory name
#' @param ext_name an extension name, by default "ktheme"
#' @export
#' @importFrom utils file.edit
use_quarto_ext <- function(file_name = NULL,
                           ext_name = "ktheme") {

  if (is.null(file_name)) {
    stop("You must provide a valid file_name")
  }

  out_dir <- file_name

  if(!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # check for available extensions
  stopifnot("Extension not in package" = ext_name %in% c("ktheme"))

  # check for existing _extensions directory
  if(!file.exists("_extensions")) dir.create("_extensions")
  message("Created '_extensions' folder")

  # various reading of key-value pairs for reporting
  ext_yml <- readLines(system.file("extdata/_extensions/ktheme/_extension.yml",
                                   package = "ktheme"))

  ext_ver <- gsub(
    x = ext_yml[grepl(x = ext_yml, pattern = "version:")],
    pattern = "version: ",
    replacement = ""
  )

  ext_nm <- gsub(
    x = ext_yml[grepl(x = ext_yml, pattern = "title:")],
    pattern = "title: ",
    replacement = ""
  )

  # Create folder for recursive copying into ahead of time
  if(!file.exists(paste0("_extensions/", ext_name))) dir.create(paste0("_extensions/", ext_name))

  # copy from internals
  file.copy(
    from = system.file(paste0("extdata/_extensions/", ext_name), package = "ktheme"),
    to = paste0("_extensions/"),
    overwrite = TRUE,
    recursive = TRUE,
    copy.mode = TRUE
  )

  # logic check to make sure extension files were moved
  n_files <- length(dir(paste0("_extensions/", ext_name)))

  if (n_files >= 2){
    message(paste(ext_nm, "v", ext_ver, paste0("was installed to _extensions",
      "folder in current working directory.")))
  } else {
    message("Extension appears not to have been created")
  }

  # create new qmd report based on skeleton
  readLines("_extensions/ktheme/skeleton.qmd") |>
    writeLines(text = _,
               con = paste0(out_dir, "/", file_name, ".qmd", collapse = ""))

  # open the new file in the editor
  utils::file.edit(paste0(out_dir, "/", file_name, ".qmd", collapse = ""))

  # copy header.tex over as well
  readLines("_extensions/ktheme/header.tex") |>
    writeLines(text = _, con = paste0(out_dir, "/header.tex"))

}
