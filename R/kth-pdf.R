#' kth R markdown template for PDF output
#'
#' Template for creating an R markdown document
#' @importFrom rmarkdown pdf_document render
#' @inheritDotParams rmarkdown::pdf_document
#' @param ... Arguments to \code{rmarkdown::pdf_document}
#'
#' @return R Markdown output format to pass to \code{\link[rmarkdown:render]{render}}
#'
#' @export
kth_pdf <- function(...) {
  rmarkdown::pdf_document(...)
}

