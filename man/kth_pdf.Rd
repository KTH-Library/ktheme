% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kth-pdf.R
\name{kth_pdf}
\alias{kth_pdf}
\title{kth R markdown template for PDF output}
\usage{
kth_pdf(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[rmarkdown:pdf_document]{rmarkdown::pdf_document}}
  \describe{
    \item{\code{toc}}{\code{TRUE} to include a table of contents in the output}
    \item{\code{toc_depth}}{Depth of headers to include in table of contents}
    \item{\code{number_sections}}{\code{TRUE} to number section headings}
    \item{\code{fig_width}}{Default width (in inches) for figures}
    \item{\code{fig_height}}{Default height (in inches) for figures}
    \item{\code{fig_crop}}{\code{TRUE} to automatically apply the \code{pdfcrop} utility
(if available) to pdf figures}
    \item{\code{fig_caption}}{\code{TRUE} to render figures with captions}
    \item{\code{dev}}{Graphics device to use for figure output (defaults to pdf)}
    \item{\code{df_print}}{Method to be used for printing data frames. Valid values
include "default", "kable", "tibble", and "paged". The "default" method
uses a corresponding S3 method of \code{print}, typically
\code{print.data.frame}. The "kable" method uses the
\code{\link[knitr:kable]{knitr::kable}} function. The "tibble" method uses
the \pkg{tibble} package to print a summary of the data frame. The "paged"
method creates a paginated HTML table (note that this method is only valid
for formats that produce HTML). In addition to the named methods you can
also pass an arbitrary function to be used for printing data frames. You
can disable the \code{df_print} behavior entirely by setting the option
\code{rmarkdown.df_print} to \code{FALSE}.}
    \item{\code{highlight}}{Syntax highlighting style. Supported styles include
"default", "tango", "pygments", "kate", "monochrome", "espresso",
"zenburn", and "haddock". Pass \code{NULL} to prevent syntax highlighting.}
    \item{\code{template}}{Pandoc template to use for rendering. Pass "default" to use
the rmarkdown package default template; pass \code{NULL} to use pandoc's
built-in template; pass a path to use a custom template that you've
created.  See the documentation on
\href{http://pandoc.org/README.html}{pandoc online documentation}
for details on creating custom templates.}
    \item{\code{keep_tex}}{Keep the intermediate tex file used in the conversion to PDF}
    \item{\code{keep_md}}{Keep the markdown file generated by knitting.}
    \item{\code{latex_engine}}{LaTeX engine for producing PDF output. Options are
"pdflatex", "lualatex", and "xelatex".}
    \item{\code{citation_package}}{The LaTeX package to process citations, \code{natbib}
or \code{biblatex}. Use \code{none} if neither package is to be used.}
    \item{\code{includes}}{Named list of additional content to include within the
document (typically created using the \code{\link[rmarkdown]{includes}} function).}
    \item{\code{md_extensions}}{Markdown extensions to be added or removed from the
default definition or R Markdown. See the \code{\link[rmarkdown]{rmarkdown_format}} for
additional details.}
    \item{\code{output_extensions}}{Pandoc extensions to be added or removed from the
output format, e.g., \code{"-smart"} means the output format will be
\code{latex-smart}.}
    \item{\code{pandoc_args}}{Additional command line options to pass to pandoc}
    \item{\code{extra_dependencies}}{A LaTeX dependency \code{latex_dependency()}, a
list of LaTeX dependencies, a character vector of LaTeX package names (e.g.
\code{c("framed", "hyperref")}), or a named list of LaTeX package options
with the names being package names (e.g. \code{list(hypreref =
c("unicode=true", "breaklinks=true"), lmodern = NULL)}). It can be used to
add custom LaTeX packages to the .tex header.}
  }}
}
\value{
R Markdown output format to pass to \code{\link[rmarkdown:render]{render}}
}
\description{
Template for creating an R markdown document
}
