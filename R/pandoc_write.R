#' Write a `Pandoc` File
#'
#' @param pandoc internal `Pandoc` representation
#' @param to output format (supported by `Pandoc`)
#'
#' @return the converted text
#' @export
pandoc_write <- function(pandoc, to = pandoc_output_formats()) {
  to <- match.arg(to)
  args <- sprintf("-f json -t %s", shQuote(to))
  pandoc <- paste0(pandoc, collapse = "")
  paste(system2(pandoc(), args, input = pandoc, stdout=TRUE, stderr=TRUE), collapse = "\n")
}
