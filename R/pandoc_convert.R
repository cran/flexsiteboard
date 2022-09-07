#' Convert a `Pandoc` File
#'
#' @param file file to read
#' @param text string to use instead of file if set
#' @param from input format
#' @param to output format
#'
#' @return the converted text
#' @export
#'
#' @examples
#' if (rmarkdown::pandoc_available()) {
#'   x <- pandoc_convert(
#'       text = "\\section{Test}", from = "latex", to = "markdown")
#'   stopifnot(identical(x, "Test\n====") || identical(x, "# Test"))
#' }
pandoc_convert <- function(file, text = NULL, from = pandoc_input_formats(),
                           to = pandoc_output_formats()) {
  from <- match.arg(from)
  to <- match.arg(to)
  pandoc_write(pandoc_read(file = file, text = text, from = from), to = to)
}
