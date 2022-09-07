#' Read a `Pandoc` File from some Supported Format
#'
#' @param file file to read
#' @param text string to use instead of file if set
#' @param from format of the file
#'
#' @return the `Pandoc` object
#' @export
pandoc_read <- function(file, text = NULL, from = pandoc_input_formats()) {
  from <- match.arg(from)
  if (is.null(text)) {
    args <- sprintf("-f %s -t json %s", shQuote(from), shQuote(normalizePath(file)))
  } else {
    args <- sprintf("-f %s -t json", shQuote(from))
  }
  system2(pandoc(), args, input = text, stdout=TRUE, stderr=TRUE)
}
