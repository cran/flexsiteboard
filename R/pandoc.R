pandoc <- function() {
  if (!rmarkdown::pandoc_available()) {
    stop("Need pandoc (>= 2.4) - http://pandoc.org for conversion.") # nocov
  }
  rmarkdown::pandoc_exec()
}

..pandoc <- new.env(parent = emptyenv())

#' Give all Input Formats Supported by `Pandoc`
#'
#' @param use_cache don't call `Pandoc` again
#'
#' @return all supported `Pandoc` input formats
#' @export
pandoc_input_formats <- function(use_cache = TRUE) {
  if (!use_cache || !exists('pandoc_input_formats', envir = ..pandoc)) {
    assign('pandoc_input_formats',
           system2(pandoc(), "--list-input-formats",
                   stdout = TRUE, stderr = TRUE), envir = ..pandoc)
  }
  return(get('pandoc_input_formats', envir = ..pandoc))
}

#' Give all Output Formats Supported by `Pandoc`
#'
#' @param use_cache don't call `Pandoc` again
#'
#' @return all supported `Pandoc` output formats
#' @export
pandoc_output_formats <- function(use_cache = TRUE) {
  if (!use_cache || !exists('pandoc_output_formats', envir = ..pandoc)) {
    assign('pandoc_output_formats',
           system2(pandoc(), "--list-output-formats",
                   stdout = TRUE, stderr = TRUE), envir = ..pandoc)
  }
  return(get('pandoc_output_formats', envir = ..pandoc))
}
