#' Flexible Dashboards Split in Sub-Pages
#'
#' @param ... see `flexdashbaord`
#'
#' Hint: Needs all global JavaScript code before the first header.
#'
#' @return a new format for `rmarkdown`
#' @export
#'
#' @importFrom utils tail
flex_site_board <- function(...) {

  .sc <- list(...)[["self_contained"]]

  if (length(.sc) != 1) {
    .sc <- TRUE
  }

  # keep_rmds, intermediates: quite fuzzy -- intermediates_generator is only
  # called, if an intermediates_dir has been given calling rmarkdown::render()
  fmt <- flexdashboard::flex_dashboard(...)

  if (.sc) {
    warning("argument 'self-contained = TRUE' by flex_site_board, disables flex_site_baord -- falling back to flex_dashboard")
    return(fmt)
  }

  if ("--self-contained" %in% fmt$pandoc$args) {
    fmt$pandoc$args <-
      setdiff(fmt$pandoc$args, "--self-contained")
  }
  knitenv <- knitr::knit_global()
  if (identical(knitenv, globalenv())) { # only, if not rendering a document
    knitenv <- new.env(parent = emptyenv()) # the object in the knit env is not needed
  }
  assign("is_flex_site_board", TRUE, envir = knitenv)
  # TODO: Detect, if both, dash and site are in the front-matter
  assign("orig", fmt$post_knit, .props)
  fmt$post_knit <- post_knit
  fmt$clean_supporting <- FALSE # why, oh why is this needed?! I cannot define intermediates?
  fmt
}
