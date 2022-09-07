## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
flexsiteboard::pandoc_convert(text = c("# Test", "# Test2\nTest3"), 
                              from = "markdown",
                              to = "html")

## -----------------------------------------------------------------------------
flexsiteboard::pandoc_convert(text = "This is a <i>test</i>.", 
                              from = "html",
                              to = "markdown")

