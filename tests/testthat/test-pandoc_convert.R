test_that("pandoc conversion works", {
  skip_if_not(rmarkdown::pandoc_available())
  x <- pandoc_convert(
    text = "\\section{Test}",
    from = "latex",
    to = "markdown")
  expect_true(
    identical(x, "Test\n====") ||
      identical(x, "# Test"))
})
