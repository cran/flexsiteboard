test_that("pandoc_read works", {
  skip_if_not(rmarkdown::pandoc_available())
  source <- file.path(getwd(), "input-data", "testBoard.Rmd")
  x <- pandoc_read(file = source, from = "markdown")
  y <- pandoc_write(x, to = "plain")
  y <- gsub("\\s", "", y)
  y <- tolower(y)
  expect_snapshot_value(y, )
})
