test_that("flex_site_board format", {
  skip_if_not_installed("withr")
  skip_if_not(rmarkdown::pandoc_available())

  testdoc <- "test_site_board.Rmd"

  source <- file.path(getwd(), "input-data", "testBoard.Rmd")

  withr::with_tempdir({

    # strongly inspired by flexdashbaord's test:
    file.copy(source, testdoc)

    capture.output({
      output_file <- "tstrsltidx.html"
      output_format <- flex_site_board(self_contained = FALSE)
      rmarkdown::render(testdoc,
                        output_format = output_format,
                        output_file = output_file)
      expect_true(file.exists(output_file))
    })
    expect_gt(length(list.files(recursive = TRUE)), 6)
    expect_true(any(grepl("flexsiteboard.js",
                    readLines(output_file))))
  })
})
test_that("flex_site_board warns on self_contained", {
  skip_if_not_installed("withr")
  skip_if_not(rmarkdown::pandoc_available())

  testdoc <- "test_site_board.Rmd"

  source <- file.path(getwd(), "input-data", "testBoard.Rmd")

  withr::with_tempdir({

    # strongly inspired by flexdashbaord's test:
    file.copy(source, testdoc)

    expect_warning(
      capture.output({
        output_file <- "tstrsltidx.html"
        output_format <- flex_site_board(self_contained = TRUE)
          rmarkdown::render(testdoc,
                            output_format = output_format,
                            output_file = output_file)
        expect_true(file.exists(output_file))
      }),
      regexp =
        "self.contained *= *TRUE.*by flex.*site.*board, disables flex_site_baord.*falling back to flex.*dashboard",
      perl = TRUE
    )
    expect_equal(length(list.files(recursive = TRUE)), 2)
    expect_false(any(grepl("flexsiteboard.js",
                          readLines(output_file))))
  })
})
