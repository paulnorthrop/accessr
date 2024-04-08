# Copy the example.Rmd file to be used in tests
ex_file <- system.file(package = "accessr", "examples", "example.Rmd")
ex_file <- sub(".Rmd", "", ex_file)

# Check for packages needed for the tests
got_hux <- requireNamespace("huxtable", quietly = TRUE)
got_flex <- requireNamespace("flextable", quietly = TRUE)
got_pandoc <- rmarkdown::pandoc_available("1.14")
got_all <- got_hux && got_flex && got_pandoc

# Check that rmd2html() returns the correct file names
# Set pdf = FALSE to avoid using pagedown::chrome_print()

got_all <- TRUE

if (got_all) {
  # rmd2html()
  res <- rmd2html(ex_file, params = list(hide = TRUE), pdf = FALSE, zip = TRUE)
  test_that("rmd2html()", {
    expect_equal(basename(res$files), "example.html")
  })
  test_that("rmd2html(), zips", {
    expect_equal(basename(res$zips), "accessr_html.zip")
  })
}


