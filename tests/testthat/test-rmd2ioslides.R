# Check that rmd2ioslides() returns the correct file names
# Set pdf = FALSE to avoid using pagedown::chrome_print()

# This example needs packages huxtable and flextable
if (got_all) {
  # rmd2ioslides()
  res <- rmd2ioslides(ex_file, params = list(hide = TRUE), pdf = FALSE, zip = TRUE)
  test_that("rmd2ioslides()", {
    expect_equal(basename(res$files), "example.html")
  })
  test_that("rmd2ioslides(), zips", {
    expect_equal(basename(res$zips), "accessr_ioslides.zip")
  })
}


