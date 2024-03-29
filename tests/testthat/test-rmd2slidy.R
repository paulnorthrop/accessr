# Check that rmd2slidy() returns the correct file names
# Set pdf = FALSE to avoid using pagedown::chrome_print()

# This example needs packages huxtable and flextable
if (got_all) {
  # rmd2slidy()
  res <- rmd2slidy(ex_file, params = list(hide = TRUE), pdf = FALSE, zip = TRUE)
  test_that("rmd2slidy()", {
    expect_equal(basename(res$files), "example.html")
  })
  test_that("rmd2slidy(), zips", {
    expect_equal(basename(res$zips), "accessr_slidy.zip")
  })
}


