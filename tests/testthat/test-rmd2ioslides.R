# Check that rmd2html() returns the correct file names
# Set pdf = FALSE to avoid using OfficeToPDF or pagedown::chrome_print()

if (got_all) {
  # rmd2many(), isoslides only
  res <- rmd2many(ex_file, params = list(hide = TRUE), outputs = "ioslides",
                  add18 = FALSE, pdf = FALSE, zip = TRUE)
  test_that("rmd2many(), ioslides", {
    expect_equal(basename(res$files), "example.html")
  })
  test_that("rmd2many(), word, zips", {
    expect_equal(basename(res$zips), "example.zip")
  })
}


