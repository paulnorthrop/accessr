# Check that rmd2many() returns the correct file names
# Set pdf = FALSE to avoid using OfficeToPDF or pagedown::chrome_print()

# This example needs packages huxtable and flextable
if (got_all) {
  # rmd2many(), slidy only
  res <- rmd2many(ex_file, params = list(hide = TRUE), outputs = "slidy",
                  add18 = FALSE, pdf = FALSE, zip = FALSE)
  test_that("rmd2many(), slidy", {
    expect_equal(basename(res$files), "example.html")
  })
}


