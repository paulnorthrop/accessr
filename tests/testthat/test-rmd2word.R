# Check that rmd2many() returns the correct file names
# Set pdf = FALSE to avoid using OfficeToPDF or pagedown::chrome_print()

# This example needs packages huxtable and flextable
if (got_all) {
  # rmd2many(), word only
  res <- rmd2many(ex_file, params = list(hide = TRUE), outputs = "word",
                  add18 = TRUE, pdf = FALSE, zip = FALSE)
  test_that("rmd2many(), word", {
    expect_equal(basename(res$files), c("example18pt.docx", "example.docx"))
  })
}


