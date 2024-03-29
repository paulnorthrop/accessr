# Check that rmd2word() returns the correct file names
# Set pdf = FALSE to avoid using OfficeToPDF or pagedown::chrome_print()

# This example needs packages huxtable and flextable
if (got_all) {
  # rmd2word()
  res <- rmd2word(ex_file, params = list(hide = TRUE), pdf = FALSE, zip = TRUE)
  test_that("rmd2word()", {
    expect_equal(basename(res$files), "example.docx")
  })
  test_that("rmd2word(), zips", {
    expect_equal(basename(res$zips), "accessr_word.zip")
  })
}


