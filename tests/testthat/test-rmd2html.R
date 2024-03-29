# Check that rmd2html() returns the correct file names
# Set pdf = FALSE to avoid using pagedown::chrome_print()

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


