# Check that rmd2html() returns the correct file names
# Set pdf = FALSE to avoid using OfficeToPDF or pagedown::chrome_print()

# Create documents from example.Rmd
got_hux <- requireNamespace("huxtable", quietly = TRUE)
got_flex <- requireNamespace("flextable", quietly = TRUE)
got_pandoc <- rmarkdown::pandoc_available("1.14")
got_all <- got_hux && got_flex && got_pandoc
# This example needs packages huxtable and flextable
if (got_all) {
  ex_file <- system.file(package = "accessr", "examples", "example.Rmd")
  ex_file <- sub(".Rmd", "", ex_file)
  # ioslides only
  res <- rmd2html(ex_file, params = list(hide = TRUE), pdf = FALSE,
                  zip = FALSE)
  test_that("rmd2html()", {
    expect_equal(basename(res$files), "example.html")
  })
}


