# Copy the example.Rmd file to be used in tests
ex_file <- system.file(package = "accessr", "examples", "example.Rmd")
ex_file <- sub(".Rmd", "", ex_file)

# Check for packages needed for the tests
got_hux <- requireNamespace("huxtable", quietly = TRUE)
got_flex <- requireNamespace("flextable", quietly = TRUE)
got_pandoc <- rmarkdown::pandoc_available("1.14")
got_all <- got_hux && got_flex && got_pandoc
