# accessr 1.1.1

## New features

* In `rmd2word()` the argument `pdf_args` may be used to pass [command line switches](https://github.com/cognidox/OfficeToPDF?tab=readme-ov-file#command-line-switches) to [OfficeToPDF](https://github.com/cognidox/OfficeToPDF) when creating PDF documents. The default setting creates bookmarks in the PDF file using the headings in the input Word document.

## Bug fixes and minor improvements

* `ext_img` now also works if the file extension of an input image is capitalised, that is, "PNG", "JPG" or "JPEG". In version 1.0.1 it only worked for lower case file extensions.
* CRAN installation details added to README
* CRAN downloads badges added to README
