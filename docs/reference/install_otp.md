# Install OfficeToPDF

Downloads `OfficeToPDF.exe` from
<https://github.com/cognidox/OfficeToPDF/releases> and copies it to a
suitable location, by default `system.file(package = "accessr")`.

## Usage

``` r
install_otp(dir, url, method, quiet = TRUE, ...)
```

## Arguments

- dir:

  Directory into which to download the executable file
  `OfficeToPDF.exe`. **This argument must be supplied**. Pass
  `dir = "accessr"` to choose the `accessr` package directory, that is,
  `system.file(package = "accessr")`. This choice should enable
  `OfficeToPDF.exe` to be found but there may be a problem if
  [`.libPaths`](https://rdrr.io/r/base/libPaths.html) refers to a cloud
  storage directory, such as OneDrive.

  If `dir` is not supplied, e.g. the call is `install_otp()`, then an
  error is thrown and `dir = "accessr"` is suggested.

- url:

  URL from which to download `OfficeToPDF.exe`. If this is missing then
  <https://github.com/cognidox/OfficeToPDF/releases/download/v1.9.0.2/OfficeToPDF.exe>
  is used.

- method:

  Passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html). If
  missing then `getOption("download.file.method")` is used to set it.

- quiet:

  Passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html). `quiet`
  controls whether messages are printed during the download.

- ...:

  Additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html).

## Value

See the **Value** section of
[`download.file`](https://rdrr.io/r/utils/download.file.html).

## Details

The argument `mode = "wb"` to
[`download.file`](https://rdrr.io/r/utils/download.file.html) is
hard-coded, so that binary transfer is forced.

## Note

`OfficeToPDF.exe` is used to create PDF files from Word documents in
[`rmd2word`](https://github.com/paulnorthrop/accessr/reference/rmd2word.md).
