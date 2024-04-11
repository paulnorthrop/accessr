#' Internal accessr functions
#'
#' Internal accessr functions
#' @details
#' These functions are not intended to be called by the user.
#' @name accessr-internal
#' @keywords internal
NULL

#' @keywords internal
#' @rdname accessr-internal
rmd2presentation <- function(x, format = c("ioslides", "slidy"), zip = TRUE,
                             pdf = FALSE, zip_pdf = zip, pdf_args = list(),
                             add = FALSE, quiet = TRUE, rm_html = FALSE,
                             rm_pdf = FALSE, inc_rmd = FALSE, params = NULL,
                             ...) {
  format <- match.arg(format)
  # If x is missing then find all the .Rmd files in the working directory
  if (missing(x)) {
    rmd_files <- list.files(pattern = "Rmd")
    x <- sub(".Rmd", "", rmd_files)
    html_files <- sub(".Rmd", ".html", rmd_files)
    pdf_files <- sub(".Rmd", ".pdf", rmd_files)
  } else if (length(x) == 1 && dir.exists(x)) {
    rmd_files <- list.files(x, pattern = "Rmd")
    rmd_files <- paste0(x, "/", rmd_files)
    x <- sub(".Rmd", "", rmd_files)
    html_files <- paste0(x, ".html")
    pdf_files <- paste0(x, ".pdf")
  } else {
    rmd_files <- paste0(x, ".Rmd")
    html_files <- paste0(x, ".html")
    pdf_files <- paste0(x, ".pdf")
  }
  # Make doc the same length as x
  lenx <- length(x)
  # Function for Rmd to ioslides or slidy
  if (format == "ioslides") {
    output_format <- ioslides_presentation_accessr(...)
  } else {
    output_format <- rmarkdown::slidy_presentation(...)
  }
  #
  render_fun <- function(i) {
    # Render the .Rmd file as an ioslides presentation
    rmarkdown::render(input = rmd_files[i],
                      output_format = output_format,
                      params = params,
                      quiet = quiet)
  }
  res <- sapply(1:lenx, render_fun)
  res <- list(files = res)
  # Create pdf files, if required
  if (pdf) {
    if (!requireNamespace("pagedown", quietly = TRUE)) {
      stop("The 'pagedown' package is required. Please install it.",
           call.= FALSE)
    }
    pdf_fun <- function(i) {
      # Print to pdf
      pdf_args$input <- html_files[i]
      do.call(pagedown::chrome_print, pdf_args)
    }
    res_pdf <- sapply(1:lenx, pdf_fun)
  }
  # Identify the different directories in x
  dnames <- dirname(rmd_files)
  # Unique directories
  udnames <- unique(dirname(rmd_files))
  # Create html zip file(s), if required
  if (is.character(zip)) {
    zipfile <- rep_len(zip, length(udnames))
    zip <- TRUE
  } else if (is.logical(zip) && zip) {
    zipfile <- rep_len(paste0("accessr_", format), length(udnames))
  }
  if (zip) {
    res_zip <- accessr_zip(x, dnames, udnames, zipfile, add,
                           extension = ".html")
    if (inc_rmd) {
      res_zip <- accessr_zip(x, dnames, udnames, zipfile, add = TRUE,
                             extension = ".rmd")
    }
    res <- c(res, list(zips = res_zip))
    if (rm_html) {
      sapply(html_files, file.remove)
    }
  }
  # Create pdf zip file(s), if required
  if (is.character(zip_pdf)) {
    zipfile <- rep_len(zip_pdf, length(udnames))
    zip_pdf <- TRUE
  } else if (is.logical(zip_pdf) && zip_pdf) {
    zipfile <- rep_len(paste0("accessr_", format, "_pdf"), length(udnames))
  }
  if (pdf && zip_pdf) {
    res_zip_pdf <- accessr_zip(x, dnames, udnames, zipfile, add,
                               extension = ".pdf")
    res <- c(res, list(pdf_zips = res_zip_pdf))
    if (rm_pdf) {
      sapply(pdf_files, file.remove)
    }
  }
  return(invisible(res))
}

#' @keywords internal
#' @rdname accessr-internal
accessr_zip <- function(x, dnames, udnames, zipfile, add, extension) {
  # The zip package is required
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop("The 'zip' package is required. Please install it.",
         call.= FALSE)
  }
  # Directory identifiers for the files
  which_dir <- charmatch(x = dnames, table = udnames)
  # Function to create a zip file
  zip_fun <- function(i) {
    # Set the directory and filename
    d <- dnames[which(which_dir == i)]
    f <- basename(x[which(which_dir == i)])
    zipname <- paste0(d[1], "/", zipfile[i], ".zip")
    zip_exists <- file.exists(zipname)
    files_to_add <- paste0(d[1], "/", f, extension)
    f_ext <- paste0(f, extension)
    # First deal with the cases where we are not adding to an existing zip
    if (!(add & zip_exists)) {
      # If the zip file exists then remove it, because add = FALSE
      if (zip_exists) {
        file.remove(zipname)
      }
      res_zip <- zip::zip(zipfile = zipname, files = files_to_add,
                          mode = "cherry-pick")
    } else {
      # If we get to here then add = TRUE and the zip already exists
      # zip::zip_append() could result in multiple files of the same name
      # We want to overwrite any existing files with new versions
      #
      # Which files are in the zip?
      in_zip <- zip::zip_list(zipname)$filename
      # Do we need to overwrite some files?
      overwrite <- any(f_ext %in% in_zip)
      # Which new files appear in files_to_add and in the zip
      if (!overwrite) {
        res_zip <- zip::zip_append(zipfile = zipname, files = files_to_add,
                                   mode = "cherry-pick")
      } else {
        # If all the existing files are to be replaced then use zip::zip().
        # Otherwise, we need to unzip to store the files that need to remain
        # and then write these files and the new ones to a new zip.
        if (all(in_zip %in% f_ext)) {
          res_zip <- zip::zip(zipfile = zipname, files = files_to_add,
                              mode = "cherry-pick")
        } else {
          # Unzip zipname
          # Create a temporary directory into which to unzip files
          dir.create(tmp <- tempfile())
          dir.create(file.path(tmp, "mydir"))
          zip::unzip(zipfile = zipname, exdir = "mydir")
          old_files <- dir("mydir")
          to_stay <- setdiff(old_files, f_ext)
          if (length(to_stay) > 0) {
            to_stay <- paste0("mydir", "/", to_stay)
            res_zip <- zip::zip(zipfile = zipname, files = to_stay,
                                mode = "cherry-pick")
            res_zip <- zip::zip_append(zipfile = zipname, files = files_to_add,
                                       mode = "cherry-pick")
          } else {
            res_zip <- zip::zip(zipfile = zipname, files = files_to_add,
                                mode = "cherry-pick")
          }
          unlink("mydir", recursive = TRUE)
        }
      }
    }
    return(res_zip)
  }
  res_zip <- sapply(unique(which_dir), zip_fun)
  return(res_zip)
}

#' @keywords internal
#' @rdname accessr-internal
ioslides_presentation_accessr <- function(number_sections = FALSE,
                                          logo = NULL,
                                          slide_level = 2,
                                          incremental = FALSE,
                                          fig_width = 7.5,
                                          fig_height = 4.5,
                                          fig_retina = 2,
                                          fig_caption = TRUE,
                                          dev = 'png',
                                          df_print = "default",
                                          smart = TRUE,
                                          self_contained = TRUE,
                                          widescreen = FALSE,
                                          smaller = FALSE,
                                          transition = "default",
                                          math_method = "mathjax",
                                          mathjax = "default",
                                          analytics = NULL,
                                          template = NULL,
                                          css = NULL,
                                          includes = NULL,
                                          keep_md = FALSE,
                                          lib_dir = NULL,
                                          md_extensions = NULL,
                                          pandoc_args = NULL,
                                          extra_dependencies = NULL,
                                          ...) {

  # base pandoc options for all output
  args <- c()

  # math
  math <- mathjax_to_math(mathjax, math_method)
  math <- check_math_argument(math)
  if (!identical(math$engine, "mathjax")) {
    stop2("Only mathjax is supported for `ioslide_presentation()` for 'math'.")
  }

  # widescreen
  if (widescreen)
    args <- c(args, "--variable", "widescreen");

  # pagedtables
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies,
                                 list(rmarkdown::html_dependency_pagedtable()))

  }

  # transition
  if (is.numeric(transition))
    transition <- as.character(transition)
  else if (transition %in% c("default", "faster", "slower"))
    transition <- switch(transition,
                         "default" = "0.4",
                         "faster" = "0.2",
                         "slower" = "0.6")
  else
    stop2('transition must be "default", "faster", "slower" or a ',
          'numeric value (representing seconds)')
  args <- c(args, rmarkdown::pandoc_variable_arg("transition", transition))

  # additional css
  for (css_file in css)
    args <- c(args, "--css",
              rmarkdown::pandoc_path_arg(css_file, backslash = FALSE))

  # content includes
  args <- c(args, rmarkdown::includes_to_pandoc_args(includes))

  # template path and assets
  if (is.null(template) || !file.exists(template))
    template <- pkg_file("rmd/ioslides/default.html")
  args <- c(args, "--template", rmarkdown::pandoc_path_arg(template))

  # html dependency for ioslides
  extra_dependencies <- append(extra_dependencies,
                               list(html_dependency_ioslides(slide_level)))

  # analytics
  if (!is.null(analytics))
    args <- c(args, rmarkdown::pandoc_variable_arg("analytics", analytics))

  # do not wrap lines: https://github.com/rstudio/rmarkdown/issues/2327
  if (!length(grep('--wrap', pandoc_args)))
    pandoc_args <- c('--wrap', 'none', pandoc_args)

  logo_placeholder <- "data:,LOGO"

  # pre-processor for arguments that may depend on the name of the
  # the input file (e.g. ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    # use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir))
      lib_dir <- files_dir

    # extra args
    args <- c()

    # create the files dir if it doesn't exist
    if (!dir_exists(files_dir))
      dir.create(files_dir)

    # logo
    if (!is.null(logo)) {
      logo_path <- logo
      if (!self_contained) {
        # use same extension as specified logo (default is png if unspecified)
        logo_ext <- xfun::file_ext(logo)
        if (nchar(logo_ext) < 1)
          logo_ext <- "png"
        logo_path <- file.path(files_dir, paste("logo", logo_ext, sep = "."))
        file.copy(from = logo, to = logo_path)
        logo_path <- normalized_relative_to(output_dir, logo_path)
      } else {
        # placeholder, will be replaced by base64-encoded logo in post_processor
        logo_path <- logo_placeholder
      }
      args <- c(args, "--variable", paste("logo=", logo_path, sep = ""))
    }

    # return additional args
    args
  }

  # post processor that renders our markdown using our custom lua
  # renderer and then inserts it into the main file
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    # setup args
    args <- c()

    # add any custom pandoc args
    args <- c(args, pandoc_args)

    # number sections
    if (number_sections)
      args <- c(args,
        rmarkdown::pandoc_lua_filter_args(
          rmarkdown::pkg_file_lua("number-sections.lua")))
    lua_writer <- file.path(dirname(input_file), "ioslides_presentation.lua")
    # The input directory may not be writable (on e.g. Shiny Server), so write
    # to the output directory in this case. We don't always do this since
    # supplying a fully qualified path to the writer can trigger a bug on some
    # Linux configurations.
    if (!file.create(lua_writer, showWarnings = FALSE))
      lua_writer <- file.path(dirname(output_file), basename(lua_writer))
    on.exit(unlink(lua_writer), add = TRUE)

    # determine whether we need to run citeproc
    run_citeproc <- citeproc_required(metadata, read_utf8(input_file))

    # write settings to file
    settings <- c()
    add_setting <- function(name, value) {
      settings <<- c(settings, paste("local", name, "=",
                                     ifelse(value, "true", "false")))
    }
    add_setting("fig_caption", fig_caption)
    add_setting("incremental", incremental)
    add_setting("smaller", smaller)
    add_setting("smart", smart)
    add_setting("mathjax", !is.null(mathjax))

    # Set level of slide header (used by ioslides_presentation.lua)
    settings <- c(settings, sprintf("local slide_level = %s", slide_level))
    write_utf8(settings, lua_writer)

    # For consistency add as pandoc argument
    args <- c(args, "--slide-level", as.character(slide_level))

    # append main body of script
    # If slide_level = 1 use accessr's version of ioslides_presentation.lua
    # Otherwise, use rmarkdown's
    if (slide_level == 1) {
      lua_file <- pkg_file("ioslides/ioslides_presentation.lua",
                           package = "accessr")
    } else {
      lua_file <- pkg_file("rmd/ioslides/ioslides_presentation.lua",
                           package = "rmarkdown")
    }
    file.append(lua_writer, lua_file)

    output_tmpfile <- tempfile("ioslides-output", fileext = ".html")
    on.exit(unlink(output_tmpfile), add = TRUE)

    # on Windows, cache the current codepage and set it to 65001 (UTF-8) for the
    # duration of the Pandoc command. Without this, Pandoc fails when attempting
    # to hand UTF-8 encoded non-ASCII characters over to the custom Lua writer.
    # See https://github.com/rstudio/rmarkdown/issues/134
    if (is_windows() && !pandoc2.0()) {
      # 'chcp' returns e.g., "Active code page: 437"; strip characters and parse
      # the number
      codepage <- as.numeric(gsub("\\D", "", system2("chcp", stdout = TRUE)))

      if (!is.na(codepage)) {
        # if we got a valid codepage, restore it on exit
        on.exit(system2("chcp", args = codepage, stdout = TRUE), add = TRUE)

        # change to the UTF-8 codepage
        system2("chcp", args = 65001, stdout = TRUE)
      }
    }

    rmarkdown::pandoc_convert(input = input_file,
                              to = rmarkdown::relative_to(dirname(input_file), lua_writer),
                              from = rmarkdown::from_rmarkdown(fig_caption),
                              output = output_tmpfile,
                              options = args,
                              citeproc = run_citeproc,
                              verbose = verbose)

    # read the slides
    slides_lines <- read_utf8(output_tmpfile)

    # read the output file
    output_lines <- read_utf8(output_file)

    # base64 encode if needed
    if (self_contained) {
      slides_lines <- base64_encode_images(slides_lines)
      if (!is.null(logo)) {
        logo_base64 <- if (grepl("^data:", logo)) logo else xfun::base64_uri(logo)
        output_lines <- gsub(logo_placeholder, logo_base64, output_lines, fixed = TRUE)
      }
    }

    # substitute slides for the sentinel line
    sentinel_line <- grep("^RENDERED_SLIDES$", output_lines)
    if (length(sentinel_line) == 1) {
      preface_lines <- c(output_lines[1:sentinel_line[1] - 1])
      suffix_lines <- c(output_lines[-(1:sentinel_line[1])])
      output_lines <- c(preface_lines, slides_lines, suffix_lines)
      write_utf8(output_lines, output_file)
    } else {
      stop2("Slides placeholder not found in slides HTML")
    }

    output_file
  }

  # return format
  rmarkdown::output_format(
    knitr = rmarkdown::knitr_options_html(fig_width, fig_height, fig_retina,
                                          keep_md, dev),
    pandoc = rmarkdown::pandoc_options(to = "html",
                                       from = rmarkdown::from_rmarkdown(fig_caption, md_extensions),
                                       args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    df_print = df_print,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = rmarkdown::html_document_base(lib_dir = lib_dir,
                                                self_contained = self_contained,
                                                mathjax = mathjax,
                                                pandoc_args = pandoc_args,
                                                extra_dependencies = extra_dependencies,
                                                bootstrap_compatible = TRUE, ...))
}

#' @keywords internal
#' @rdname accessr-internal
html_dependency_ioslides <- function(slide_level) {
  # If slide_level = 1 use accessr's version of theme/css/default.css
  # Otherwise, use rmarkdown's
  if (slide_level == 1) {
    src_path <- pkg_file("ioslides/ioslides-13.5.1", package = "accessr")
  } else {
    src_path <- pkg_file("rmd/ioslides/ioslides-13.5.1", package = "rmarkdown")
  }
  htmltools::htmlDependency(
    name = "ioslides",
    version = "13.5.1",
    src = src_path,
    script = c(
      "js/modernizr.custom.45394.js",
      "js/prettify/prettify.js",
      "js/prettify/lang-r.js",
      "js/prettify/lang-yaml.js",
      "js/hammer.js",
      "js/slide-controller.js",
      "js/slide-deck.js"
    ),
    stylesheet = c(
      "fonts/fonts.css",
      "theme/css/default.css",
      "theme/css/phone.css")
  )
}

#' @keywords internal
#' @rdname accessr-internal
check_math_argument <- function(math) {
  url <- NULL
  engine <- NULL
  # math is deactivated
  if (is.null(math)) return(NULL)
  # otherwise
  if (is.list(math) && !is.null(names(math))) {
    # can be a named list
    engine <- if (is.character(math$engine)) math$engine
    url <- math$url
  } else if (length(math) == 1L && is.character(math[[1L]])) {
    # can be a string
    engine <- math[[1L]]
  }

  # if no engine found, incorrect value must have been provided
  if (is.null(engine)) {
    stop2("'math' can be the engine name (a string) or a list with engine and optionally the url to use.")
  }

  list(engine = engine, url = url)
}

#' @keywords internal
#' @rdname accessr-internal
mathjax_to_math <- function(mathjax, math) {
  if (is.null(mathjax)) {
    # deactivate math
    return(NULL)
  } else if (identical(mathjax, "default")) {
    # default to other argument now
    return(math)
  } else if (identical(mathjax, "local")) {
    # use local mathajx
    return(list(engine = "mathjax", url = "local"))
  } else if (is.logical(mathjax)) {
    # let mathjax = FALSE deactivate
    return(if(isTRUE(mathjax)) math)
  } else if (is.character(mathjax)) {
    # any other string should be a mathjax url
    return(list(engine = "mathjax", url = mathjax))
  }
  # just return math for any other value
  math
}

# needs to handle the case when this function is used in a package loaded with
# devtools or pkgload load_all(). Required for testing with testthat also.
# From pkgdown:
# https://github.com/r-lib/pkgdown/blob/04d3a76892320ac4bd918b39604c157e9f83507a/R/utils-fs.R#L85
#' @keywords internal
#' @rdname accessr-internal
pkg_file <- function(..., package = "rmarkdown", mustWork = FALSE) {
  if (devtools_loaded(package)) {
    # used only if package has been loaded with devtools or pkgload
    file.path(find.package(package), "inst", ...)
  } else {
    system.file(..., package = package, mustWork = mustWork)
  }
}

# devtools metadata -------------------------------------------------------

# from pkgdown & downlit
# https://github.com/r-lib/pkgdown/blob/77f909b0138a1d7191ad9bb3cf95e78d8e8d93b9/R/utils.r#L52

#' @keywords internal
#' @rdname accessr-internal
devtools_loaded <- function(x) {
  if (!x %in% loadedNamespaces()) {
    return(FALSE)
  }
  ns <- .getNamespace(x)
  !is.null(ns$.__DEVTOOLS__)
}

# test if all paths in x are directories
#' @keywords internal
#' @rdname accessr-internal
dir_exists <- function(x) {
  length(x) > 0 && utils::file_test('-d', x)
}

#' @keywords internal
#' @rdname accessr-internal
citeproc_required <- function(yaml_front_matter,
                              input_lines = NULL) {
  # TODO: remove the hack below after BETS is updated on CRAN https://github.com/nmecsys/BETS/pull/18
  if (tryCatch(xfun::check_old_package('BETS', '0.4.9'), error = function(e) FALSE)) return(FALSE)
  (
    is.null(yaml_front_matter$citeproc) ||
      yaml_front_matter$citeproc
  ) && (
    !is.null(yaml_front_matter$bibliography) ||
      !is.null(yaml_front_matter$references) ||
      # detect references: and bibliography: outside of yaml header
      # as Pandoc is supporting
      # TODO: remove when supporting multiple yaml block
      # https://github.com/rstudio/rmarkdown/issues/1891
      length(grep("^references:\\s*$", input_lines)) > 0 ||
      length(grep("^bibliography:\\s*$", input_lines)) > 0
  )
}

#' @keywords internal
#' @rdname accessr-internal
read_utf8 <- function(file) {
  if (inherits(file, 'connection')) con <- file else {
    con <- base::file(file, encoding = 'UTF-8'); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}

#' @keywords internal
#' @rdname accessr-internal
write_utf8 <- function(text, con, ...) {
  opts <- options(encoding = "native.enc"); on.exit(options(opts), add = TRUE)
  writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
}

#' @keywords internal
#' @rdname accessr-internal
is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' @keywords internal
#' @rdname accessr-internal
pandoc2.0 <- function() {
  rmarkdown::pandoc_available("2.0")
}

#' @keywords internal
#' @rdname accessr-internal
base64_encode_images <- function(html) {
  encode <- function(img_src, src) {
    in_file <- utils::URLdecode(src)
    if (length(in_file) && file.exists(in_file)) {
      img_src <- sub(src, xfun::base64_uri(in_file), img_src, fixed = TRUE)
    }
    img_src
  }
  html <- process_images(html, encode)
  process_html_res(html, "<[^>]*style=\"[^\"]*url\\(([^\\)]+)\\)", encode)
}

#' @keywords internal
#' @rdname accessr-internal
process_images <- function(html, processor) {
  process_html_res(html, "<\\s*img\\s+.*?src\\s*=\\s*[\"']([^\"']+)[\"']", processor)
}

# processes an HTML resource, given a regular expression that locates
# instances of that resource
#' @keywords internal
#' @rdname accessr-internal
process_html_res <- function(html, reg, processor) {
  m <- gregexpr(reg, html, perl = TRUE, ignore.case = TRUE)
  regmatches(html, m) <- lapply(regmatches(html, m), function(img_src) {
    src <- sub(reg, '\\1', img_src, ignore.case = TRUE)
    vapply(
      seq_along(img_src),
      function(i) processor(img_src[[i]], src[[i]]),
      character(1)
    )
  })
  html
}

#' @keywords internal
#' @rdname accessr-internal
stop2 <- function(...) stop(..., call. = FALSE)

# A variant of relative_to that normalizes its inputs.
#' @keywords internal
#' @rdname accessr-internal
normalized_relative_to <- function(dir, file) {
  relative_to(
    normalize_path(dir, must_work = FALSE),
    normalize_path(file, must_work = FALSE))
}
