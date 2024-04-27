## Resubmission

In this version I have 

* DESCRIPTION: removed unnecessary spaces and written package, software and 
API names in single quotes in title and description
* Removed all instances of \dontrun{}
* R/install_otp.R, R/ioslides_level: ensured that these functions do not write 
  by default, or in my examples and tests, in the user's home filespace      

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- Debian Linux, GCC (R-patched and R-devel) on R-hub
- Fedora Linux, Clang (R-devel) on R-hub
- macOS (R-release), ubuntu (R-oldrel, R-release, R-devel), windows (R-release) using the rcmdcheck package
- macOS builder 
- win-builder (R-devel, R-release and R-oldrelease)

## Downstream dependencies

None, this is a new submission
