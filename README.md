# wgutil

This package provides an assortment of functions that I frequently find useful when analyzing ecological data in R. 

## Installation

The recommended way to install this package is using `install_github` from the [devtools](https://github.com/hadley/devtools) package.  Your computer will need to be able to compile from source.  Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/) on Windows, or Xcode on Mac.  If you're on Linux, you're probably ready to go.

After installing Rtools or Xcode, install `devtools` and test your installation with the following:

```R
install.packages("devtools")
devtools::has_devel()
```

If `has_devel()` returns true, your system is ready to install from GitHub.

```R
devtools::install_github("wgaul/wgutil")
```

## Using package wgutil

Use `help(package="wgutil")` to view the help index of all functions available in package `wgutil`.
