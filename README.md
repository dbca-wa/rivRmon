
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rivRmon <img src="man/figures/rivRmon_logo.png" align="right" style="padding-left:10px;background-color:white;" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/Bartesto/rivRmon.svg?branch=master)](https://travis-ci.org/Bartesto/rivRmon)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/Bartesto/rivRmon?branch=master&svg=true)](https://ci.appveyor.com/project/Bartesto/rivRmon)
<!-- badges: end -->

The goal of rivRmon is to provide tools to aid in data munging and
visualisation of monitoring data from the Swan and Canning Rivers.

## Installation

You can install the development version from
[GitHub](https://github.com/Bartesto) with:

``` r
# install.packages("devtools")
devtools::install_github("Bartesto/rivRmon")
```

## Example

A basic example:

``` r
library(rivRmon)
# To create surfer plots for the SWan River - please see help for parameter descriptions
swan_surfR(path = "C:/some path to sonde spreadsheets", ovit = "green", ocav = "red")
```
