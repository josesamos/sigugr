
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sigugr: Workflow for Geographic Data <a href="https://josesamos.github.io/sigugr/"><img src="man/figures/logo.png" align="right" height="139" alt="sigugr website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/josesamos/sigugr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/josesamos/sigugr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/josesamos/sigugr/graph/badge.svg)](https://app.codecov.io/gh/josesamos/sigugr)
<!-- badges: end -->

The workflow for geographic data typically involves:

- **Data acquisition**: Many datasets are sourced from web downloads.  
- **Data transformation**: This includes tasks such as raster
  composition, resolution adjustments, clipping, and style management.  
- **Data storage**: Storing the processed data in databases, such as
  PostGIS.  
- **Data publication**: Making the data accessible via platforms like
  GeoServer.

The goal of the `sigugr` package is to provide a comprehensive set of
functions that simplify the processes of transforming, storing, and
publishing geographic data.

## Installation

You can install the development version of sigugr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("josesamos/sigugr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sigugr)
## basic example code
```
