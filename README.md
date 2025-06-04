
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# startbox

<!-- badges: start -->
<!-- badges: end -->

The goal of startbox is to manage and visualize experimental data in
plant protection trials.

This R package was developped as part of the STAR project 2024-2027
(France), with the support of the French Ministry of Agriculture and
Food, and the financial contribution of the special allocation account
for agricultural and rural development (CASDAR). The responsability of
the French Ministry of Agriculture and Food cannot be engaged.

## Installation

You can install the development version of startbox from
[GitHub](https://github.com/) with:

``` r
#install.packages("devtools")  # if not already installed
devtools::install_github("vignevin/startbox")
```

If you also want to install the vignette (which serves as a tutorial for
the package), run this after installation:

``` r
devtools::install(build_vignettes = TRUE)
```

Once installed, you can access the vignette with:

``` r
vignette("startbox")
#> démarrage du serveur d'aide httpd ... fini
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
#library(startbox)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Contribution

You can contribute to this projetc project by creating a pull request,
or use issues to track bugs or suggest ideas in a repository.
