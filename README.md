
<!-- README.md is generated from README.Rmd. Please edit that file -->

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
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo vignevin/startbox@HEAD
#> rlang (1.1.5 -> 1.1.6) [CRAN]
#> cli   (3.6.4 -> 3.6.5) [CRAN]
#> Installing 2 packages: rlang, cli
#> Installation des packages dans 'C:/Users/hmaire.VIGNEVIN/AppData/Local/Temp/Rtmp0aHE8x/temp_libpath4fa8551d7dd8'
#> (car 'lib' n'est pas spécifié)
#> le package 'rlang' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> le package 'cli' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> 
#> Les packages binaires téléchargés sont dans
#>  C:\Users\hmaire.VIGNEVIN\AppData\Local\Temp\Rtmp4e1Dji\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\hmaire.VIGNEVIN\AppData\Local\Temp\Rtmp4e1Dji\remotes542c445c1c0e\vignevin-startbox-2b41faf/DESCRIPTION' ...  ✔  checking for file 'C:\Users\hmaire.VIGNEVIN\AppData\Local\Temp\Rtmp4e1Dji\remotes542c445c1c0e\vignevin-startbox-2b41faf/DESCRIPTION' (716ms)
#>       ─  preparing 'startbox':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts (599ms)
#>       ─  checking for empty or unneeded directories
#>       ─  building 'startbox_0.0.0.9000.tar.gz'
#>      
#> 
#> Installation du package dans 'C:/Users/hmaire.VIGNEVIN/AppData/Local/Temp/Rtmp0aHE8x/temp_libpath4fa8551d7dd8'
#> (car 'lib' n'est pas spécifié)
```

If you also want to install the vignette (which serves as a tutorial for
the package), run this after installation:

``` r
devtools::install(build_vignettes = TRUE)
#> pkgbuild (1.4.7 -> 1.4.8 ) [CRAN]
#> fs       (1.6.5 -> 1.6.6 ) [CRAN]
#> sass     (0.4.9 -> 0.4.10) [CRAN]
#> tinytex  (0.56  -> 0.57  ) [CRAN]
#> later    (1.4.1 -> 1.4.2 ) [CRAN]
#> Installing 5 packages: pkgbuild, fs, sass, tinytex, later
#> Installation des packages dans 'C:/Users/hmaire.VIGNEVIN/AppData/Local/Temp/Rtmp0aHE8x/temp_libpath4fa8551d7dd8'
#> (car 'lib' n'est pas spécifié)
#> le package 'pkgbuild' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> le package 'fs' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> le package 'sass' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> le package 'tinytex' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> le package 'later' a été décompressé et les sommes MD5 ont été vérifiées avec succés
#> 
#> Les packages binaires téléchargés sont dans
#>  C:\Users\hmaire.VIGNEVIN\AppData\Local\Temp\Rtmp4e1Dji\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\hmaire.VIGNEVIN\OneDrive - IFV\Documents\startbox/DESCRIPTION' ...     checking for file 'C:\Users\hmaire.VIGNEVIN\OneDrive - IFV\Documents\startbox/DESCRIPTION' ...   ✔  checking for file 'C:\Users\hmaire.VIGNEVIN\OneDrive - IFV\Documents\startbox/DESCRIPTION' (695ms)
#>       ─  preparing 'startbox': (2s)
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  installing the package to build vignettes (834ms)
#>          creating vignettes ...     creating vignettes ...   ✔  creating vignettes (23.7s)
#>       ─  checking for LF line-endings in source and make files and shell scripts (3.5s)
#>       ─  checking for empty or unneeded directories
#>       ─  building 'startbox_0.0.0.9000.tar.gz'
#>      
#> Running "C:/PROGRA~1/R/R-44~1.3/bin/x64/Rcmd.exe" INSTALL \
#>   "C:\Users\HMAIRE~1.VIG\AppData\Local\Temp\Rtmp4e1Dji/startbox_0.0.0.9000.tar.gz" \
#>   --install-tests 
#> * installing to library 'C:/Users/hmaire.VIGNEVIN/AppData/Local/Temp/Rtmp0aHE8x/temp_libpath4fa8551d7dd8'
#> * installing *source* package 'startbox' ...
#> ** using staged installation
#> ** R
#> ** inst
#> ** tests
#> ** byte-compile and prepare package for lazy loading
#> ** help
#> *** installing help indices
#> *** copying figures
#> ** building package indices
#> ** installing vignettes
#> ** testing if installed package can be loaded from temporary location
#> ** testing if installed package can be loaded from final location
#> ** testing if installed package keeps a record of temporary installation path
#> * DONE (startbox)
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
