## dev history
# see https://thinkr.fr/transformer-plusieurs-scripts-eparpilles-en-beau-package-r/
# see https://linogaliana.gitlab.io/collaboratif/package.html

usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("misc")
usethis::use_build_ignore("")
usethis::use_build_ignore("doc")
usethis::use_build_ignore("logo_startbox.png")

# 1.c. Renseigner les méta-données du package
# Titre du package
desc::desc_set(
  Title = "visualisation and standardization of experimental data in plant protection"
)
# Désigner les auteurs, contributeurs et
# les détenteurs des droits de propriété intellectuelle
desc::desc_set_authors(c(
  person(
    "Xavier",
    "Delpuech",
    role = c("aut", "cre"),
    email = "xavier.delpuech@vignevin.com"
  ),
  person(
    "Hervé",
    "Maire",
    role = c("aut"),
    email = "herve.maire@vignevin.com"
  ),
  person(
    "Anne-Sophie",
    "Chazalmartin",
    role = c("aut"),
    email = "anne-sophie.chazalmartin@vignevin.com"
  ),
  person(
    family = "Institut Français de la Vigne et du Vin",
    role = "cph"
  )
))

# Décrire ce que fait le package
desc::desc_set(
  Description = "This package is used for management and visualisation of experimental data in plant protection trials.")

# Choisir une licence
usethis::use_gpl_license(version = 3, include_future = TRUE)

# Si la documentation du package est en français
desc::desc_set(Language = "fr")

# 2.d. Utiliser testthat pour les tests
usethis::use_testthat()

### documentation
devtools::document()

## logo
usethis::use_logo("logo_startbox.png")

## creer readMe
usethis::use_readme_rmd()

## creer une vignette
usethis::use_vignette("quickstart.qmd")

# update version if necessary
usethis::use_version()

## creer site
usethis::use_pkgdown()
pkgdown::clean_site()
pkgdown::build_site(override = list(destination = "docs"))
usethis::use_pkgdown_github_pages()


### dependencies
#usethis::use_package("R6", min_version = "2.6.1")
# pour utiliser %>% dans un package
usethis::use_pipe()
usethis::use_package("R6", min_version = "2.6.1")
usethis::use_package("openxlsx2", min_version = "1.14")
usethis::use_package("ggplot2", min_version = "3.5.1")
usethis::use_package("dplyr", min_version = "1.1.4")
usethis::use_package("agricolae", min_version = "1.3-7")
usethis::use_package("rlang")

# !!! supprimer rep Rcheck avant
devtools::check()
