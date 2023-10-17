# Prepare EDGE Transport Data for the REMIND model

R package **edgeTransport**, version **1.2.0**

[![CRAN status](https://www.r-pkg.org/badges/version/edgeTransport)](https://cran.r-project.org/package=edgeTransport)  [![R build status](https://github.com/pik-piam/edgeTransport/workflows/check/badge.svg)](https://github.com/pik-piam/edgeTransport/actions) [![codecov](https://codecov.io/gh/pik-piam/edgeTransport/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/edgeTransport) [![r-universe](https://pik-piam.r-universe.dev/badges/edgeTransport)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

EDGE-T is a fork of the GCAM transport module https://jgcri.github.io/gcam-doc/energy.html#transportation with a high level of detail in its representation of technological and modal options. It is a partial equilibrium model with a nested multinomial logit structure and relies on the modified logit formulation. Most of the sources are not publicly available. PIK-internal users can find the sources in the distributed file system in the folder `/p/projects/rd3mod/inputdata/sources/EDGE-Transport-Standalone`.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("edgeTransport")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("EDGEtransport") # Data preparation with EDGEtransport
```

## Questions / Problems

In case of questions / problems please contact Alois Dirnaichner <dirnaichner@pik-potsdam.de>.

## Citation

To cite package **edgeTransport** in publications use:

Dirnaichner A, Rottoli M, Hoppe J (2023). _edgeTransport: Prepare EDGE Transport Data for the REMIND model_. R package version 1.2.0, <https://github.com/pik-piam/edgeTransport>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {edgeTransport: Prepare EDGE Transport Data for the REMIND model},
  author = {Alois Dirnaichner and Marianna Rottoli and Johanna Hoppe},
  year = {2023},
  note = {R package version 1.2.0},
  url = {https://github.com/pik-piam/edgeTransport},
}
```
