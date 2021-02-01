# esthecorareef

R code and data to reproduce figures and appendices of Langlois et al.'s 2021 article: "An integrated approach to measure aesthetic and ecological values of coralligenous reefs"

<hr />

## General

This repository is structured as follow:

- `data/`: contains all data required to reproduce figures and appendices
- `R/`: contains R functions developed for this project
- `man/`: contains R functions documentation
- `analyses/`: contains R scripts (one per figure or appendices) and a setup file defining parameters

## Notes

- All required packages will be installed (if necessary) and loaded.
- Figures will be stored in `output/`
- All Figures, Tables and Appendices of the article can be reproduced with this repository except Figure 1, Figure 2 and Appendix S2 Table S2
- The information about the species and pressures are properties of Andromède Océanologie and have thus been anonymized here. You can contact Andromède Océanologie and/or visit the medtrix plateform to access the complete data.

## Usage

Clone the repository and run this command in R/RStudio:

```r
source("make.R")
```

Enjoy!
