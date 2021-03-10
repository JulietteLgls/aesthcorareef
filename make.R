###################################################################################################
#' Run the Entire Project
#'
#' This script runs the entire project and produces the figures and appendices present in the 
#' Langlois et al.'s 2021 paper.
#'
#' @author Juliette Langlois, \email{juliette.a.langlois@@gmail.com},
#'         Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         François Guilhaumon, \email{francois.guilhaumon@@ird.fr}
#'
#' @date 2021/01/12
##################################################################################################

# ----- clean workspace

rm(list = ls())

# ----

# ----- install/update packages

devtools::install_deps()

# ----

# ----- install compendium package

devtools::install(build = FALSE)
devtools::document()

# ----- 

# ----- Appendix S1

source(hh("analysis", "01_AppendixA.R"))

# ----- Appendix S2

source(hh("analysis", "02_AppendixB.R"))

# ----- Appendix S3

source(hh("analysis", "03_AppendixC.R"))

# ----- Appendix S4

source(hh("analysis", "04_AppendixD.R"))

# ----- Appendix S5

source(hh("analysis", "05_AppendixE.R"))

# ----- Appendix S6

source(hh("analysis", "06_AppendixF.R"))

# ----- Figure 3

source(hh("analysis", "07_Figure3.R"))

# ----- Table 2

source(hh("analysis", "08_Table2.R"))

# ----- Figure 4

source(hh("analysis", "09_Figure4.R"))

# ----- Appendix S7

source(hh("analysis", "10_AppendixG.R"))

# ----- Figure 5

source(hh("analysis", "11_Figure5.R"))

# ----- Figure 6

source(hh("analysis", "12_Figure6.R"))

# ----- Appendix S8

source(hh("analysis", "13_AppendixH.R"))

# ----- Appendix S9

source(hh("analysis", "14_AppendixI.R"))

# ----