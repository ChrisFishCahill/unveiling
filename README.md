# Unveiling the recovery dynamics of Walleye after the Invisible Collapse
This repository contains code to support assessing inland fisheries across landscapes as per Cahill et al. 2021. In particular, this directory contains Stan and R code to fit age-structured population dynamics models to fishery independent survey data collected from Alberta Walleye fisheries. 

The relevant portions of this repository that describe the modeling approaches used in this manuscript are contained in the stan-files and r-files folders.  

The primary .stan script is `BERTA_unveiling.stan`, which is the model script that conducts all our Bayesian analyses.  This .stan file is called and controlled by the `run.R` script in the r-files folder.  All assessment models were fitted to Alberta Fall Walleye Index Netting survey data using these two scripts.

## An additional FYI for users
Please note that *all other folders and files* were used to develop the online BERTA Shiny application, develop plots and maps of relevant assessment quantities, process assessment output, or were used to conduct sensitivity tests specific to the Alberta Walleye fishery. Thus, most users will find these of limited use. 

Interested users can access the BERTA shiny app  [here.](https://fw-habitat-aep.shinyapps.io/BERTA/)

One final note of interest.  The second .stan file `BERTA_unveiling_depensation.stan` is an extension to the `BERTA_unveiling.stan` model described in our paper that also estimates a stock-recruit depensation parameter as per Hilborn and Walters (1992).  This model was not used in our manuscript but other researchers inquired about the potential of explicitly modeling depensation, and so we provide it here in the hopes that others find it useful. 
