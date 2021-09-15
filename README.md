# Unveiling the recovery dynamics of Walleye following the Invisible Collapse
This repository contains code to support assessing inland fisheries across landscapes as per Cahill et al. In particular, this directory contains Stan and R code to fit age-structured population dynamics models to fishery independent survey data collected from Alberta Walleye fisheries. 

The relevant portions of the directory for users interested in the modeling approaches described in Cahill et al. are listed in the stan-files and r-files folders.  

The primary .stan script is `BERTA_unveiling.stan`, which is model script that conducts our Bayesian analyses.  This .stan file is called by the `run.R` script in the r-files folder.  All assessment models were fitted to Alberta Fall Walleye Index Netting survey data using these two scripts.

## An additional FYI for users
Please note that *all other folders and files* were used to develop the online BERTA Shiny application, develop plots and maps of relevant assessment quantities, process assessment output after fitting models, or were used to conduct sensitivity tests. 

One final note of interest.  The second .stan file `BERTA_unveiling_depensation.stan` is an extension to `BERTA_unveiling.stan` model described in our paper that also estimates a stock-recruit depensation parameter as per Hilborn and Walters (1992).  This was not used in our manuscript but other researchers inquired about the potential of explicitly incorporating such a parameter, and so we provide it here in the hopes that others find it useful. 
