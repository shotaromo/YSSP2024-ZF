# Analysis of the zero-fossil scenarios by AIM-Technology & MESSAGEix-GLOBIOM

# Introduction
- This repository includes scenario data and source code for figure production for the zero-fossil scenario analysis by AIM-Technology and MESSAGEix-GLOBIOM.
- Datails on the analysis can be found in the following manuscript:

# How to use
- The IPCC AR6 scenario data v1.1 needs to be downloaded from [here](https://data.ene.iiasa.ac.at/ar6/) and copied to `./data/`.
- Execute `./prog/1st_submission.R` on the command line or main console on RStudio. The figures are generated in `./output/`.
- Following R packages are required: tidyverse, imputeTS, ggtern, ggalluvial, patchwork, cowplot, lemon.