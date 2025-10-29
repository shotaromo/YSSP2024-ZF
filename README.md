# Analysis of the zero-fossil scenarios by AIM-Technology & MESSAGEix-GLOBIOM

# Introduction
- This repository includes scenario data and source code for figure production for the zero-fossil scenario analysis by AIM-Technology and MESSAGEix-GLOBIOM.
- Datails on the analysis can be found in the following manuscript:

# How to use
- The IPCC AR6 scenario data v1.1 needs to be downloaded from [here](https://data.ene.iiasa.ac.at/ar6/) and copied to `./data/`.
- The technology-specific mineral intensity provided by Wei et al. (2025) [here](https://www.nature.com/articles/s41558-025-02373-3) needs to be downloaded from [here](https://zenodo.org/records/17195662) and copied to `./data/minerals`.
- Execute `./prog/1st_revision.R` on the command line or main console on RStudio. The figures are generated in `./output/`.
- Following R packages are required: dplyr, tidyr, readr, stringr, ggplot2, patchwork, imputeTS, ggtern, cowplot and lemon.