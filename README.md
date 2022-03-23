# SettlementPersistence

The code and data presented here provide all of the necessary information to replicate the analyses in Crawford et. al. (in press).

Crawford, Katherine, Angela Huster, Matthew A. Peeples, Nicolas Gauthier, Michael E. Smith, Abigail Yor, Daniel Lawrence. (in press)
A systematic approach for studying the persistence of settlements in the past. **Antiquity**

## Contents

* Figures_reproduce.R - R code required to reproduce figures 2-9 in paper and supplemental materials
* RegData.RData - RData object containing all of the site level persistence estimates in R objects by region
* BasinOfMexico.csv - Site persistence data for Basin of Mexico region (same as data contained in RegData.RData)
* CentralItaly.csv - Site persistence data for Central Italy region (same as data contained in RegData.RData)
* FertileCrescent.csv - Site persistence data for Fertile Crescent region (same as data contained in RegData.RData)
* SantaValley.csv - Site persistence data for Santa Valley region (same as data contained in RegData.RData)
* SoutheastUS.csv - Site persistence data for Southeast US region (same as data contained in RegData.RData)
* SouthwestUS.csv - Site persistence data for Southwest US region (same as data contained in RegData.RData)
* Yautepec.csv - Site persistence data for Yautepec Valley region (same as data contained in RegData.RData)
* npp_BM - Net primary productivity data for the Basin of Mexico region
* npp_CI - Net primary productivity data for the Central Italy region
* npp_FC - Net primary productivity data for the Fertile Crescent region
* npp_SE - Net primary productivity data for the Southeast US region
* npp_SV - Net primary productivity data for the Santa Valley region
* npp_SW1 - Net primary productivity data for the Southwest US region (part 1)
* npp_SW2 - Net primary productivity data for the Southwest US region (part 2)
* npp_YA - Net primary productivity data for the Yautepec Valley region
* survey-boundaries.zip - this file contains all of the shape files necessary for creating figure 1 as well as for the net primary productivity calculations outlined in npp_plot.Rmd
* npp_plot.Rmd - R Markdown file outlining the net primary productivity calculation. This file requires the shapefiles in the file "survey-boundaries.zip" to be extracted to a folder in the working directory called "survey-boundaries" and also requires two large CHELSA data files "CHELSA_bio10_01.tif" and "CHELSA_bio10_12.tif" to be downloaded from https://chelsa-climate.org/downloads/ (see details in Markdown document).
* npp_plot.html - This represents the knitted version of the R markdown document in html format.
