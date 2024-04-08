# shared code
## 1.  shared_individuals_allYears.R
this script contains functions to read and process the ailment data from 2011 to 2021. from 2016 to 2021 (batch2), the data is in wide format, transform_long function is for transforming it from wide to long format.
## 2. shared_tidycensus_gini.R
considering the Gini coefficient data previously shared contained an excessive number of NAs, I used tidycensus package and retrieved zip code Gini coefficients for the years 2011 to 2021 (5-year estimates). The data obtained is stored in the data folder. 
## shared_consumption_simpleClean.ipynb
This script performs preliminary cleaning of the consumption data. It will be updated later.

# data
## 1. AilmentLong16-21.csv
this is the processed ailment data (long format) from 2016 to 2021. 
## 2. gini_5yEst.csv
zip code Gini coefficients for the years 2011 to 2021 (5-year estimates)

