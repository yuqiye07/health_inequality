
# this script is to fetch zip code level Gini coefficients (5-year estimate) using tidycensus package. 

rm(list = ls())

library(readxl)
library(readr)
library(stringr)
library(data.table)
library(pbapply)
library(dplyr)
library(tidyr)
library(tidycensus)


# might need to divide the data into separate year groups due to API limit
data_list <- lapply(2011:2021, function(year) {
  get_acs(geography = "zcta", 
          variables = "B19083_001E", 
          year = year)
})

df_gini <- bind_rows(data_list, .id = "year")

df_gini$year <- as.numeric(df_gini$year)+2010

df_gini <- df_gini %>% 
  mutate(fips = substr(NAME, nchar(NAME)-4, nchar(NAME))) %>% 
  select(c(year, fips, estimate)) %>% 
  rename(zip_gini = estimate) %>% 
  filter(!is.na(zip_gini))
