# code
1.  shared_ailments.R <br> 
this script contains functions to read and process the ailment data from 2011 to 2021. from 2016 to 2021 (batch2), the data is in wide format, transform_long function is for transforming it from wide to long format. <br> 
2. shared_panel.R <br> 
this code cleans panel data based on 02-clean_households.R shared by NM. the 4 new variables are: average age of household heads, average income of household heads, average age of household heads, average education years of household heads, unemployment for both heads (0/1)<br> 
4. shared_tidycensus_gini.R <br> 
considering the Gini coefficient data previously shared contained an excessive number of NAs, I used tidycensus package and retrieved zip code Gini coefficients for the years 2011 to 2021 (5-year estimates). The data obtained is stored in the data folder. <br> 
5. shared_consumption_simpleClean.ipynb<br> 
This script performs preliminary cleaning of the consumption data. It will be updated later.<br>

# data
1. df_ailments.RData<br>
this is the clean ailment data (long format) from 2011 to 2021.<br>
2. panel.RData<br>
this is the clean panel data.<br>
3. gini.RData<br>
zip code Gini coefficients for the years 2011 to 2021 (5-year estimates)<br>
4. **ailments_merge.RData** <br>
**merged data** of ailments data, panel data and Gini data. 
