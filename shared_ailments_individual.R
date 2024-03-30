
# this script contains the following parts:
# 1. transforming batch 2 ailment data from wide format to long format. see transform_longer function.
# 2. reading and processing panel data and Gini data. merged them with transformed ailment data. 
# 3. logistic regressions using individual level data. with household income, zip-code level Gini coefficient, individual age and gender being included as predictors.   
# 4. visualizations for interaction effects.
# Note: the format of the ailment data, particularly the column names, varies across years for Batch 2 data. Therefore, the current processing function may not be suitable for year 2021. It will be adjusted later to generalize to data from all years.


rm(list = ls())

library(readxl)
library(readr)
library(stringr)
library(data.table)
library(pbapply)
library(dplyr)
library(tidyr)
library(ggplot2)
library(interactions)

setwd("/Users/yeyuqi/Desktop/RA_inequality/")


# Import batches 2016 to 2021 ---------------------------------------------

import_2 <- function(x) {
  
  df_raw <- read_excel(x)
  names <- names(df_raw)
 
  
  selected_ailments <- c('Ailment19',
                         'Ailment9',
                         'Ailment16',
                         'Ailment20',
                          'Ailment22',
                          'Ailment28',
                          'Ailment29',
                          'Ailment30',
                          'Ailment32',
                          'Ailment35',
                          'Ailment43')
  
  # match columns starting with "Q16" and ending with selected ailments
  # these columns are ailment occurrence for each household (does any family member got the ailment?)
  regex_pattern1 <- paste("^Q16.*(", paste(selected_ailments, collapse = "|"), ")$", sep = "")
  hhold_ailment_cols <- grep(regex_pattern1, names, value = TRUE)
  
  # match columns starting with "Q31" and ending with selected ailments
  # these columns are ailment occurrence for each household member
  regex_pattern2 <- paste("^Q31.*(", paste(selected_ailments, collapse = "|"), ")$", sep = "")
  mem_ailment_cols <- grep(regex_pattern2, names, value = TRUE)
 
  # select demographic columns for each member
  mem_demo_start <-  match("Q24_who_mem1", names)
  mem_demo_end <-  match("Q28_height_inches_mem9", names)
  demo_cols <- names[mem_demo_start:mem_demo_end]
  
  keep <- c(
    "Household ID",
    'survey number',
    #hhold_ailment_cols,
    mem_ailment_cols,
    "Q23",
    demo_cols
 )
  

  df <- df_raw[which(names(df_raw) %in% keep)]
  
  setnames(df,"Household ID", 'hhold')
  setnames(df,'survey number', 'survey_no')
  setnames(df,"Q23","hhold_size")
  
  setDT(df)
  df[, hhold := factor(hhold)]
  df[, survey_no := factor(survey_no)]

  names(df) <- sub("Q31_", "", names(df))

  return(df)
}



transform_longer <- function(x){
  
  data <- import_2(x)
  
  # split data into ailment and demographic data frames
  data_ailment <-data[ , -c(103:156)] 
  data_demo <-data[ , c(1,103:156)] 
  
  # transform ailment data into long format
  df_long_a <- data_ailment %>%
    pivot_longer(
      -c(1:3), 
      names_to = "combined", 
      values_to = "has_ailment"
    ) %>%
    separate(combined, into = c("member", "ailment"), sep = "_") %>%
    pivot_wider(
      names_from = ailment, 
      values_from = has_ailment
    ) %>% mutate(mem_id = paste0(hhold,substr(member, 
                                              nchar(member), 
                                              nchar(member)))) %>%
    group_by(hhold) %>%
    mutate(row_within_hhold = row_number()) %>%
    filter(row_within_hhold <= hhold_size) %>%
    ungroup() %>%
    select(-row_within_hhold)
    
    # transform demographic data into long format
    df_long_demo <- data_demo %>%
      pivot_longer(
        cols = c(contains("age"), contains("gender")),
        names_to = "demographic_info",
        values_to = "value") %>% 
      mutate(mem_id = paste0(hhold,substr(demographic_info, 
                                          nchar(demographic_info), 
                                          nchar(demographic_info)))) %>%
      separate(demographic_info, 
               into = c("prefix", "demographic", "member"),
               sep = "_", 
               remove = TRUE) %>%
      pivot_wider(names_from = demographic, 
                  values_from = value,
                  id_cols = mem_id) 
    
     df_merge <- merge(df_long_a,df_long_demo, 
                       by="mem_id",
                       how = "left")
     
     # merge transformed ailment data and demographic data
     df_merge <- df_merge %>% filter(age>=13)
     setnames(df_merge,'Ailment19', 'anxiety_depression')
     setnames(df_merge,'Ailment9', 'asthma')
     setnames(df_merge,'Ailment16', 'cholesterol_problems')
     setnames(df_merge,'Ailment20', 'pre-diabetes')
     setnames(df_merge,'Ailment22', 'diabetes_type2')
     setnames(df_merge,'Ailment28', 'headache_chronic_tension')
     setnames(df_merge,'Ailment29', 'headache_migraine')
     setnames(df_merge,'Ailment30', 'heart_disease')
     setnames(df_merge,'Ailment32', 'hbp')
     setnames(df_merge,'Ailment35', 'insomnia')
     setnames(df_merge,'Ailment43', 'obesity')
     
  
  
  return(df_merge)
  
}


x="ailments/2016/2016_160269_HealthCare_ParsedData.xlsx"
df_ailment_long <- transform_longer(x)

# data <- pblapply(x, transform_longer)
# df1 <- rbindlist(data, idcol = T)
# df1[,.id := .id + 2015]


# read panel---------------------------------------------------------
read_panel <- function(path){
  df <- read_tsv(path)
  df <- df[,c("Household_Cd","Fips_State_Cd","Fips_County_Cd","Panelist_ZipCd","Household_Income")]
  names(df) <- c("hhold","fips_state","fips_county","zip_code","hh_income")
  df$fips_state <- as.factor(formatC(df$fips_state,width=2,flag="0"))
  df$fips_county <- formatC(df$fips_county,width=3,flag="0")
  df$fips_county <- as.factor(paste0(df$fips_state,df$fips_county))
  df$zip_code <- as.factor(formatC(df$zip_code,width=5,flag="0"))
  df$hhold <- as.factor(df$hhold)
  
  #get the income bracket midpoints
  income_ranges <- setNames(c("0-5000", "5000-7999", "8000-9999", "10000-11999", "12000-14999", 
                              "15000-19999", "20000-24999", "25000-29999", "30000-34999", 
                              "35000-39999","40000-44999","45000-49999","50000-59999",
                              "60000-69999","70000-99999","100000-200000"),
                            as.character(c(3,4,6,8,10,11,13,15:19,21,23,26,27)))
  
  # Map income levels to ranges
  df$income_range <- income_ranges[as.character(df$hh_income)]
  
  
  # Calculate midpoints
  calculate_midpoint <- function(range) {
    bounds <- as.numeric(strsplit(range, "-")[[1]])
    return(mean(bounds))
  }
  df$income_midpoint <- sapply(df$income_range, calculate_midpoint)
  return(df)
}

panelist_path <- "2016/Annual_files/panelists_2016.tsv"
panel2016 <- read_panel(panelist_path)



# read gini data----------------------------------------------------

read_gini <- function(path,level){
  df <- read_csv(path)
  names(df) <- c("state","area","fips",paste0("gini",as.character(seq(2011,2021))))
  if (level=="zip"){
    df <- df %>%
      filter(nchar(fips) == 5 & !grepl("county", area, ignore.case = TRUE)) %>% 
      unique() %>%
      rename_with(~paste0("zip_", .), starts_with("gini"))
  }
  else if (level=="county"){
    df <- df %>%
      filter(nchar(fips) == 5 & grepl("county", area, ignore.case = TRUE)) %>% 
      unique() %>%
      rename_with(~paste0("county_", .), starts_with("gini"))
  }
  
  else{
    stop("check the geographical level")
  }
  df <- df %>%
    mutate(across(contains("gini"), as.numeric),
           fips = as.factor(fips))
  
  return(df)
}

gini_path = "gini/0288_Gini_Coefficient_RAND_US_7b59fc3d-7fa6-11ee-9174-0e56f8b00795_001.csv"
gini_zip <- read_gini(gini_path,"zip")
gini_zip <- gini_zip[!is.na(gini_zip$zip_gini2016),]

gini_county <- read_gini(gini_path,"county")
gini_county <- gini_county[!is.na(gini_county$county_gini2016),]



# merge ailment, panel, and Gini data---------------------------------------
df_merge <- merge(df_ailment_long,panel2016,by="hhold")

df_merge <- merge(df_merge,gini_zip[,c('fips','gini_2016')],
                   by.x="zip_code",
                   by.y="fips")
df_merge <- df_merge %>% rename(zip_gini = gini_2016)

model <- glm(obesity ~ age + gender + income_midpoint * zip_gini, 
             data = df_merge, 
             family = binomial)
summary(model)

interact_plot(model, pred = "income_midpoint", modx = "zip_gini",interval = TRUE,
              x.label = "income")

df_mean <- df_merge %>% 
  group_by(gender) %>% 
  summarise(across(7:17, mean, na.rm = TRUE))


