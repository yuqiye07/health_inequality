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
library(tidycensus)
library(lme4)
library(AER)

setwd("/Users/yeyuqi/Desktop/RA_inequality/")

path <- 'ailments'
# Specify the pattern to match file names
pattern <- 'ParsedData.*\\.xlsx$'

files <- list.files(
  path = path,
  pattern = pattern,
  recursive = T, 
  full.names = T
)

# Import batches 2011 to 2015 ---------------------------------------------
import_1 <- function(x) {
  
  df <- read_excel(x)
  
  year <- strsplit(x, "/")[[1]][2]
  
  hhold <- str_subset(names(df), regex('househ(i)?old id', ignore_case = T))
  
  keep <- c(
    hhold,
    'Q1_Ailment5 Anxiety/Depression',
    'Q1_Ailment8 Asthma',
    'Q1_Ailment11 Cholesterol Problems',
    'Q1_Ailment13 Pre-Diabetes',
    'Q1_Ailment15 Diabetes Type II',
    'Q1_Ailment20 Headache - chronic/tension/migraine',
    'Q1_Ailment21 Heart Disease/Heart Attack/Angina/Heart Failure',
    'Q1_Ailment22 High Blood Pressure/Hypertension',
    'Q1_Ailment24 Insomnia/Sleepless',
    'Q1_Ailment30 Obesity/Overweight',
    "Q7 Age Break",
    #'Q8',
    'Q13a_yy',
    'Q14'
  )
  
  df <- df[which(names(df) %in% keep)]
  
  ## Household
  setnames(df, hhold, 'hhold')
  setnames(df, 'Q1_Ailment5 Anxiety/Depression', 'anxiety_depression')
  setnames(df, 'Q1_Ailment8 Asthma', 'asthma')
  setnames(df, 'Q1_Ailment11 Cholesterol Problems','cholesterol_problems')
  setnames(df, 'Q1_Ailment13 Pre-Diabetes','pre-diabetes')
  setnames(df, 'Q1_Ailment15 Diabetes Type II','diabetes_type2')
  setnames(df, 'Q1_Ailment20 Headache - chronic/tension/migraine', 'headache')
  setnames(df, 'Q1_Ailment21 Heart Disease/Heart Attack/Angina/Heart Failure', 'heart_disease')
  setnames(df, 'Q1_Ailment22 High Blood Pressure/Hypertension', 'hbp')
  setnames(df, 'Q1_Ailment24 Insomnia/Sleepless', 'insomnia')
  setnames(df, 'Q1_Ailment30 Obesity/Overweight', 'obesity')
  setnames(df, 'Q7 Age Break', 'age_break')
  #setnames(df, 'Q8', 'smoker') # not in batch2 data
  setnames(df, 'Q13a_yy', 'yob')
  setnames(df, 'Q14', 'gender')
  
  setDT(df)
  df[, hhold := factor(hhold)]
  
  df[
    ,
    age_break := factor(
      age_break,
      levels = 1:10,
      labels = c(
        'Under 6 years old',
        '6 to 12 years old',
        '13 to 17 years old',
        '18 to 20 years old',
        '21 to 24 years old',
        '25 to 34 years old',
        '35 to 44 years old',
        '45 to 54 years old',
        '55 to 64 years old',
        '65 years or older'
      )
    )
  ]
  
  df[, year := year]
  setcolorder(df, c("year", setdiff(names(df), "year")))
  #df[, smoker := factor(smoker, levels = c(0:2), labels = c(NA, 'Yes', 'No'))]
  df[, yob := yob + 1940]
  df[, gender := factor(gender, levels = c(1, 2), labels = c('Male', 'Female'))]
  df <- df[!df$age_break %in% c('Under 6 years old', 
                                '6 to 12 years old'), ] #remove individuals under 13 years old
  
  # Remove rows where all selected ailments are NA
  df <- df[rowSums(!is.na(df[, ..ailment_names])) > 0, ]
  
  df$age <- ifelse(is.na(df$year) | is.na(df$yob), 
                        NA, 
                        as.numeric(df$year) - df$yob)
  df[, year := factor(year)]
  
  return(df)
}

df11_15 <- pblapply(files[1:2], import_1)
df11_15 <- rbindlist(df11_15, idcol = F)



# Import batches 2016 to 2021 ---------------------------------------------

import_2 <- function(x) {
  if (grepl("2020", x)) {
    df_raw <- read_excel(x,sheet=2)
  } else {df_raw <- read_excel(x)}
  
  names <- names(df_raw)
  year <- strsplit(x, "/")[[1]][2]
  if (!year %in% as.character(2016:2021)){
    stop("this function only applies to data from 2016 to 2021")
  } 
  
  
  selected_ailments2016 <- c('Ailment9',
                             'Ailment16',
                             'Ailment19',
                             'Ailment20',
                             'Ailment22',
                             'Ailment28',
                             'Ailment29',
                             'Ailment30',
                             'Ailment32',
                             'Ailment35',
                             'Ailment43')
  selected_ailments_other <- c('Ailment9',
                               'Ailment16',
                               'Ailment19',
                               'Ailment20',
                               'Ailment22',
                               'Ailment29',
                               'Ailment30',
                               'Ailment31',
                               'Ailment33',
                               'Ailment36',
                               'Ailment44')
  if (year == "2016") {
    selected_ailments <- selected_ailments2016
  } else {
    selected_ailments <- selected_ailments_other
  }
  
  if (year %in% c('2016', '2017', '2018')) {
    hh_id <- "Household ID"
    
    # these columns are ailment occurrence for each household member
    reg_pattern1 <- paste0("mem.*(", paste(selected_ailments, collapse = "|"), ")")
    mem_ailment_cols <- grep(reg_pattern1, names, value = TRUE)
    
    # select demographic columns for each member
    reg_pattern2 <- "who_mem|age_mem|gender_mem"
    demo_cols <- grep(reg_pattern2, names, value = TRUE)
    
    keep <- c(
      hh_id,
      mem_ailment_cols,
      demo_cols
    )
    
    
    df <- df_raw[which(names(df_raw) %in% keep)]
    
    names(df) <- sub("Q31_", "", names(df))
    names(df) <- sub("Q18_", "", names(df))
    names(df) <- sub("Q24_", "", names(df))
    names(df) <- sub("Q25_", "", names(df))
    names(df) <- sub("Q26_", "", names(df))
  }  
  
  if (year %in% c("2019","2020","2021")) {
    if (year == "2020") {
      hh_id <- "Household ID HHID"} else {hh_id <- "PANELISTID (HHID)"}
    
    ailments_index_start <- c(73,136,163,172,190,253,262,271,289,316,388)
    
    ailments_index <- unlist(lapply(ailments_index_start, function(x) x:(x+8)))
    end_pattern <- paste(ailments_index, collapse="|")
    reg_pattern1 <- paste0("^18.*_(", end_pattern, "))$")
    mem_ailment_cols <- grep(reg_pattern1, names, value = TRUE)
    
    reg_pattern2 <- "Q10_[BCD]"
    demo_cols <- grep(reg_pattern2, names, value = TRUE)
    
    keep <- c(
      hh_id,
      mem_ailment_cols,
      demo_cols
    )
    
    df <- df_raw[which(names(df_raw) %in% keep)]
    
    # Initialize an empty list to store mappings
    name_mappings <- list()
    
    # Generate mappings
    for (i in seq_along(ailments_index_start)) {
      start_index <- ailments_index_start[i]
      ailment_name <- selected_ailments[i]
      
      for (j in 0:8) {
        old_name <- paste0("18 (Q9_A_", start_index + j,")")
        new_name <- paste0("mem", j + 1, "_", ailment_name)
        name_mappings[[old_name]] <- new_name
      }
    }
    
    for (old_name in names(name_mappings)) {
      setnames(df, old_name, name_mappings[[old_name]])
    }
    
    
    trans_demo_names <- sapply(names(df), function(name) {
      if (startsWith(name, "HHmembersQ17 (Q10_")) {
        name <- sub("^HHmembersQ17 \\(Q10_", "", name)
        name <- sub("B_", "who_mem", name)
        name <- sub("C_", "age_mem", name)
        name <- sub("D_", "gender_mem", name)
        name <- sub(")$", "", name)
      }
      return(name)
    })
    
    # Update the demographic column names 
    names(df) <- trans_demo_names
    
  }
  
  
  names(df)[1] <- 'hhold'
  
  setDT(df)
  df[, hhold := factor(hhold)]
  
  df[, year := year]
  setcolorder(df, c("year", setdiff(names(df), "year")))
  
  
  return(df)
}


transform_longer <- function(x){
  
  data <- import_2(x)
  year <- strsplit(x, "/")[[1]][2]
  # split data into ailment and demographic data frames
  data_ailment <- data[ , c(1:101)] 
  
  data_demo <-data[ , c(1:2,102:128)] 
  
  # transform ailment data into long format
  df_long_a <- data_ailment %>%
    pivot_longer(
      -c(1:2), 
      names_to = "combined", 
      values_to = "has_ailment"
    ) %>%
    separate(combined, into = c("member", "ailment"), sep = "_") %>%
    pivot_wider(
      names_from = ailment, 
      values_from = has_ailment
    ) %>% mutate(mem_id = paste0(hhold,substr(member, 
                                              nchar(member), 
                                              nchar(member))))  
  
  setnames(df_long_a,'Ailment9', 'asthma')
  setnames(df_long_a,'Ailment16', 'cholesterol_problems')
  setnames(df_long_a,'Ailment19', 'anxiety_depression')
  setnames(df_long_a,'Ailment20', 'pre-diabetes')
  setnames(df_long_a,'Ailment22', 'diabetes_type2')
  if (year == "2016"){
    setnames(df_long_a,'Ailment28', 'headache_chronic_tension')
    setnames(df_long_a,'Ailment29', 'headache_migraine')
    setnames(df_long_a,'Ailment30', 'heart_disease')
    setnames(df_long_a,'Ailment32', 'hbp')
    setnames(df_long_a,'Ailment35', 'insomnia')
    setnames(df_long_a,'Ailment43', 'obesity')} else {
      setnames(df_long_a,'Ailment29', 'headache_chronic_tension')
      setnames(df_long_a,'Ailment30', 'headache_migraine')
      setnames(df_long_a,'Ailment31', 'heart_disease')
      setnames(df_long_a,'Ailment33', 'hbp')
      setnames(df_long_a,'Ailment36', 'insomnia')
      setnames(df_long_a,'Ailment44', 'obesity')}
  
  
  df_long_a <- df_long_a %>% # combine 2 headache columns
    mutate(headache = headache_chronic_tension + headache_migraine) %>% 
    mutate(headache = if_else(headache==2,1,headache)) %>% 
    select(-c(headache_chronic_tension,headache_migraine)) %>% 
    select(-mem_id, everything(), mem_id) # make the mem_id the last column
  
  
  # transform demographic data into long format
  df_long_demo <- data_demo %>%
    pivot_longer(
      cols = c(contains("who"),contains("age"), contains("gender")),
      names_to = "demographic_info",
      values_to = "value") %>% 
    mutate(mem_id = paste0(hhold,substr(demographic_info, 
                                        nchar(demographic_info), 
                                        nchar(demographic_info)))) %>%
    separate(demographic_info, 
             into = c("demographic", "member"),
             sep = "_", 
             remove = TRUE) %>%
    pivot_wider(names_from = demographic, 
                values_from = value,
                id_cols = mem_id) %>% 
    filter(who!= 0)
  
  df_merge <- merge(df_long_a,df_long_demo, 
                    by="mem_id") %>% 
    filter(age>=13) %>% 
    rowwise() %>%
    filter(!all(is.na(c_across(5:14)))) %>%
    ungroup()
  
  df_merge <- df_merge[df_merge$gender!=0,] #remove people who don't report gender
  df_merge$gender <- factor(df_merge$gender,
                           levels = c(1,2),
                           labels = c("Male","Female"))
  df_merge$hhold <- factor(df_merge$hhold)
  df_merge$year <- factor(df_merge$year)
  return(df_merge)
  
}


ailment_names <- c('asthma','cholesterol_problems','anxiety_depression',
                   'pre-diabetes','diabetes_type2','headache','heart_disease',
                   'hbp','insomnia','obesity')



df16_21 <- pblapply(files[c(6,10)], transform_longer)
df16_21 <- rbindlist(df16_21, idcol = F)
df16_21 <- df16_21 %>%
  mutate(age_break = case_when(
    age < 6 ~ 1,
    age >= 6 & age <= 12 ~ 2,
    age >= 13 & age <= 17 ~ 3,
    age >= 18 & age <= 20 ~ 4,
    age >= 21 & age <= 24 ~ 5,
    age >= 25 & age <= 34 ~ 6,
    age >= 35 & age <= 44 ~ 7,
    age >= 45 & age <= 54 ~ 8,
    age >= 55 & age <= 64 ~ 9,
    age >= 65 ~ 10
  ))


df16_21$age_break <- factor(df16_21$age_break,
                            levels = 1:10,
                            labels = c(
                              'Under 6 years old',
                              '6 to 12 years old',
                              '13 to 17 years old',
                              '18 to 20 years old',
                              '21 to 24 years old',
                              '25 to 34 years old',
                              '35 to 44 years old',
                              '45 to 54 years old',
                              '55 to 64 years old',
                              '65 years or older'
                            )
)



# combine 2 batches---------------------------------------------------------
df1 <- df11_15 %>% select(c("year","hhold",ailment_names,"age","age_break","gender"))
df2 <- df16_21 %>% select(c("year","hhold",ailment_names,"age","age_break","gender"))

df1 <- data.table(df1)
df2 <- data.table(df2)

df_ailments <- rbindlist(list(df1,df2), idcol = F, use.names = F)

save(df_ailments,file="df_ailments.Rdata")





