# next, select individual columns (age, sex) in batch 2 data


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

# Setup -------------------------------------------------------------------

# Set the path to the directory you want to search
#path <- 'data/kilts-PanelViewSurvey-ailments'
path <- 'ailments'

# Specify the pattern to match file names
pattern <- 'HealthCare.*.xlsx'

# Find files matching the pattern, including in subdirectories
files <- list.files(
  path = path,
  pattern = pattern,
  recursive = T, 
  full.names = T
)

rm(path, pattern)

# Import batches 2011 to 2015 ---------------------------------------------

import_1 <- function(x) {
  
  df <- read_excel(x)
  
  hhold <- str_subset(names(df), regex('househ(i)?old id', ignore_case = T))
  
  keep <- c(
    hhold,
    'Survey number...2',
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
    'Q8',
    'Q13a_yy',
    'Q14'
  )
  
  df <- df[which(names(df) %in% keep)]
  
  ## Household
  setnames(df, hhold, 'hhold')
  setnames(df, 'Survey number...2', 'survey_no')
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
  setnames(df, 'Q7', 'age')
  setnames(df, 'Q8', 'smoker')
  setnames(df, 'Q13a_yy', 'yob')
  setnames(df, 'Q14', 'sex')
  
  setDT(df)
  df[, hhold := factor(hhold)]
  df[, survey_no := factor(survey_no)]
   
  df[
    ,
    age := factor(
      age,
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
  df[, smoker := factor(smoker, levels = c(0:2), labels = c(NA, 'Yes', 'No'))]
  df[, yob := yob + 1940]
  df[, sex := factor(sex, levels = c(1, 2), labels = c('Male', 'Female'))]
  return(df)
}

data <- pblapply(files[1:5], import_1)
df1 <- rbindlist(data, idcol = T)
df1[,.id := .id + 2010]
df1 <- df1[!df1$age %in% c("1", "2"), ] #remove individuals under 13 years old

# Import batches 2016 to 2021 ---------------------------------------------


import_2 <- function(x) {
  
  df <- read_excel(x)
  names <- names(df)
 
  
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
  #regex_pattern2 <- paste("^Q31.*(", paste(selected_ailments, collapse = "|"), ")$", sep = "")
  #mem_ailment_cols <- grep(regex_pattern2, names, value = TRUE)
 
  # select demographic columns for each member
  mem_demo_start <-  match("Q24_who_mem1", names)
  mem_demo_end <-  match("Q28_height_inches_mem9", names)
  demo_cols <- names[mem_demo_start:mem_demo_end]
  
  keep <- c(
    "Household ID",
    'survey number',
    hhold_ailment_cols,
    #mem_ailment_cols,
    #demo_cols,
    "Q23"
   
 )
  

  df <- df[which(names(df) %in% keep)]
  
  setnames(df,"Household ID", 'hhold')
  setnames(df,'survey number', 'survey_no')
  setnames(df,'Q16_Ailment19', 'anxiety_depression')
  setnames(df,'Q16_Ailment9', 'asthma')
  setnames(df,'Q16_Ailment16', 'cholesterol_problems')
  setnames(df,'Q16_Ailment20', 'pre-diabetes')
  setnames(df,'Q16_Ailment22', 'diabetes_type2')
  setnames(df,'Q16_Ailment28', 'headache_chronic_tension')
  setnames(df,'Q16_Ailment29', 'headache_migraine')
  setnames(df,'Q16_Ailment30', 'heart_disease')
  setnames(df,'Q16_Ailment32', 'hbp')
  setnames(df,'Q16_Ailment35', 'insomnia')
  setnames(df,'Q16_Ailment43', 'obesity') 
  setnames(df,"Q23","hhold_size")
  setDT(df)
  df[, hhold := factor(hhold)]
  df[, survey_no := factor(survey_no)]
  
  # combine 2 headache types into one for consistency
  df$headache <- df$headache_chronic_tension + df$headache_migraine
  df$headache[df$headache == 2] <- 1 
  df <- subset(df, select = -c(headache_chronic_tension, headache_migraine))
  df <- df %>% # make the hhold_size the last column
    select(-hhold_size, everything(), hhold_size)
  return(df)
}


x="ailments/2016/2016_160269_HealthCare_ParsedData.xlsx"
data <- pblapply(x, import_2)
data <- data[[1]]


# read panel--------------------------------------------------
read_panel <- function(path){
  df <- read_tsv(path)
  df <- df[,c("Household_Cd","Fips_State_Cd","Fips_County_Cd","Panelist_ZipCd","Household_Income")]
  names(df) <- c("hhold","fips_state","fips_county","zip_code","hh_income")
  df$fips_state <- formatC(df$fips_state,width=2,flag="0")
  df$fips_county <- formatC(df$fips_county,width=3,flag="0")
  df$fips_county <- paste0(df$fips_state,df$fips_county)
  df$zip_code <- formatC(df$zip_code,width=5,flag="0")
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
  names(df) <- c("state","area","fips",paste0("gini_",as.character(seq(2011,2021))))
  if (level=="zip"){
    df <- df %>%
      filter(nchar(fips) == 5 & !grepl("county", area, ignore.case = TRUE)) %>% 
      unique() 
  }
  else if (level=="county"){
    df <- df %>%
      filter(nchar(fips) == 5 & grepl("county", area, ignore.case = TRUE)) %>% 
      unique() 
  }
  else{
    stop("check the geographical level")
  }
  df <- df %>%
    mutate(across(starts_with("gini"), as.numeric))
  return(df)
}

gini_path = "gini/0288_Gini_Coefficient_RAND_US_7b59fc3d-7fa6-11ee-9174-0e56f8b00795_001.csv"
gini_zip <- read_gini(gini_path,"zip")
gini_zip <- gini_zip[!is.na(gini_zip$gini_2016),]




# aggregated level analysis -------------------------------------------

## combine ailment data with panel data 
ailments_panel2016 <- merge(data,panel2016, by="hhold")


## calculate the zip level occurrence rate
zip_sum <- ailments_panel2016 %>%
  group_by(zip_code) %>%
  summarise(across(3:13, sum, na.rm = TRUE))
zip_sum <- zip_sum %>%
  rename(zip_size = hhold_size)

zip_ailment_gini <- merge(zip_sum,gini_zip[,c("fips","gini_2016")],
                          by.x="zip_code",
                          by.y="fips")

for (i in 2:11) {
  zip_ailment_gini[, i] <- zip_ailment_gini[, i] / zip_ailment_gini[, "zip_size"]
}


## divide zip-code areas to 10 groups based on Gini
zip_ailment_gini$gini_group <- cut(zip_ailment_gini$gini_2016, 
                                      breaks = quantile(zip_ailment_gini$gini_2016, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
                                      include.lowest = TRUE,
                                      labels = FALSE)
df_long <- zip_ailment_gini %>%
  pivot_longer(cols = c(2:11), names_to = "ailments", values_to = "occurrence")

## density plot
ggplot(df_long, aes(x = occurrence)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ ailments, scales = "free") +
  labs(title = "density plots of zip level ailments' occurrence rate",
       x = "zip level ailments occurrence rate",
       y = "Density") +
  theme_minimal()


#prepare the data for average occurrence across Gini decile groups
group_means <- df_long %>%
  group_by(gini_group, ailments) %>%
  summarise(mean_value = mean(occurrence, na.rm = TRUE),
            sem = sd(occurrence, na.rm = TRUE) / sqrt(n()),
            .groups = 'drop')

ggplot(group_means, aes(x = gini_group, y = mean_value)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_value - sem, ymax = mean_value + sem), width = .2) +
  facet_wrap(~ ailments, scales = "free_y") +
  labs(title = "Mean occurrence rate across Gini deciles for 10 ailments",
       x = "Gini deciles",
       y = "Mean occurence rate") +
  theme_minimal()



# household level analysis ----------------------------------------------

hhold_all <- merge(ailments_panel2016,gini_zip[,c("fips","gini_2016")],
                   by.x="zip_code",
                   by.y="fips")
setnames(hhold_all,'gini_2016', 'zip_gini') 

model <- glm(insomnia ~ income_midpoint*zip_gini, data = hhold_all, family = binomial)
summary(model)

interact_plot(model, pred = "income_midpoint", modx = "zip_gini",interval = TRUE,
                          x.label = "income")


#get income rank
hhold_all <- hhold_all %>%
  group_by(zip_code) %>%
  mutate(income_rank = rank(hh_income, ties.method = "min")) %>%
  ungroup()

#check frequency density 
test <- as.data.frame(table(hhold_all$zip_code))
ggplot(test, aes(x = Freq)) + 
  geom_density(adjust=2)






