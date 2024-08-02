library(dplyr)
library(data.table)
library(pbapply)
library(stringr)
library(tidycensus)

# Functions ---------------------------------------------------------------

import_hholds <- function(year) {
  
  if (year <= 2020) {
    
    # Get file
    file <- paste(
      # 'data',
      # 'nielsen_extracts',
      # 'HMS',
      year,
      'Annual_Files',
      sprintf('panelists_%d.tsv', year),
      sep = '/'
    )
    
    # Import file
    hholds <- fread(
      file,
      select = c(
        Household_Cd = 'factor',
        Household_Income = 'character',
        Household_Size = 'numeric',
        Type_Of_Residence = 'character',
        Household_Composition = 'character',
        Age_And_Presence_Of_Children = 'character',
        Male_Head_Age = 'character',
        Female_Head_Age = 'character',
        Male_Head_Employment = 'character',
        Female_Head_Employment = 'character',
        Male_Head_Education = 'character',
        Female_Head_Education = 'character',
        Marital_Status = 'character',
        Race = 'character',
        Panelist_ZipCd = 'character',
        Fips_State_Cd = 'character',
        Fips_County_Cd = 'character'
      )
    )
    
    names(hholds) <- str_to_lower(names(hholds))
    
  } else if (year >= 2021) {
    
    # Get file
    file <- paste(
      # 'data',
      # 'nielsen_extracts',
      # 'HMS',
      year,
      'Annual_Files',
      'panelist.tsv',
      sep = '/'
    )
    
    # Import file
    hholds <- fread(
      file,
      select = c(
        household_code = 'factor',
        household_income = 'character',
        household_size = 'numeric',
        type_of_residence = 'character',
        household_composition = 'character',
        age_and_presence_of_children = 'character',
        male_head_age = 'character',
        female_head_age = 'character',
        male_head_employment = 'character',
        female_head_employment = 'character',
        male_head_education = 'character',
        female_head_education = 'character',
        marital_status = 'character',
        race = 'character',
        panelist_zip_code = 'character',
        fips_state_code = 'character',
        fips_county_code = 'character'
      )
    )
    
  }
  
  # Reformat file
  income_brackets <- c(
    `Under $5000`       = '3',
    `$5000-$7999`       = '4',
    `$8000-$9999`       = '6',
    `$10,000-$11,999`   = '8',
    `$12,000-$14,999`   = '10',
    `$15,000-$19,999`   = '11',
    `$20,000-$24,999`   = '13',
    `$25,000-$29,999`   = '15',
    `$30,000-$34,999`   = '16',
    `$35,000-$39,999`   = '17',
    `$40,000-$44,999`   = '18',
    `$45,000-$49,999`   = '19',
    `$50,000-$59,999`   = '21',
    `$60,000-$69,999`   = '23',
    `$70,000-$99,999`   = '26',
    `$100,000+`         = '27',
    #in 2006-1009 data, the income range are coded more specifically for income above $100k. 
    #To maintain consistency with other years, I have labeled all such incomes as '$100,000+'.
    `$100,000+`         = '28',
    `$100,000+`         = '29',
    `$100,000+`         = '30'
    
    # `$100,000-$124,999` = '27',
    # `$125,000-$149,999` = '28',
    # `$150,000-$199,999` = '29',
    # `$200,000+`         = '30'
  )
  
  hholds[
    ,
    household_income := factor(
      household_income,
      levels = unname(income_brackets),
      labels = names(income_brackets)
    )
  ]
  # add income midpoint
  calculate_midpoint <- function(range) {
    # Remove "$" and ","
    range <- gsub("\\$", "", range)
    range <- gsub(",", "", range)
    range <- gsub("Under 5000", "0-5000", range)
    range <- gsub("100000\\+", "100000-200000", range)#assume an upper limit
    # Split the range and calculate the average
    parts <- strsplit(range, "-")[[1]]
    lower <- as.numeric(parts[1])
    upper <- as.numeric(parts[2])
    midpoint <- (lower + upper) / 2
    return(midpoint)
    
  }
  
  # Add the midpoint column
  hholds <- hholds %>%
    mutate(income_midpoint = sapply(household_income, calculate_midpoint))
  
  ## Type of Residence
  housing_types <- c(
    `One Family House` = '1',
    `One Family House (Condo/Coop)` = '2',
    `Two Family` = '3',
    `Two Family House (Condo/Coop)` = '4',
    `Three+ Family House` = '5',
    `Three+ Family House (Condo/Coop)` = '6',
    `Mobile Home or Trailer` = '7'
  )
  
  hholds[
    , 
    type_of_residence := factor(
      type_of_residence, 
      levels = unname(housing_types), 
      labels = names(housing_types)
    )
  ]
  
  ## Household Composition
  living_arrangements <- c(
    `Married` = '1',
    `Female Head Living with Others Related` = '2',
    `Male Head Living with Others Related` = '3',
    `Female Living Alone` = '5',
    `Female Living with Non-Related` = '6',
    `Male Living Alone` = '7',
    `Male Living with Non-Related` = '8'
  )
  
  hholds[
    , 
    household_composition := factor(
      household_composition, 
      levels = unname(living_arrangements), 
      labels = names(living_arrangements)
    )
  ]
  
  ## Age and Presence of Children
  child_age_categories <- c(
    `Under 6 only` = '1',
    `6-12 only` = '2',
    `13-17 only` = '3',
    `Under 6 & 6-12` = '4',
    `Under 6 & 13-17` = '5',
    `6-12 & 13-17` = '6',
    `Under 6 & 6-12 & 13-17` = '7',
    `No Children Under 18` = '9'
  )
  
  hholds[
    , 
    age_and_presence_of_children := factor(
      age_and_presence_of_children, 
      levels = unname(child_age_categories), 
      labels = names(child_age_categories)
    )
  ]
  
  ## Male and Female Head Age
  age_categories_heads <- c(
    `Under 25 Years` = '1',
    `25-29 Years` = '2',
    `30-34 Years` = '3',
    `35-39 Years` = '4',
    `40-44 Years` = '5',
    `45-49 Years` = '6',
    `50-54 Years` = '7',
    `55-64 Years` = '8',
    `65+ Years` = '9',
    `No Male/Female Head` = '0'
  )
  
  hholds[
    , 
    male_head_age := factor(
      male_head_age, 
      levels = unname(age_categories_heads), 
      labels = names(age_categories_heads)
    )
  ]
  
  hholds[
    , 
    female_head_age := factor(
      female_head_age, 
      levels = unname(age_categories_heads), 
      labels = names(age_categories_heads)
    )
  ]
  
  ## Male and Female Head Employment
  employment_hours <- c(
    `Under 30 hours` = '1',
    `30-34 hours` = '2',
    `35+ hours` = '3',
    `Not Employed for Pay` = '9',
    `No Male/Female Head` = '0'
  )
  
  hholds[
    , 
    male_head_employment := factor(
      male_head_employment, 
      levels = unname(employment_hours), 
      labels = names(employment_hours)
    )
  ]
  
  hholds[
    , 
    female_head_employment := factor(
      female_head_employment, 
      levels = unname(employment_hours), 
      labels = names(employment_hours)
    )
  ]
  
  ## Male and Female Head Education
  education_levels <- c(
    `Grade School` = '1',
    `Some High School` = '2',
    `Graduated High School` = '3',
    `Some College` = '4',
    `Graduated College` = '5',
    `Post College Grad` = '6',
    `No Male/Female Head or Unknown` = '0'
  )
  
  hholds[
    , 
    male_head_education := factor(
      male_head_education, 
      levels = unname(education_levels), 
      labels = names(education_levels)
    )
  ]
  
  hholds[
    , 
    female_head_education := factor(
      female_head_education, 
      levels = unname(education_levels), 
      labels = names(education_levels)
    )
  ]
  
  ## Marital status
  married_status <- c(
    `Married` = '1',
    `Widowed` = '2',
    `Divorced/Separated` = '3',
    `Single` = '4'
  )
  
  hholds[
    , 
    marital_status := factor(
      marital_status, 
      levels = unname(married_status), 
      labels = names(married_status)
    )
  ]
  
  ## Race
  racial_categories <- c(
    `White/Caucasian` = '1',
    `Black/African American` = '2',
    `Asian` = '3',
    `Other` = '4'
  )
  
  hholds[
    , 
    race := factor(
      race, 
      levels = unname(racial_categories), 
      labels = names(racial_categories)
    )
  ]
  
  return(hholds)
}


# Import data -------------------------------------------------------------

df <- pblapply(c(2004:2020), import_hholds) %>% 
  rbindlist(use.names = F, idcol = T)

df[, .id := factor(.id + 2003)]

setnames(df, '.id', 'year')
setnames(df,'household_cd',"hhold")
setnames(df,'panelist_zipcd','zip')
setnames(df,'fips_county_cd','county')
setnames(df,'fips_state_cd','state')

df[, state := sprintf("%02d", as.numeric(state))]
df[, county := sprintf("%03d", as.numeric(county))]

df$county <- paste0(df$state, df$county)

panel <- df

# calculate household head age midpoint-------------------------------------
age_midpoint <- function(age_range) {
  # Convert to character if it is a factor
  age_range <- as.character(age_range)
  
  # Initialize the result vector
  result <- numeric(length(age_range))
  
  # Assign values based on conditions
  result[age_range == "No Male/Female Head"] <- NA
  result[age_range == "Under 25 Years"] <- 20
  result[age_range == "65+ Years"] <- 70
  range_indices <- grepl("-", age_range)
  if (any(range_indices)) {
    # Extract and process numeric ranges
    range_strings <- age_range[range_indices]
    numeric_ranges <- gsub("[^0-9-]", "", range_strings)  # Remove non-numeric characters except dash
    result[range_indices] <- sapply(strsplit(numeric_ranges, "-"), function(x) {
      nums <- as.numeric(x)
      mean(nums)
    })
  }
  
  # Handle unrecognized formats by assigning NA
  
  return(result)
}


panel[, c("female_age_midpoint", "male_age_midpoint") := 
        .(age_midpoint(female_head_age), age_midpoint(male_head_age))]

panel[, mean_head_age := rowMeans(.SD, na.rm = TRUE), 
      .SDcols = c("female_age_midpoint", "male_age_midpoint")]


panel[, c("female_age_midpoint", "male_age_midpoint") := NULL]

# head unemployment------------------------------------------------------------

## for households whose both heads are unemployed, or those who only has one hhold head and is unemployed, we set "TRUE" to "head_unemployment", other wise "False"
panel[, head_unemployment := (
  (male_head_employment == "Not Employed for Pay" & 
     female_head_employment %in% c("Not Employed for Pay", "No Male/Female Head")) |
    (male_head_employment == "No Male/Female Head" & 
       female_head_employment == "Not Employed for Pay")
)]

# head education years----------------------------------------------------------
##calculate the average education years for household heads
mapping <- setNames(c(6, 10,12,14,16,18,NA), 
                    levels(panel$female_head_education))
panel[, female_education_years := 
        mapping[as.character(panel$female_head_education)]]

panel[, male_education_years := 
        mapping[as.character(panel$male_head_education)]]

panel[, mean_head_education := rowMeans(.SD, na.rm = TRUE), 
      .SDcols = c("female_education_years", "male_education_years")]

panel[, c("female_education_years", "male_education_years") := NULL]


save(panel, file = "panel.RData")
load("processed_data/panel.RData")
