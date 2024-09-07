
rm(list = ls())

# Load libraries
library(data.table)
library(tidyverse)
library(readr)


# matching category labels------------------------------------------------------
m <- read_csv("ppu_merge/matching_labels.csv")

# process rating data-----------------------------------------------------------
rating <- read_csv("Consumer perceptions of luxury in product categories_July 29, 2024_08.56.csv")

# remove unimportant columns
rating <- rating %>% select(-c(1:4,6:18))

names(rating)[1:7] <- c("progress","id","race","latino","marital","income","education")

#extract category label
cat_label <- sapply(rating[1,8:ncol(rating)], 
                    function(x) strsplit(x, " - ")[[1]][1])

# rename rating columns with category name
names(rating)[8:ncol(rating)] <- cat_label

rating <- rating[-c(1,2),] #remove rows that are not participants ratings

rating <- rating %>% mutate(
  race = case_when(
    race == "White or Caucasian" ~ "White/Caucasian",  
    race == "Black or African American" ~ "Black/African American",  
    race == "Asian" ~ "Asian",  
    TRUE ~ "other"  # This keeps all other races as they are
  )
)

rating$income <- factor(rating$income,
                        levels = c("Less than $25,000",
                                   "$25,000-$49,999",
                                   "$50,000-$74,999",
                                   "$75,000-$99,999",
                                   "$100,000-$149,999",
                                   "$150,000 or more",
                                   "Prefer not to say"),
                        ordered = T)

#shorten value
rating$education[rating$education=="Graduate or professional degree (MA, MS, MBA, PhD, JD, MD, DDS etc.)"] <- "Graduate or professional degree"

rating$education <- factor(rating$education,
                           levels = c("Some high school or less",
                                      "High school diploma or GED",
                                      "Associates or technical degree",
                                      "Some college, but no degree",
                                      "Bachelor’s degree",
                                      "Graduate or professional degree",
                                      "Prefer not to say"), 
                           ordered = T)

# remove text from ratings
rating <- rating %>%
  mutate(across(.cols = 8:ncol(rating), ~ case_when(
    . == "7 (strongly agree)" ~ "7",
    . == "1 (strongly disagree)" ~ "1",
    TRUE ~ .
  ))) 

# convert rating cols to numeric
rating <- rating %>% 
  mutate(across(.cols = 8:ncol(rating), .fns = as.numeric))

process_data <- function(data) {
  # Calculate mean and standard deviation
  mean_val <- mean(data)
  sd_val <- sd(data)
  
  # Identify outliers (more than 2 standard deviations from the mean)
  non_outliers <- data[abs(data - mean_val) <= 2 * sd_val]
  
  # Calculate new mean and standard deviation without outliers
  new_mean <- mean(non_outliers)
  new_sd <- sd(non_outliers)
  n <- length(non_outliers)
  
  # Calculate 95% confidence interval for the mean
  error_margin <- qt(0.975, df = n - 1) * new_sd / sqrt(n)
  ci_lower <- new_mean - error_margin
  ci_upper <- new_mean + error_margin
  
  # Return results as a list
  return(c(mean = new_mean, sd = new_sd, CI_lower = ci_lower, CI_upper = ci_upper))
}

# Apply the function to each column of the dataframe and store the results
results_list <- lapply(rating[, 8:ncol(rating)], process_data)

# Convert the list of results into a data frame
results_df <- as.data.frame(do.call(rbind, results_list))

results_df$new_label <- rownames(results_df) 

avg_rating <- results_df %>% 
  mutate(rating_normalized = (mean-min(mean))/(max(mean)-min(mean)))

# merge product category file with average ratings
m1 <- merge(m, avg_rating[,c("new_label","rating_normalized")],
            by = 'new_label',
            all.x=T)


#write_csv(m1,"matching_labels_with_rating.csv")





