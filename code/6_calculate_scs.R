


# Clean environment
rm(list = ls())

# Load libraries
library(data.table)
library(tidyverse)


# prepare data files------------------------------------------------------------

purchase_files <- list.files(
  path = "kilts_data/purchases",
  pattern = "^purchases.*\\.tsv$",
  recursive = T,
  full.names= T
)

# keep the selected years 
#purchase_files <- purchase_files[8:17]

trip_files <- list.files(
  path = "kilts_data/trips",
  pattern = "^trips.*\\.tsv$",
  recursive = T,
  full.names= T
)

# keep the selected years 
#trip_files <- trip_files[8:17]


ppu_files <- list.files(
  path = "ppu_data",
  
  recursive = T,
  full.names= T
)


# read product data
pm <- fread("kilts_data/master/products.tsv", 
            select = c("upc", "upc_ver_uc","product_module_descr"))

# read rating data, which is produced from 5_survey_ratings.R
df_ratings <- fread("matching_labels_with_rating.csv")  

# function for calculating scs--------------------------------------------------

get_scs <- function(i){
  purchase <- fread(purchase_files[i])
  
  # aggregate rows of the same upcs in the same trip
  purchase <- purchase[, .(total_price_paid = sum(total_price_paid),
                           coupon_value = sum(coupon_value),
                           quantity = sum(quantity)),
                       by = .(trip_code_uc,upc,upc_ver_uc)
  ]
  
  purchase[, real_price_paid := total_price_paid-coupon_value]
  purchase <- purchase[real_price_paid != 0]
  
  
  trip <- fread(trip_files[i],
                select = c("trip_code_uc","household_code","purchase_date")
  )
  
  
  ppu <- fread(ppu_files[i],
               select = c("upc", 
                          "upc_ver_uc",
                          #"ppu",
                          "ppu_normalized"))
  
  
  # merge data
  df <- purchase[pm, on=.(upc, upc_ver_uc),
                 allow.cartesian = T,
                 nomatch = 0L] 
  
  df <- df[df_ratings, on=.(product_module_descr),
                 allow.cartesian = T,
                 nomatch = 0L] 
  
  df <- df[trip, on = .(trip_code_uc),
           allow.cartesian = T,
           nomatch = 0L]
  
  df <- df[ppu, on = .(upc,upc_ver_uc),
           allow.cartesian = T,
           nomatch = 0L] 
  
  df_ppu <- df_ppu[!is.na(ppu)]
  
  # rescale ppu using min max standardisation
  df_ppu[, ppu_normalized := (ppu - min(ppu)) / (max(ppu) - min(ppu)), 
         by = product_module_descr]
  
  # remove households whose shopping trips are less than 20 or consumed items 
  # are less than 100 in a year, around 1% households are removed
  hh_to_remove <- df[,.(hh_trips = uniqueN(trip_code_uc)),
                     by = household_code][hh_trips <= 20,household_code]
  hh_to_remove2 <- df[,.(hh_items = sum(quantity)),
                      by = household_code][hh_items <= 100,household_code]
  hh_to_remove3 <- unique(c(hh_to_remove,hh_to_remove2))
  
  df <- df[!household_code %in% hh_to_remove3]
  
  # aggregate items of the same upc for each household
  df1 <- df[, .(real_price_paid = sum(real_price_paid),
                ppu_nor = mean(ppu_normalized),
                #rating = mean(rating_avg),
                rating_nor = mean(rating_normalized),
                module = product_module_descr[1]),
            by = .(household_code, upc, upc_ver_uc)]
  
  # calculate total expenditure for each household
  df1[, all_expenses := sum(real_price_paid),
      by = .(household_code)]
  # calculate expense proportion of each upc for each household
  df1[, expense_pct :=  real_price_paid/all_expenses]
  
  # calculate scs score for each upc 
  df1[, item_scs :=  expense_pct*ppu_nor*rating_nor]

  # sum the scores on household level 
  hh <- df1[, .(hh_scs= sum(item_scs)),
            by = household_code]
  return(hh)
}


# generate status consumption score -------------------------------------------- 
df_scs <- pbapply::pblapply(1:10, get_scs) %>% 
  rbindlist(use.names = F, idcol = T)

df_scs[, .id := factor(.id + 2010)]
names(df_scs)[1:2] <- c("year","hhold")

