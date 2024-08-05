# Clean environment
rm(list = ls())

# Load libraries
library(data.table)
library(tidyverse)
library(gridExtra)
setwd("/Users/yeyuqi/Desktop/RA_inequality")

# prepare data files-----------------------------------------------------------

purchase_files <- list.files(
  pattern = "^purchases.*\\.tsv$",
  recursive = T,
  full.names= T
)
purchase_files <- purchase_files[8:17] # select 2011-2020 data

trip_files <- list.files(
  pattern = "^trips.*\\.tsv$",
  recursive = T,
  full.names= T
)
trip_files <- trip_files[8:17]


ppu_files <- list.files(
  pattern = "^ppu_merge_.*\\.csv$",
  recursive = T,
  full.names= T
)


pm <- fread("master/products.tsv", 
            select = c("upc", "upc_ver_uc","product_module_descr"))

# hholds se scores by year-----------------------------------------------------
get_se <- function(i){
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
               select = c("upc", "upc_ver_uc","ppu",
                          "ppu_normalized", "rating_avg","rating_normalized"))
  # merge data
  df <- purchase[pm, on=.(upc, upc_ver_uc),
                 allow.cartesian = T,
                 nomatch = 0L] 
  
  df <- df[trip, on = .(trip_code_uc),
           allow.cartesian = T,
           nomatch = 0L]
  
  df <- df[ppu, on = .(upc,upc_ver_uc),
           allow.cartesian = T,
           nomatch = 0L] #41171097
  
  # remove households whose shopping trips are less than 20 or consumed items are less than 100 in a year
  hh_to_remove <- df[,.(hh_trips = uniqueN(trip_code_uc)),
                     by = household_code][hh_trips <= 20,household_code]
  hh_to_remove2 <- df[,.(hh_items = sum(quantity)),
                      by = household_code][hh_items <= 100,household_code]
  hh_to_remove3 <- unique(c(hh_to_remove,hh_to_remove2))
  
  df <- df[!household_code %in% hh_to_remove3]
  
  # aggregate the same upcs for each household
  df1 <- df[, .(real_price_paid = sum(real_price_paid),
                ppu_nor = mean(ppu_normalized),
                rating = mean(rating_avg),
                rating_nor = mean(rating_normalized),
                module = product_module_descr[1]),
            by = .(household_code, upc, upc_ver_uc)]
  # calculate total expense for each household
  df1 <- df1[, all_expenses := sum(real_price_paid),
             by = .(household_code)]
  # calculate expense proportion of each upc for each household
  df1[, expense_pct :=  real_price_paid/all_expenses]
 
  # calculate se score for each upc 
  df1[, item_se :=  expense_pct*ppu_nor*rating_nor]

  # sum the scores on hhold level and keep the average ppu and rating
  hh <- df1[, .(hh_se = sum(item_se),
                ppu_nor = mean(ppu_nor),
                rating_nor = mean(rating_nor)),
            by = household_code]
  return(hh)
}

df <- pbapply::pblapply(1:10, get_se_log) %>% 
  rbindlist(use.names = F, idcol = T)

df[, .id := factor(.id + 2010)]
names(df)[1:2] <- c("year","hhold")
write_csv(df,"ppu_calculations/temp/data/se_scores.csv")
df <- fread("ppu_calculations/temp/data/se_scores.csv")


# hholds se scores by year*month-----------------------------------------------
get_se_month <- function(i){
  purchase <- fread(purchase_files[i])
  
  purchase <- purchase[, .(total_price_paid = sum(total_price_paid),
                           coupon_value = sum(coupon_value),
                           quantity = sum(quantity)),
                       by = .(trip_code_uc,upc,upc_ver_uc)
  ]
  
  purchase <- purchase[, real_price_paid := total_price_paid-coupon_value]
  purchase <- purchase[real_price_paid != 0]
  
  trip <- fread(trip_files[i],
                select = c("trip_code_uc","household_code","purchase_date")
  )
  
  ppu <- fread(ppu_files[i],
               select = c("upc", "upc_ver_uc","ppu",
                          "ppu_normalized", "rating_avg","rating_normalized"))
  df <- purchase[pm, on=.(upc, upc_ver_uc),
                 allow.cartesian = T,
                 nomatch = 0L] 
  
  df <- df[trip, on = .(trip_code_uc),
           allow.cartesian = T,
           nomatch = 0L]
  df[, month := format(as.Date(purchase_date, format = "%Y-%m-%d"), "%m")]
  
  
  df <- df[ppu, on = .(upc,upc_ver_uc),
           allow.cartesian = T,
           nomatch = 0L] #41171097
  
  hh_to_remove <- df[,.(hh_trips = uniqueN(trip_code_uc)),
                     by = household_code][hh_trips <= 20,household_code]
  hh_to_remove2 <- df[,.(hh_items = sum(quantity)),
                      by = household_code][hh_items <= 100,household_code]
  hh_to_remove3 <- unique(c(hh_to_remove,hh_to_remove2))
  
  df <- df[!household_code %in% hh_to_remove3]
  
  df1 <- df[, .(real_price_paid = sum(real_price_paid),
                ppu_nor = mean(ppu_normalized),
                rating = mean(rating_avg),
                rating_nor = mean(rating_normalized),
                module = product_module_descr[1]),
            by = .(household_code, upc, upc_ver_uc, month)]
  
  
  df1 <- df1[, all_expenses := sum(real_price_paid),
             by = .(household_code,month)]
  # calculate expense proportion of each upc for each household
  df1[, expense_pct :=  real_price_paid/all_expenses]
  df1[, item_se :=  expense_pct*ppu_nor*rating_nor]

  hh <- df1[, .(hh_se = sum(item_se)),
            by = .(household_code,month)]
  return(hh)
}

dfs <- pbapply::pblapply(1:10, get_se_month) %>% 
  rbindlist(use.names = F, idcol = T)

dfs[, .id := factor(.id + 2010)]
names(dfs)[1:2] <- c("year","hhold")

write_csv(dfs, "ppu_calculations/temp/data/month_status_scores.csv")
dfs <- fread("ppu_calculations/temp/data/month_status_scores.csv")



# plots------------------------------------------------------------------------
## plotting average se scores 
p1<- 
  ggplot(df, aes(x = factor(year), y = hh_se)) +
  stat_summary(
    fun = mean,  # Calculate mean
    geom = "line",  
    color = "lightblue",  
    group = 1 
    
  ) +
  stat_summary(
    fun.data = mean_cl_normal, 
    geom = "errorbar",  
    color = "navyblue", 
    width=0.2,
  )+
  theme_classic()+
  labs(x="",y="SE scores",
       title="average SE scores by year")

p2 <- ggplot(dfs, aes(x = factor(month), y = hh_se)) +
  stat_summary(
    fun = mean,  # Calculate mean
    geom = "line",  # Connect means with a line
    color = "lightblue",
    group=1
  ) +
  stat_summary(
    fun.data = mean_cl_normal,  # Calculate mean and normal CI
    geom = "errorbar",  # Use error bars for CI
    width = 0.2,  # Width of the error bars
    color = "navyblue", 
  ) +
  theme_classic() +
  labs(
    x = "month",
    y = "SE Scores",
    title = "average SE score by month"
  )

# merge with panel data--------------------------------------------------------
load("processed_data/panel.RData")
load('other_data/gini/gini.RData')

df[, hhold := factor(hhold)]
df[, year := factor(year)]

panel_se <- panel[df, on = .(year,hhold),
                  nomatch = 0L]

panel_se_gini <- panel_se[gini, on = .(year,zip),
                          allow.cartesian = T,
                          nomatch = 0L]

m <- lmerTest::lmer(hh_se ~ scale(income_midpoint)+ 
                      scale(zip_gini)+
                      scale(mean_head_age)+ 
                      scale(mean_head_education)+
                      head_unemployment
                      race+
                      (1|zip)
                      (1|year),
                    panel_se_gini)


# merge with ailments data-----------------------------------------------------
load("processed_data/df_ailments.RData")

df_ailments_se <- df_ailments[df, on = .(year, hhold),
                              allow.cartesian = T,
                              nomatch = 0L]
df_ailments_se <- df_ailments_se[panel, on = .(year, hhold),
                                 allow.cartesian = T,
                                 nomatch = 0L]
df_ailments_se <- df_ailments_se[gini, on = .(year,zip),
                                 allow.cartesian = T,
                                 nomatch = 0L]
m2 <- glmer(anxiety_depression ~ scale(age) + sex + 
                 scale(income_midpoint)+scale(zip_gini) + 
              scale(hh_se2) +
              race+
              mean_head_education+
              (1|zip)+
              (1|year),  
               family = binomial(link = "logit"), 
               data = df_ailments_se,
               control = glmerControl(optimizer = "bobyqa",
                                      optCtrl = list(maxfun = 100000)))
