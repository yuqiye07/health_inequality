# Clean environment
rm(list = ls())

# Load libraries
library(data.table)
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(lme4)
library(lmerTest)
library(wesanderson)


# merge with panel data---------------------------------------------------------
load("panel.RData") # produced from 2_process_panel.R 
load('gini.RData') # produced from 1_get_gini.R
load("df_scs.RData") # produced from 6_calculate_scs.R

# merge panel data with scs 
panel_scs <- panel[df_scs, on = .(year,hhold),
                   nomatch = 0L]

#standardize scs within the same income group 
panel_scs[,scs := (hh_scs - min(hh_scs))/(max(hh_scs)-min(hh_scs)),
          by = .(household_income,year)]

#take square root of scs to reduce skewness
panel_scs[, scs := sqrt(scs)] 

dfm <- panel_scs[gini, on = .(year,zip),
                 allow.cartesian = T,
                 nomatch = 0L]

# model scs as a function as Gini
m <- lmer(scs ~ scale(zip_gini)+
            scale(mean_head_age)+ 
            scale(mean_head_education)+ 
            race+
            female_unemployment+
            male_unemployment+
            (1|zip)+
            (1|year),
          dfm)

summary(m)


# merge with ailment data -----------------------------------------------------
load("df_ailments_adults.RData") # produced from 3_clean_ailment_data.R


dfm2 <- df_ailments[panel_scs, on = .(year, hhold),
                              allow.cartesian = T,
                              nomatch = 0L]

dfm2 <- dfm2[gini, 
             on = .(year,zip),
             allow.cartesian = T,
             nomatch = 0L]


dfm2$county <- factor(dfm2$county)
dfm2$state <- factor(dfm2$state)

# models-----------------------------------------------------------------------
## define function to fit models reported in table 1.
## specification1: ailment ~ Gini + covariates
## specification2: ailment ~ Gini + SCS + covariates
## specification3: ailment ~ Gini * log income + SCS + covariates

fit_model <- function(ailment = c("insomnia","anxiety_depression","obesity"),
                      specification = c(1,2,3)){
  if(specification==1){
    ivs <- "~scale(age)+sex +scale(zip_gini)+scale(log_income)+race+(1|zip)+(1|year)"
  } else if(specification == 2){
    ivs <- "~scale(age)+sex +scale(zip_gini)+scale(log_income)+scale(scs)+race+(1|zip)+(1|year)"
  } else if(specification == 3){
    ivs <- "~scale(age)+sex +scale(log_income)+scale(zip_gini)*scale(scs)+race+(1|zip)+(1|year)"
  }
  formula_str <- paste(ailment,ivs)
  formula <- as.formula(formula_str)
  model <- glmer(formula,  
                 family = binomial(link = "logit"), 
                 na.action = na.exclude,
                 data = dfm2,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 100000)))
  return(model)
}

# plots------------------------------------------------------------------------
## plot interaction between Gini and SCS

plot_interaction <- function(model,
                             title,
                             legend_show=F){
  p <- interact_plot(model,
                             pred = "scs", 
                             modx = "zip_gini",
                             interval = F,
                             colors = wes_palette(n=5,"Zissou1")[c(2,3,5)],
                             vary.lty = F,
                             legend.main = "Zip Gini",
                             data= df_ailments_se)+
    
    theme_classic()+
    labs(x = "SCS",
         y = "Predicted probability",
         title = title)
  if (legend_show==F){
    p <- p+
      theme(legend.position = "none")
  }
  return(p)
}

# fit 3 models using specification3 (Gini * SCS)
model1 <- fit_model("insomnia", 3)
model2 <- fit_model("anxiety_depression", 3)
model3 <- fit_model("obesity", 3)

# create Figure 2 (interaction plot of Gini * SCS)
p1 <- plot_interaction(model1,
                       title = "(a) Insomnia")
p2 <- plot_interaction(model2,
                       title = "(b) Anxiety/Depression")
p3 <- plot_interaction(model3,
                       title = "(c) Obesity")
p_legend <- plot_interaction(model3,
                             title = "(c) Obesity",
                             legend_show = T
                             )
legend <- lemon::g_legend(p_legend)

p_all <- grid.arrange(p1,p2,p3,ncol =3)

p <- grid.arrange(p_all,legend, ncol=2,widths=c(3,0.3))


