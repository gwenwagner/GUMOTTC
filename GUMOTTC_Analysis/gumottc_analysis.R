# gumottc_analysis.R
## script to analyze the leaf chemistry data for GUMO,
## comparing the 1970 samples to the 2020 samples
## author: gwen wagner and nick smith

# load libraries
library(tidyverse)
## library(dplyr)
## install.packages('lme4')
library(lme4)
library(car)
library(emmeans)
library(grid)

# load and compile all data
## load characters data
gumottc_characters <- read.csv('../data/gumottc_characters.csv')
head(gumottc_characters)

## load cn data
gumottc_cn <- read.csv('../data/gumottc_cn.csv')
head(gumottc_cn)

## load isotope data
gumottc_isotope <- read.csv('../data/gumottc_isotope.csv')
head(gumottc_isotope)

## create master data
gumottc_data <- left_join(gumottc_characters, gumottc_cn, by= 'id') %>%
                            left_join(., gumottc_isotope, by= 'id')
head(gumottc_data)

# calculations
## calculate percent N and C for samples that didn't get CosTech
gumottc_data$n_weight_p.x <- (gumottc_data$Total_N / (gumottc_data$iso_sample_weight * 1000)) * 100
tail(gumottc_data)

gumottc_data$c_weight_p.x <- (gumottc_data$Total_C / (gumottc_data$iso_sample_weight * 1000)) * 100
tail(gumottc_data)

## calculate to WUE
### calculate Delta13C
gumottc_data$D13C <- (gumottc_data$δ13Catm - gumottc_data$δ13CVPDB)/
  (1 + (gumottc_data$δ13CVPDB / 1000))

head(gumottc_data)

### calculate ci/ca
cica <- function(x) (x - 4.4)/22.6
gumottc_data$cica <- cica(gumottc_data$D13C)

head(gumottc_data)

### calculate WUE
WUE <- function(x) (gumottc_data$CO2ppm/1.6)*(1 - x)
gumottc_data$WUE <- WUE(gumottc_data$cica)

head(gumottc_data)

# c13 data and analysis
## hypothesis: c13 in historical samples is lower than in 
## present samples

### analyze c13 data - all (except c4)
δ13cvpdb_lm <- lmer((δ13CVPDB) ~ time_period.x + elevation_ft + 
                      woody_forb_grass + (1|scientific_name) + (1|mckittrick), 
                    data = subset(gumottc_data, c3_c4.x == 'c3')) # fit the linear model
plot(resid(δ13cvpdb_lm) ~ fitted(δ13cvpdb_lm)) # check the residuals
summary(δ13cvpdb_lm) # get summary of linear model
Anova(δ13cvpdb_lm) # shows significance for woody/forbs/grass
emmeans(δ13cvpdb_lm, ~time_period.x)
emmeans(δ13cvpdb_lm, ~woody_forb_grass)
emtrends(δ13cvpdb_lm, ~1, var = 'elevation_ft')

# WUE data and analysis
## hypothesis: WUE in historical samples is lower than in
## present samples
### analyze WUE - all (except c4)
WUE_lm <- lmer((WUE) ~ time_period.x + elevation_ft + 
                       woody_forb_grass + (1|scientific_name) + (1|mckittrick), 
                     data = subset(gumottc_data, c3_c4.x == 'c3')) # fit the linear model
plot(resid(WUE_lm) ~ fitted(WUE_lm)) # check the residuals
summary(WUE_lm) # get summary of linear model
Anova(WUE_lm) # shows significance for woody/forbs/grass
emmeans(WUE_lm, ~time_period.x)
emmeans(WUE_lm, ~woody_forb_grass)
emtrends(WUE_lm, ~1, var = 'elevation_ft')

# n percent data and analysis
## hypothesis: nitrogen in historical samples is higher than in
## present samples

### analyze n percent data - all (except c4)
n_weight_p_lm <- lmer((n_weight_p.x) ~ time_period.x + elevation_ft + 
                        woody_forb_grass + (1|scientific_name) + (1|mckittrick), 
                      data = subset(gumottc_data, c3_c4.x == 'c3')) # fit the linear model
plot(resid(n_weight_p_lm) ~ fitted(n_weight_p_lm)) # check the residuals
summary(n_weight_p_lm) # get summary of linear model
Anova(n_weight_p_lm) # shows significance for woody/forbs/grass
emmeans(n_weight_p_lm, ~time_period.x)
emmeans(n_weight_p_lm, ~woody_forb_grass)
emtrends(n_weight_p_lm, ~1, var = 'elevation_ft')

# n15 data and analysis
## hypothesis:
### analyze n15 data - all (except c4)
n15_lm <- lmer((δ15NAir) ~ time_period.x + elevation_ft + 
                        woody_forb_grass + (1|scientific_name) + (1|mckittrick), 
               data = subset(gumottc_data, c3_c4.x == 'c3')) # fit the linear model
plot(resid(n15_lm) ~ fitted(n15_lm)) # check the residuals
summary(n15_lm) # get summary of linear model
Anova(n15_lm) # shows significance for woody/forbs/grass
emmeans(n15_lm, ~time_period.x)
emmeans(n15_lm, ~woody_forb_grass)
emtrends(n15_lm, ~1, var = 'elevation_ft')

## tables for species diversity
### make table for number of samples per species
head(gumottc_data)
gumottc_data_groupby_species <- group_by(gumottc_data, scientific_name)
gumottc_data_species <- summarise(gumottc_data_groupby_species, n_samples = n())
write.csv(gumottc_data_species, 'gumottc_data_species.csv')

### make table for number of samples per family
head(gumottc_data)
gumottc_data_groupby_family <- group_by(gumottc_data, family)
gumottc_data_family <- summarise(gumottc_data_groupby_family, n_samples = n())
write.csv(gumottc_data_family, 'gumottc_data_family.csv')

### make table for number of samples with matches per species
head(gumottc_match)
gumottc_match_groupby_species <- group_by(gumottc_match, scientific_name)
gumottc_match_species <- summarise(gumottc_match_groupby_species, n_samples = n())
write.csv(gumottc_match_species, 'gumottc_match_species.csv')
