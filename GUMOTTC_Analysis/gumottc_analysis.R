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
                      woody_forb_grass + (1|scientific_name), 
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
                       woody_forb_grass + (1|scientific_name), 
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
                        woody_forb_grass + (1|scientific_name), 
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
                        woody_forb_grass + (1|scientific_name), 
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

## figures
theme_set(theme_classic())

### WUE boxplot - time period comparison
label1 <- "Time Period*
Plant Growth Form*"

WUE_time_plot <- subset(gumottc_data, c3_c4.x == 'c3') %>%
  ggplot(aes(x = factor(time_period.x), y = WUE)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", fill = "white") + 
  geom_jitter(size = 3, width = 0.2, shape = 21,
              aes(fill = factor(woody_forb_grass))) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               unknown = "yellow", woody = "dodgerblue"),
                    name = "Plant Growth Form", labels = c('Forb', 'Grass', 'Unknown', 'Woody')) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.x = element_blank() , axis.text = element_text(size = 18),
        axis.text.x = element_blank(), axis.title = element_text(size = 22, face = "bold"), 
        axis.title.x = element_blank(), legend.text = element_text(size = 18), 
        legend.title = element_text(size = 22, face = "bold"), legend.justification = "top", 
        plot.tag = element_text(size = 22, face = "bold")) + 
  annotate("text", x = Inf, y = 80, label = label1, hjust = -0.08, size = 6) +
  scale_x_discrete(labels = c('Historical', 'Present')) +
  labs(x = "Time Period", y = expression(bold(paste('WUE (μmol・',mol^{-1}, ')'))), tag = "A")

WUE_time_plota <- ggplotGrob(WUE_time_plot)
WUE_time_plota$layout$clip[WUE_time_plota$layout$name == "panel"] = "off"
grid.draw(WUE_time_plota)

### WUE boxplot - plant tissue type comparison
WUE_ppt_plot <- subset(gumottc_data, c3_c4.x == 'c3' & woody_forb_grass != 'unknown') %>%
  ggplot(aes(x = factor(woody_forb_grass), y = WUE, fill = factor(woody_forb_grass))) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               woody = "dodgerblue")) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.x = element_blank(),
        axis.text = element_text(size = 18), axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.title = element_text(size = 22, face = "bold"), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Forb', 'Grass', 'Woody')) +
  labs(x = "Plant Growth Form", y = expression(bold(paste('WUE (μmol・',mol^{-1}, ')'))), tag = "B")

WUE_ppt_plot

### N boxplot - time period comparison
label2a <- expression('Time Period' ^ 'ns')
label2b <- expression('Plant Growth Form' ^ 'ns')

N_time_plot <- subset(gumottc_data, c3_c4.x == 'c3') %>%
  ggplot(aes(x = factor(time_period.x), y = n_weight_p.x)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", fill = "white", outlier.shape = NA) + 
  geom_jitter(width = 0.2, size = 3, shape = 21,
              aes(fill = factor(woody_forb_grass))) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               unknown = "yellow", woody = "dodgerblue"),
                    name = "Plant Growth Form", labels = c('Forb', 'Grass', 'Unknown', 'Woody')) +
  guides(fill = guide_legend(override.aes = list(color = "white", shape = 1))) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 18), axis.text.x = element_blank(), 
        axis.title = element_text(size = 22, face = "bold"), axis.title.x = element_blank(), 
        legend.text = element_text(size = 18, color = "white"),
        legend.title = element_text(size = 22, face = "bold", color = "white"),
        legend.justification = "bottom", plot.tag = element_text(size = 22, face = "bold")) + 
  annotate("text", x = Inf, y = 5, label = label2a, hjust = -0.08, size = 6) +
  annotate("text", x = Inf, y = 4.6, label = label2b, hjust = -0.08, size = 6) +
  scale_x_discrete(labels = c('Historical', 'Present')) +
  labs(x = "Time Period", y = "Nitrogen (%)", tag = "C")

N_time_plota <- ggplotGrob(N_time_plot)
N_time_plota$layout$clip[N_time_plota$layout$name == "panel"] = "off"
grid.draw(N_time_plota)

### N boxplot - plant tissue type comparison
N_ppt_plot <- subset(gumottc_data, c3_c4.x == 'c3' & woody_forb_grass != 'unknown') %>%
  ggplot(aes(x = factor(woody_forb_grass), y = n_weight_p.x, fill = factor(woody_forb_grass))) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               woody = "dodgerblue")) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 18), axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.title = element_text(size = 22, face = "bold"), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Forb', 'Grass', 'Woody')) +
  labs(x = "Plant Growth Form", y = "Nitrogen (%)", tag = "D")

N_ppt_plot

### 15N boxplot - time period comparison
label3a <- "Time Period*"
label3b <- expression('Plant Growth Form' ^ 'ns')

Niso_time_plot <- subset(gumottc_data, c3_c4.x == 'c3') %>%
  ggplot(aes(x = factor(time_period.x), y = δ15NAir)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", fill = "white", outlier.shape = NA) + 
  geom_jitter(size = 3, width = 0.2, shape = 21,
              aes(fill = factor(woody_forb_grass))) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               unknown = "yellow", woody = "dodgerblue"),
                    name = "Plant Growth Form", labels = c('Forb', 'Grass', 'Unknown', 'Woody')) +
  guides(fill = guide_legend(override.aes = list(color = "white", shape = 1))) + 
  theme(axis.ticks.length = unit(.5, "cm"), axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"), 
        legend.text = element_text(size = 18, color = "white"), 
        legend.title = element_text(size = 22, face = "bold", color = "white"), 
        legend.justification = "bottom",
        plot.tag = element_text(size = 22, face = "bold")) + 
  annotate("text", x = Inf, y = 6, label = label3a, hjust = -0.08, size = 6) +
  annotate("text", x = Inf, y = 5.1, label = label3b, hjust = -0.08, size = 6) +
  scale_x_discrete(labels = c('Historical', 'Present')) +
  labs(x = "Time Period", y = expression(bold(paste(δ^{15},'N (‰)'))), tag = "E")

Niso_time_plota <- ggplotGrob(Niso_time_plot)
Niso_time_plota$layout$clip[Niso_time_plota$layout$name == "panel"] = "off"
grid.draw(Niso_time_plota)

### 15N boxplot - plant tissue type comparison
Niso_ppt_plot <- subset(gumottc_data, c3_c4.x == 'c3' & woody_forb_grass != 'unknown') %>%
  ggplot(aes(x = factor(woody_forb_grass), y = δ15NAir, fill = factor(woody_forb_grass))) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               woody = "dodgerblue")) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.text = element_text(size = 18), 
        axis.text.y = element_blank(), axis.title = element_text(size = 22, face = "bold"), 
        axis.title.y = element_blank(), plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Forb', 'Grass', 'Woody')) +
  labs(x = "Plant Growth Form", y = "δ15N (‰)", tag = "F")

Niso_ppt_plot

### combine graphs
all_time_plot_g <- rbind(WUE_time_plota, N_time_plota, Niso_time_plota, 
                         size = "max")

WUE_ppt_plot_g <- ggplotGrob(WUE_ppt_plot)
N_ppt_plot_g <- ggplotGrob(N_ppt_plot)
Niso_ppt_plot_g <- ggplotGrob(Niso_ppt_plot)

all_ppt_plot_g <- rbind(WUE_ppt_plot_g, N_ppt_plot_g, Niso_ppt_plot_g,
                        size = "max")

all_timeppt_plot_g <- cbind(all_time_plot_g, all_ppt_plot_g, 
                            size = "max")

jpeg(filename = "comboplot.jpg",
     width = 14, height = 16, units = 'in', res = 500)
grid.newpage()
grid.draw(all_timeppt_plot_g)
dev.off()
