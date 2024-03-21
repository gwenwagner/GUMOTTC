# Figure creation for both thesis and poster

theme_set(theme_classic())

legend_title_mck <- "Was Specimen 
Collected in
McKittrick Canyon"

## Thesis - Combo image with tags
### WUE boxplot - time period comparison
label1 <- "Time Period*
Plant Growth Form*"

WUE_time_plot <- subset(gumottc_data, c3_c4.x == 'c3') %>%
  ggplot(aes(x = factor(time_period.x), y = WUE)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", fill = "white") + 
  geom_jitter(size = 3, width = 0.2, 
              aes(fill = factor(woody_forb_grass), shape = factor(mckittrick))) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               unknown = "yellow", woody = "dodgerblue"),
                    name = "Plant Growth Form", labels = c('Forb', 'Grass', 'Unknown', 'Woody')) +
  scale_shape_manual(values = c(22, 24), name = legend_title_mck, labels = c('No', 'Yes')) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.x = element_blank() , axis.text = element_text(size = 18),
        axis.text.x = element_blank(), axis.title = element_text(size = 22, face = "bold"), 
        axis.title.x = element_blank(), legend.text = element_text(size = 18), 
        legend.title = element_text(size = 22, face = "bold"), legend.justification = "bottom", 
        plot.tag = element_text(size = 22, face = "bold")) + 
  annotate("text", x = Inf, y = 120, label = label1, hjust = -0.08, size = 6) +
  scale_x_discrete(labels = c('Historical', 'Present')) +
  labs(x = "Time Period", y = "WUE (μmol・mol-1)", tag = "A")

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
        axis.ticks.y = element_blank(), axis.text = element_text(size = 18), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title = element_text(size = 22, face = "bold"), axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Forb', 'Grass', 'Woody')) +
  labs(x = "Plant Growth Form", y = "WUE (μmol・mol-1)", tag = "B")

WUE_ppt_plot

### N boxplot - time period comparison
label2 <- "Time Period ns
Plant Growth Form ns"

N_time_plot <- subset(gumottc_data, c3_c4.x == 'c3') %>%
  ggplot(aes(x = factor(time_period.x), y = n_weight_p.x)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", fill = "white", outlier.shape = NA) + 
  geom_jitter(width = 0.2, size = 3, aes(fill = factor(woody_forb_grass), shape = factor(mckittrick))) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               unknown = "yellow", woody = "dodgerblue"),
                    name = "Plant Growth Form", labels = c('Forb', 'Grass', 'Unknown', 'Woody')) +
  scale_shape_manual(values = c(22, 24), name = legend_title_mck, labels = c('No', 'Yes')) +
  guides(fill = guide_legend(override.aes = list(color = "white"))) + 
  guides(shape = guide_legend(override.aes = list(color = "white"))) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 18), axis.text.x = element_blank(), 
        axis.title = element_text(size = 22, face = "bold"), axis.title.x = element_blank(), 
        legend.text = element_text(size = 18, color = "white"),
        legend.title = element_text(size = 22, face = "bold", color = "white"),
        legend.justification = "bottom", plot.tag = element_text(size = 22, face = "bold")) + 
  annotate("text", x = Inf, y = 5, label = label2, hjust = -0.08, size = 6) +
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
        axis.ticks.y = element_blank(), axis.text = element_text(size = 18), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title = element_text(size = 22, face = "bold"), axis.title.x = element_blank(),
        axis.title.y = element_blank(), plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Forb', 'Grass', 'Woody')) +
  labs(x = "Plant Growth Form", y = "Nitrogen (%)", tag = "D")

N_ppt_plot

### 15N boxplot - time period comparison
label3 <- "Time Period˙
Plant Growth Form*"

Niso_time_plot <- subset(gumottc_data, c3_c4.x == 'c3') %>%
  ggplot(aes(x = factor(time_period.x), y = δ15NAir)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", fill = "white", outlier.shape = NA) + 
  geom_jitter(size = 3, width = 0.2, 
              aes(fill = factor(woody_forb_grass), shape = factor(mckittrick))) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               unknown = "yellow", woody = "dodgerblue"),
                    name = "Plant Growth Form", labels = c('Forb', 'Grass', 'Unknown', 'Woody')) +
  scale_shape_manual(values = c(22, 24), name = legend_title_mck, labels = c('No', 'Yes')) +
  guides(fill = guide_legend(override.aes = list(color = "white"))) + 
  guides(shape = guide_legend(override.aes = list(color = "white"))) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"), 
        legend.text = element_text(size = 18, color = "white"), 
        legend.title = element_text(size = 22, face = "bold", color = "white"), 
        legend.justification = "bottom",
        plot.tag = element_text(size = 22, face = "bold")) + 
  annotate("text", x = Inf, y = 7.5, label = label3, hjust = -0.08, size = 6) +
  scale_x_discrete(labels = c('Historical', 'Present')) +
  labs(x = "Time Period", y = "δ15N (‰)", tag = "E")

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
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.y = element_blank(),
        axis.text = element_text(size = 18), axis.text.y = element_blank(),
        axis.title = element_text(size = 22, face = "bold"), axis.title.y = element_blank(),
        plot.tag = element_text(size = 22, face = "bold")) + 
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

jpeg(filename = "newcomboplot.jpg",
     width = 14, height = 16, units = 'in', res = 500)
grid.newpage()
grid.draw(all_timeppt_plot_g)
dev.off()

## Poster
### Main graphs regarding time period
#### WUE
WUE_time_plot <- subset(gumottc_data, c3_c4.x == 'c3') %>%
  ggplot(aes(x = factor(time_period.x), y = WUE)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", fill = "white") + 
  geom_jitter(size = 3, width = 0.2, 
              aes(fill = factor(woody_forb_grass), shape = factor(mckittrick))) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               unknown = "yellow", woody = "dodgerblue"),
                    name = "Plant Growth Form", labels = c('Forb', 'Grass', 'Unknown', 'Woody')) +
  scale_shape_manual(values = c(22, 24), name = legend_title_mck, labels = c('No', 'Yes')) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  theme(axis.ticks.length = unit(.5, "cm"), axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"), legend.text = element_text(size = 18), 
        legend.title = element_text(size = 22, face = "bold"), legend.justification = "bottom",
        plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Historical', 'Present')) +
  labs(x = "Time Period", y = "WUE (μmol・mol-1)")

WUE_time_plot

jpeg(filename = "WUE_time.jpg",
     width = 8, height = 6, units = 'in', res = 600)
grid.newpage()
grid.draw(WUE_time_plot)
dev.off()

#### N and Niso
N_time_plot <- subset(gumottc_data, c3_c4.x == 'c3') %>%
  ggplot(aes(x = factor(time_period.x), y = n_weight_p.x)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", fill = "white", outlier.shape = NA) + 
  geom_jitter(width = 0.2, size = 3, aes(fill = factor(woody_forb_grass), shape = factor(mckittrick))) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               unknown = "yellow", woody = "dodgerblue"),
                    name = "Plant Growth Form", labels = c('Forb', 'Grass', 'Unknown', 'Woody')) +
  scale_shape_manual(values = c(22, 24), name = legend_title_mck, labels = c('No', 'Yes')) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 18), axis.text.x = element_blank(), 
        axis.title = element_text(size = 22, face = "bold"), 
        axis.title.x = element_blank(), legend.text = element_text(size = 18),
        legend.title = element_text(size = 22, face = "bold"), legend.justification = "bottom",
        plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Historical', 'Present')) +
  labs(x = "Time Period", y = "Nitrogen (%)")

N_time_plot

Niso_time_plot <- subset(gumottc_data, c3_c4.x == 'c3') %>%
  ggplot(aes(x = factor(time_period.x), y = δ15NAir)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", fill = "white", outlier.shape = NA) + 
  geom_jitter(size = 3, width = 0.2, 
              aes(fill = factor(woody_forb_grass), shape = factor(mckittrick))) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               unknown = "yellow", woody = "dodgerblue"),
                    name = "Plant Growth Form", labels = c('Forb', 'Grass', 'Unknown', 'Woody')) +
  scale_shape_manual(values = c(22, 24), name = legend_title_mck, labels = c('No', 'Yes')) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) + 
  theme(axis.ticks.length = unit(.5, "cm"), axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"), legend.text = element_text(size = 18), 
        legend.title = element_text(size = 22, face = "bold"), legend.justification = "bottom",
        plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Historical', 'Present')) +
  labs(x = "Time Period", y = "δ15N (‰)")

Niso_time_plot

N_time_plot_g <- ggplotGrob(N_time_plot)
Niso_time_plot_g <- ggplotGrob(Niso_time_plot)

all_Ntime_plot_g <- rbind(N_time_plot_g, Niso_time_plot_g, 
                         size = "max")

jpeg(filename = "all_Ntime.jpg",
     width = 8, height = 12, units = 'in', res = 600)
grid.newpage()
grid.draw(all_Ntime_plot_g)
dev.off()

### Combo graph for plant growth form
#### WUE boxplot - plant tissue type comparison
WUE_ppt_plot <- subset(gumottc_data, c3_c4.x == 'c3' & woody_forb_grass != 'unknown') %>%
  ggplot(aes(x = factor(woody_forb_grass), y = WUE, fill = factor(woody_forb_grass))) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               woody = "dodgerblue")) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 18), axis.text.x = element_blank(),
        axis.title = element_text(size = 22, face = "bold"), axis.title.x = element_blank(),
        plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Forb', 'Grass', 'Woody')) +
  labs(x = "Plant Growth Form", y = "WUE (μmol・mol-1)")

WUE_ppt_plot

#### N boxplot - plant tissue type comparison
N_ppt_plot <- subset(gumottc_data, c3_c4.x == 'c3' & woody_forb_grass != 'unknown') %>%
  ggplot(aes(x = factor(woody_forb_grass), y = n_weight_p.x, fill = factor(woody_forb_grass))) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               woody = "dodgerblue")) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 18), axis.text.x = element_blank(),
        axis.title = element_text(size = 22, face = "bold"), axis.title.x = element_blank(),
        plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Forb', 'Grass', 'Woody')) +
  labs(x = "Plant Growth Form", y = "Nitrogen (%)")

N_ppt_plot

#### 15N boxplot - plant tissue type comparison
Niso_ppt_plot <- subset(gumottc_data, c3_c4.x == 'c3' & woody_forb_grass != 'unknown') %>%
  ggplot(aes(x = factor(woody_forb_grass), y = δ15NAir, fill = factor(woody_forb_grass))) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c(forb = "green", grass = "darkgreen", 
                               woody = "dodgerblue")) +
  theme(axis.ticks.length = unit(.5, "cm"), axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"), 
        plot.tag = element_text(size = 22, face = "bold")) + 
  scale_x_discrete(labels = c('Forb', 'Grass', 'Woody')) +
  labs(x = "Plant Growth Form", y = "δ15N (‰)")

Niso_ppt_plot

WUE_ppt_plot_g <- ggplotGrob(WUE_ppt_plot)
N_ppt_plot_g <- ggplotGrob(N_ppt_plot)
Niso_ppt_plot_g <- ggplotGrob(Niso_ppt_plot)

all_ppt_plot_g <- rbind(WUE_ppt_plot_g, N_ppt_plot_g, Niso_ppt_plot_g,
                        size = "max")

jpeg(filename = "ppt_plot.jpg",
     width = 6, height = 12, units = 'in', res = 500)
grid.newpage()
grid.draw(all_ppt_plot_g)
dev.off()
