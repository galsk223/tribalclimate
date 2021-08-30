
library(tidyverse)
library(dplyr)
library(stats)
library(kableExtra)
library(stringr)
library(ggridges)
library(cowplot)

setwd("/Users/kathrynmcconnell/Documents/Documents - Kathryn’s MacBook Pro/GitHub/tribal_lands") # KM

dat_env2 <- readRDS("data_long_5.3.21.rds") %>% # only tribes with T1 and T2
  mutate(heatdays_mean_log = log(heatdays_mean + 1)) %>%
  dplyr::select(tribe, time, 
                `Days over 100F`=heatdays_mean,
                'Log Days over 100F'= heatdays_mean_log,
                `Drought`=drought_mean,
                `Precipitation`=precip,
                `Wildfire Hazard Potential`=whp,
                `Elevation`=elevation_mean,
                `Ruggedness`=tri_mean,
                `Soil Organic Carbon`=SOC_perc,
                `Oil and Gas Basin`=BasinPortion,
                `Oil Production`=AllArea_OilPortion,
                `Gas Production`=AllArea_GasPortion,
                `Fraction Federal Land`=PADPortion) 

# change data to long
dat_env_long <- dat_env2 %>%
  pivot_longer(cols = c("Days over 100F", "Log Days over 100F", "Drought", "Precipitation", "Wildfire Hazard Potential", "Elevation", "Ruggedness",               
                        "Soil Organic Carbon", "Fraction Federal Land", "Oil and Gas Basin", "Oil Production", "Gas Production"), 
               names_to = "variable", 
               values_to = "value") %>% filter(!is.na(value))

# color pal
my_pal <- c("#994F00", "#006CD1")

# Final Plots -------------------------------------------------------------

# Extreme Heat - Days over 100 - Log
heat_plot <- ggplot(data = subset(dat_env_long, variable == "Log Days over 100F"), aes(x=value, y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = .65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height=.000, yoffset= -.005),
                      alpha=.5,show.legend = T) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="on", xlim=c(0,NA)) +
  ylab("Density") +
  xlab("Extreme Heat (log scale)")
heat_plot

# Fire Risk
fire_plot <- ggplot(data = subset(dat_env_long, variable == "Wildfire Hazard Potential"), aes(x=value, y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height = .000, yoffset= -.002),
                      alpha=.5,show.legend = T) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical", "Present Day")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical", "Present Day")) + 
  scale_x_continuous(breaks=seq(0, 7, 1)) +
  coord_cartesian(expand = FALSE, clip="on", xlim=c(0,NA)) +
  ylab("Density") +
  xlab("Wildfire Risk") 
fire_plot

# Oil and Gas Basin
og_plot <- ggplot(data = subset(dat_env_long, variable == "Oil and Gas Basin"), aes(x=value, y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .75, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = .65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height=.00, yoffset= -.03),
                      alpha=.5,show.legend = T) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="on", xlim=c(0,1.0)) + # c(-.25,1.25)
  scale_x_continuous(breaks=c(0,.25,.5,.75,1)) +
  ylab("Density") +
  xlab("Oil and Gas Basins") 
og_plot

# Precipitation - Log 
precip_plot <- ggplot(data = subset(dat_env_long, variable == "Precipitation"), aes(x=log(value), y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height = .00, yoffset= -.006),
                      alpha=.5,show.legend = T, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="off") +
  ylab("Density") +
  xlab("Precipitation (log scale)")
precip_plot

## CREATE COMBINED PLOT FIGURE FOR THE MANUSCRIPT ------

# extract a T1/T2 legend for figure
legend <- get_legend(
  # create some space to the left of the legend
  fire_plot + #grabbing legend from fire_plot
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom")
)

# Combine all plots
all_plots <- plot_grid(heat_plot + theme(legend.position="none"),
                       precip_plot + theme(legend.position="none"),
                       fire_plot + theme(legend.position="none"),
                       og_plot + theme(legend.position="none"),
                       labels = c("A","B","C","D"),
                       nrow = 2,
                       label_size=10)

# plot with the legend
final_plot <- plot_grid(all_plots, legend, nrow=2, rel_heights= c(1,.1))
final_plot

# export
ggsave(final_plot, file="/Users/kathrynmcconnell/Dropbox (Yale_FES)/tribal_lands_scratch/plot.pdf", 
       width=10, height=8)


# Appendix figures --------------------------------------------------------
# - Drought
#- Elevation
#- Ruggedness
#- SOC
#- Federal lands

# Drought plot
drought_plot <- ggplot(data = subset(dat_env_long, variable == "Drought"), aes(x=value, y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height = .011, yoffset= -.006),
                      alpha=.5,show.legend = T, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="off") +
  ylab("Density") +
  xlab("Drought")
drought_plot

# Elevation Plot
elevation_plot <- ggplot(data = subset(dat_env_long, variable == "Elevation"), aes(x=log(value +1), y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height = .00, yoffset= -.006),
                      alpha=.5,show.legend = T, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="off") +
  ylab("Density") +
  xlab("Elevation (log scale)")
elevation_plot

# Ruggedness plot
ruggedness_plot <- ggplot(data = subset(dat_env_long, variable == "Ruggedness"), aes(x=value, y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height = .000, yoffset= -.000),
                      alpha=.5,show.legend = T, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="on", xlim=c(0,NA)) +
  ylab("Density") +
  xlab("Ruggedness")
ruggedness_plot

# SOC Plot
soc_plot <- ggplot(data = subset(dat_env_long, variable == "Soil Organic Carbon"), aes(x=value, y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height = .000, yoffset= -.006),
                      alpha=.5,show.legend = T, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="on", xlim=c(0,NA)) +
  ylab("Density") +
  xlab("Soil Organic Carbon")
soc_plot

# Federal Lands plot
fed_plot <- ggplot(data = subset(dat_env_long, variable == "Fraction Federal Land"), aes(x=value, y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height = .00, yoffset= -.05),
                      alpha=.5,show.legend = T, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="on", xlim=c(0,NA)) +
  ylab("Density") +
  xlab("Fraction of Federal Lands")
fed_plot

# Oil production
oil_plot <- ggplot(data = subset(dat_env_long, variable == "Oil Production"), aes(x=value, y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height = .011, yoffset= -.006),
                      alpha=.5,show.legend = T, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="on", xlim=c(0,NA)) +
  ylab("Density") +
  xlab("Oil Production")
oil_plot

# Gas production
gas_plot <- ggplot(data = subset(dat_env_long, variable == "Gas Production"), aes(x=value, y="", fill = time, color = time)) +
  geom_density_ridges(jittered_points = TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.65,
                      quantile_lines=TRUE, quantile_fun=function(x,...)mean(x),
                      position = position_points_jitter(height = .011, yoffset= -.006),
                      alpha=.5,show.legend = T, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom", axis.title.y=element_text(family="Helvetica", size=10), 
        axis.title.x=element_text(family="Helvetica", size=12, face="bold"),
        axis.text.x= element_text(family="Helvetica", size=10),
        legend.text=element_text(size=12, family="Helvetica")) +
  scale_fill_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) +
  scale_color_manual(name="", values = my_pal, labels = c("Historical Lands", "Present Day Lands")) + 
  coord_cartesian(expand = FALSE, clip="on", xlim=c(0,NA)) +
  ylab("Density") +
  xlab("Gas Production")
gas_plot

# Final plot for appendix
all_plots_app <- plot_grid(heat_plot + theme(legend.position="none"),
                       drought_plot + theme(legend.position="none"),
                       precip_plot + theme(legend.position="none"),
                       fire_plot + theme(legend.position="none"),
                       elevation_plot + theme(legend.position="none"),
                       ruggedness_plot + theme(legend.position="none"),
                       soc_plot + theme(legend.position="none"),
                       og_plot + theme(legend.position="none"),
                       oil_plot + theme(legend.position="none"),
                       gas_plot + theme(legend.position="none"),
                       fed_plot + theme(legend.position="none"),
                       labels = c("A","B","C","D", "E", "F", "G", "H", "I", "J", "K"),
                       nrow = 4,
                       label_size=10)


#final_plot <- plot_grid(all_plots_app, legend, nrow=1, rel_heights= c(1,.1))
final_plot_app <- plot_grid(all_plots_app, legend, ncol = 1, rel_heights= c(1,.1))
final_plot_app

ggsave(final_plot_app, file="/Users/kathrynmcconnell/Dropbox (Yale_FES)/tribal_lands_scratch/plot_appendix.pdf", 
       width=10, height=8)
