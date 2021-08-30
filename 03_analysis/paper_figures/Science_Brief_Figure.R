
library(tidyverse)
library(cowplot)


# CI's for Present-Day ----------------------------------------------------

heat_cov <- read_csv("heat_var.csv")
whp_cov <- read_csv("whp_var.csv")
precip_cov <- read_csv("precip_var.csv")
og_cov <- read_csv("og_var.csv")

# Function for SE at T2
se_t2 <- function(cov_matrix) {
  sqrt(cov_matrix[1,1] + cov_matrix[2,2] + 2*cov_matrix[1,2])
}

# Confirm function works as it should - test on heat covariance matrix
sqrt(heat_cov[1,1] + heat_cov[2,2] - 2*heat_cov[1,2]) # hard code
se_t2(heat_cov) # function code

# SE's at T2
heat_se <- se_t2(heat_cov)
whp_se <- se_t2(whp_cov)
precip_se <- se_t2(precip_cov)
og_se <- se_t2(og_cov)

# CI's at T2
6.96+heat_se*1.96 # heat upper
6.96-heat_se*1.96 # heat lower

698.26+precip_se*1.96 # precip upper
698.26-precip_se*1.96 # precip lower

.17+og_se*1.96 # og upper
.17-og_se*1.96 # og lower

1.77+whp_se*1.96 # whp upper
1.77-whp_se*1.96 # whp lower

# Heat Days ---------------------------------------------------------------

heat_days <- tibble(
  heat = c(1.15, 6.96),
  proportion_low = c(.89, 4.48),
  proportion_upp = c(1.42, 9.45),
  time = c("Historical", "Present Day")
)

my_pal <- c("#994F00", "#006CD1") #cc9966
my_pal <- c("#cc9966", "#006CD1")

heat_plot <- ggplot(heat_days, aes(time, heat, group = 1)) + # label = heat 
  geom_line(color = "black", linetype = "dashed") +
  geom_point(stat = "identity", size = 4, color = my_pal) + # alpha = .7
  geom_errorbar(aes(ymin = proportion_low, ymax = proportion_upp), 
                width = .05, color = my_pal) +
  theme_bw() + 
  ylab("Days per year in excess of 100 degrees F") + xlab("") +
  scale_y_continuous(breaks=seq(0, 10, 2), limits=c(0, 10)) +
  #geom_text(nudge_x = 0.2) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 10),
       # axis.text.x = element_text(size = 10),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 13))) 


# Precipitation -----------------------------------------------------------

# Measured in millimeters
precip <- tibble(
  precip = c(905.50, 698.26),
  proportion_low = c(835.61, 636.30),
  proportion_upp = c(975.40, 760.22),
  time = c("Historical", "Present Day")
)

precip_plot <- ggplot(precip, aes(time, precip, group = 1)) + # label = precip
  geom_line(color = "black", linetype = "dashed") +
  geom_point(stat = "identity", size = 4, color = my_pal) + 
  geom_errorbar(aes(ymin = proportion_low, ymax = proportion_upp), 
                width = .05, color = my_pal) +
  theme_bw() + 
  ylab("Mean annual precipitation (millimeters)") + xlab("") +
  scale_y_continuous(breaks=seq(500, 1000, 100), limits=c(500, 1000)) +
  #geom_text(nudge_x = 0.2) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 10),
        #axis.text.x = element_text(size = 10),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 13))) 

# Mineral Potential -------------------------------------------------------

# Proportion
og <- tibble(
  og = c(.21, .17),
  proportion_low = c(.17, .13),
  proportion_upp = c(.25, .21),
  time = c("Historical", "Present Day")
)

og_plot <- ggplot(og, aes(time, og, group = 1)) + #label = og
  geom_line(color = "black", linetype = "dashed") +
  geom_point(stat = "identity", size = 4, color = my_pal) + 
  geom_errorbar(aes(ymin = proportion_low, ymax = proportion_upp), 
                width = .05, color = my_pal) +
  theme_bw() + 
  ylab("Portion land with subsurface oil and gas") + xlab("") +
  scale_y_continuous(breaks=seq(0, 1, .1), limits=c(0, .3)) +
 # geom_text(nudge_x = 0.2) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 10),
        #axis.text.x = element_text(size = 10),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 13))) 


# Wildfire -----------------------------------------------------------
wildfire <- tibble(
  wildfire = c(1.36, 1.77),
  proportion_low = c(1.30, 1.63),
  proportion_upp = c(1.42, 1.91),
  time = c("Historical", "Present Day")
)

wildfire_plot <- ggplot(wildfire, aes(time, wildfire, group = 1)) + # label = wildfire
  geom_line(color = "black", linetype = "dashed") +
  geom_point(stat = "identity", size = 4, color = my_pal) + # 
  geom_errorbar(aes(ymin = proportion_low, ymax = proportion_upp), 
                width = .05, color = my_pal) +
  theme_bw() + 
  ylab("Mean Wildfire Hazard Potential") + xlab("") +
  scale_y_continuous(breaks=seq(1, 3, .5), limits=c(1, 3)) +
  #geom_text(nudge_x = 0.2, nudge_y = -.15) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 10),
        #axis.text.x = element_text(size = 10),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 13))) 



# Combine all plots -------------------------------------------------------

# Combine all plots
all_plots <- plot_grid(heat_plot + theme(legend.position="none"),
                       wildfire_plot + theme(legend.position="none"),
                       precip_plot + theme(legend.position="none"),
                       og_plot + theme(legend.position="none"),
                       #labels = c("A","B","C","D"),
                       nrow = 1,
                       label_size=10)

all_plots <- ggarrange(heat_plot, wildfire_plot, precip_plot, og_plot, nrow = 1)

ggsave("/Users/kathrynmcconnell/Dropbox (Yale_FES)/tribal_lands_scratch/summary_figure.eps",
       plot = last_plot(), 
       width = 10.5,
       height = 3.5,
       dpi = 300)
