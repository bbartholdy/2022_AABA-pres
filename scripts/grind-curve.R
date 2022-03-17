library(tidyverse)

options(ggplot2.discrete.colour = function() scale_colour_viridis_d(),
        ggplot2.continuous.colour = function() scale_colour_viridis_c())

# set a consistent theme across plots
harvest_theme <- theme(panel.background = element_rect(fill = "transparent", colour = NA),
                        plot.background = element_rect(fill = "transparent", colour = NA),
                        panel.grid.major = element_line(colour = "#88AC82", size = 0.1),
                        axis.ticks = element_line(colour = "#88AC82", size = 0.1),
                        panel.border = element_blank(),
                        axis.title = element_text(family = "serif", size = 16),
                        legend.key = element_rect(fill = "transparent", colour = "transparent")) 

# harvest_theme <- theme(panel.background = element_rect(fill = "transparent", colour = NA),
#                         plot.background = element_rect(fill = "transparent", colour = NA),
#                         panel.border = element_blank(),
#                         panel.grid.major = element_line(colour = "#88AC82", size = 0.1),
#                         #plot.margin = element_line(colour = "#88AC82"),
#                         axis.title = element_text(family = "serif", size = 16))

grind_data_raw <- readr::read_csv2("data/grind-curve.csv")

grind_data <- grind_data_raw %>%
  filter(Sample != "Synthetic") %>%
  mutate(day = stringr::str_extract(Sample, "[0-9*?]."),
         grind = stringr::str_extract(Sample, "[a-f]$"),
         Sample = if_else(str_detect(Sample, "F"), "Artificial calculus", Sample),
         Sample_day = if_else(!is.na(day), paste(Sample, "day", day), Sample))

# Plot of grind curves
grind_data %>%
  group_by(day, Sample) %>%
  ggplot(aes(x = FWHM, y = IRSF, col = Sample_day, shape = Sample_day)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = F) +
    #theme_minimal() +
    labs(col = "Sample", shape = "Sample",
         x = "FWHM of the 1035 peak",
         y = "Splitting factor") +
    harvest_theme
    #scale_color_viridis_d()

# isolate artificial samples to see diffs between days
grind_data %>%
  filter(Sample == "Artificial calculus") %>%
  #group_by(day, Sample) %>%
  ggplot(aes(x = FWHM, y = IRSF, col = Sample_day, shape = Sample_day)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = F) +
  #theme_minimal() +
  labs(col = "Sample", shape = "Sample",
       x = "FWHM of the 1035 peak",
       y = "Splitting factor") +
  harvest_theme
#scale_color_viridis_d()


grind_data
