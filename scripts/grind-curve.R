library(tidyverse)

options(ggplot2.discrete.colour = function() scale_colour_viridis_d(),
        ggplot2.continuous.colour = function() scale_colour_viridis_c())

# set a consistent theme across plots
capapres_theme <- theme(panel.background = element_rect(fill = "transparent", colour = NA),
                        plot.background = element_rect(fill = "transparent", colour = NA),
                        panel.grid = element_line(colour = "#88AC82", size = 0.2), # gridlines matching slides colour
                        panel.border = element_blank()) 

grind_data_raw <- readr::read_csv2("data/grind-curve.csv")

grind_data <- grind_data_raw %>%
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
    capapres_theme
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
  capapres_theme
#scale_color_viridis_d()


grind_data
