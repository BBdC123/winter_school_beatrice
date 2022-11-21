# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(palmerpenguins)


# Data --------------------------------------------------------------------

# Load the dataset into the local environment
penguins <- penguins


# Example -----------------------------------------------------------------

# Basic faceted plot
lm_1 <- ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~species)
lm_1

# Basic combined plot
ggarrange(lm_1, lm_1)


# Exercise 1 --------------------------------------------------------------

# Create a new plot type and facet by gender

plot_1 <- ggplot(data = penguins, 
                 aes(x=body_mass_g, y=bill_length_mm, colour =species))+
  geom_point()+
  facet_wrap(~sex) +
  labs(x="Body mass (g)", y= "Bill length (mm)", colour="Species")

plot_1


# Exercise 2 --------------------------------------------------------------

# Create a new plot type and facet by two categories

box_1 <- ggplot(data = penguins, 
                aes(x = as.factor(sex),
                    y = body_mass_g)) + 
  geom_boxplot(aes(fill = species)) +
  facet_wrap(sex~island)+
  labs(x = "Sex", y = "Body mass (g)", fill = "Species") 
box_1

box_2 <- ggplot(data = penguins, 
                aes(x = body_mass_g,
                    y = bill_length_mm)) + 
  geom_boxplot(aes(fill = species)) +
  facet_wrap(sex~island)+
  labs(x="Body mass (g)", y= "Bill length (mm)", colour="Species") 
box_2


# Exercise 3 --------------------------------------------------------------

# Combine all of the plots you've created so far
# Save them as a high-res file larger than 2 MB


grid_2 <- ggarrange(plot_1, box_2,
                    ncol = 2, nrow = 1,
                    labels = c("A)", "B)"),
                    common.legend = TRUE,
                    legend = "bottom")
grid_2


ggsave(plot = grid_2, filename = "figures/grid_2.png", 
       width = 10, height = 8,dpi = 2000)


# BONUS -------------------------------------------------------------------

# Use a different package to combine plots
ggarrange(plot_1, box_2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1, legend = "bottom", common.legend = TRUE)



