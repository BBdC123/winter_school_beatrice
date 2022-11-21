# plots_1_basics
# Beatrice Brix da Costa
# 21.11.22


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(palmerpenguins)


# Data --------------------------------------------------------------------

# Load the dataset into the local environment
penguins <- penguins


# Example -----------------------------------------------------------------

# The basic plot
ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = species))


# Exercise 1 --------------------------------------------------------------

# Create a basic plot with different x and y axes

View (penguins)

ggplot(data= penguins,
       aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()


# Exercise 2 --------------------------------------------------------------

# Change the aes() arguments

ggplot(data= penguins,
       aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(colour = sex, shape=island))



# Exercise 3 --------------------------------------------------------------

# Change the labels

ggplot(data= penguins,
       aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(colour = sex, shape=island))+
  labs (x= "Bill length (mm)", y = "Bill depth (mm)", colour = "Sex", shape = "Island")


# BONUS -------------------------------------------------------------------

# Create a ridgeplot

library(ggridges)
library(ggplot2)

ggplot(penguins, aes(x = body_mass_g, y = island, fill = island)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  labs (x = "Body mass (g)", y = NULL)

# remove labels with y="", or y=NULL
