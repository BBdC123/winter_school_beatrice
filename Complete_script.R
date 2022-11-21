# Complete_script
# 18.11.2022 - 

# DAY 1 -------------------------------------------------------------------


library(tidyverse)
library(lubridate)

sst_monthly <- sst_NOAA %>% 
  mutate(month = month(t, label = T)) %>% 
  group_by(site, month) %>% 
  summarise(temp = round(mean(temp, na.rm = T), 3))

ggplot(data = sst_monthly, aes(x = month, y = temp)) +
  geom_point(aes(colour = site)) +
  geom_line(aes(colour = site, group = site)) +
  labs(x = NULL, y = "Temperature (°C)")  


ggplot(data = sst_monthly, aes(x = month, y = temp)) +
  geom_point(aes(colour = site)) +
  geom_line(aes(colour = site, group = site)) +
  labs(x = "", y = "Temperature (°C)") +
  facet_wrap(~site, ncol = 1) # Create panels

# DAY 3 - Plotting -------------------------------------------------------------------


## Basic plotting ----------------------------------------------------------


library(tidyverse)
library(palmerpenguins)

View(penguins)

ggplot(data= penguins,
        aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = species, shape = island, size=flipper_length_mm))

## columns from data go inside of aes()
# everything inside aes() will create a legend

# add a geom --> adds fitted models lm = linear model
ggplot(data= penguins,
       aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
geom_smooth (method = "lm") +
  # Change label text
  labs(x= "Body mass (g)", y="Bill length (mm)", colour = "Species")+
  # Change legend position
  theme (legend.position = "bottom")



## Faceting ----------------------------------------------------------------


library

ggplot(data= penguins,
       aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth (method = "lm") +
  labs(x= "Body mass (g)", y="Bill length (mm)", colour = "Species")+
  theme (legend.position = "bottom") +
  # Can take one or two column names
  facet_wrap(~species)


ggplot(data= penguins,
       aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth (method = "lm") +
  labs(x= "Body mass (g)", y="Bill length (mm)", colour = "Species")+
  theme (legend.position = "bottom") +
  # Can take two or more column names (e.g. species+sex~island)
  facet_grid(island~species)

# Assigning the ggplot2 code to an object name


## Linear model ------------------------------------------------------------


lm_1 <- ggplot(data= penguins,
               aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth (method = "lm") +
  labs(x= "Body mass (g)", y="Bill length (mm)", colour = "Species")
lm_1



## Non-linear model --------------------------------------------------------


nlm_1 <- ggplot(data= penguins,
               aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth () +
  labs(x= "Body mass (g)", y="Bill length (mm)", colour = "Species")
nlm_1



## Histogram ---------------------------------------------------------------


histogram_1 <- ggplot(data = penguins, 
                      # NB: There is no y-axis value for histograms
                      aes(x = body_mass_g)) + 
  geom_histogram(aes(fill = species), position = "stack", binwidth = 250) +
  # NB: We use 'fill' here rather than 'colour'
  labs(x = "Body mass (g)", fill = "Species")
histogram_1



## Boxplot -----------------------------------------------------------------


box_1 <- ggplot(data = penguins, 
                # Why 'as.factor()'?
                aes(x = as.factor(year),
                    y = body_mass_g)) + 
  geom_boxplot(aes(fill = species)) +
  labs(x = "Year", y = "Body mass (g)", fill = "Species") 
box_1



## Combining plots ---------------------------------------------------------


grid_1 <- ggarrange(lm_1, nlm_1, histogram_1, box_1,
                    # Set number of rows and columns
                    ncol = 2, nrow = 2,
                    # Label each figure
                    labels = c("a)", "b)", "c)", "d)"),
                    # Create common legend
                    common.legend = TRUE,
                    # Set legend position
                    legend = "bottom")
grid_1


## Saving ------------------------------------------------------------------


# Different file types
ggsave(plot = grid_1, filename = "figures/grid_1.pdf")
ggsave(plot = grid_1, filename = "figures/grid_1.png")
ggsave(plot = grid_1, filename = "figures/grid_1.eps")

# Change dimensions
ggsave(plot = grid_1, filename = "figures/grid_1.png", 
       width = 10, height = 8)

# Change DPI
ggsave(plot = grid_1, filename = "figures/grid_1.png", dpi = 600)











