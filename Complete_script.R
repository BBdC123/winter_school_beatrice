# Complete_script
# 18.11.2022 - 

# DAY 1 Set-up R-----------------------------------------------------------------


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


## 1) Basic plotting ----------------------------------------------------------


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



## 2) Faceting ----------------------------------------------------------------


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




## 3) Colors ------------------------------------------------------------------


library(tidyverse) # Contains ggplot2
library(ggpubr) # Helps us to combine figures
library(palmerpenguins) # Contains dataset



## Continous color scales --------------------------------------------------
# variables = numbers: gradient of colors

ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm))


ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm)) +
  # Change the continuous variable colour palette
  scale_colour_distiller() 


ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm)) +
  # Choose a pre-set palette
  scale_colour_distiller(palette = "Spectral")


ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm)) +
  # Viridis colour palette (c for continous, d if discrete)
  scale_colour_viridis_c(option = "B")


## Discrete color scales ---------------------------------------------------

ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(year))) +
  # The discrete colour palette function
  scale_colour_brewer()

ggplot(data = penguins,
      aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(year))) +
  # Choose a colour palette - scale color brewer always for discrete values
  scale_colour_brewer(palette = "Set1")


ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(year))) +
  # Discrete viridis colour palette
  scale_colour_viridis_d(option = "D")

# continous values use distiller
# discrete values use brewer


# Make own color palettes: scale_colour_gradientn
# scale_colour_gradient default: from red to blue
# scale colour_gradient2 blue, white, gray


# Continuous
ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm)) +
  scale_colour_gradientn(colours = c("#4E272A", "#683D4E", "#755977",
                                     "#727B9E", "#5F9FBA", "#4CC3C5"))


# Discrete
ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(sex))) +
  # How to use custom palette
  scale_colour_manual(values = c("#AA005D", "#008100"),
                      # How to change the legend text
                      labels = c("female", "male", "other")) + 
  # How to change the legend title
  labs(x="Body mass (g)", y= "Bill length (mm)", colour = "Sex") 





# 4) Plotting stats ----------------------------------------------------------



# t-test
compare_means(bill_length_mm~sex, data = penguins, method = "t.test")
# ANOVA
compare_means(bill_length_mm~species, data = penguins, method = "anova")

# stat_* adds stats into plots

ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
  #in geom you always have show.legend. if you want to remove it put =F
  geom_boxplot(aes(fill = species), show.legend = F) +
  stat_compare_means(method = "anova")

ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
  geom_boxplot(aes(fill = species), show.legend = F) +
  stat_compare_means(method = "anova", 
                     aes(label = paste0("p ", ..p.format..)), 
                     label.x = 2) +
  theme_bw()

#Multiple means

# First create a list of comparisons to feed into our figure
penguins_levels <- levels(penguins$species)
my_comparisons <- list(c(penguins_levels[1], penguins_levels[2]), 
                       c(penguins_levels[2], penguins_levels[3]),
                       c(penguins_levels[1], penguins_levels[3]))
# Then we stack it all together
ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
  geom_boxplot(aes(fill  = species), colour = "grey40", show.legend = F) +
  stat_compare_means(method = "anova", colour = "grey50",
                     label.x = 1.8, label.y = 32) +
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons,
                     label.y = c(62, 64, 66)) +
  # Perform t-tests between each group and the overall mean
  stat_compare_means(label = "p.signif", 
                     method = "t.test",
                     ref.group = ".all.") + 
  # Add horizontal line at base mean
  geom_hline(yintercept = mean(penguins$bill_length_mm, na.rm = T), 
             linetype = 2) + 
  labs(y = "Bill length (mm)", x = NULL) +
  theme_bw()

