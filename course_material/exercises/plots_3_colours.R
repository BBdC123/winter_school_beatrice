# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(palmerpenguins)
library(lubridate)
library(GGally)


# Data --------------------------------------------------------------------

# Load the dataset into the local environment
penguins <- penguins

sst_NOAA <- read_csv("course_material/data/sst_NOAA.csv")

# if exported from excel
# read_delim("course_material/data/sst_NOAA.csv", delim = ";")

# Analyses ----------------------------------------------------------------

# Look at parts of the data frame
sst_NOAA
head(sst_NOAA)
tail(sst_NOAA)

#Quick summaries
glimpse(sst_NOAA)
str(sst_NOAA)
summary(sst_NOAA)


# Example -----------------------------------------------------------------

# Discrete viridis colour palette
ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(year))) +
  scale_colour_viridis_d(option = "A")

# Compare species
ggplot(data = penguins, aes(x = species, y = bill_length_mm)) +
  geom_boxplot(aes(fill = species), show.legend = F) +
  stat_compare_means(method = "anova")


# Exercise 1 --------------------------------------------------------------

# Create your own continuous and discrete colour palettes
# Create and combine two figures, each using a different palette

# Continuous
plot_1 <- ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm)) +
  scale_colour_gradientn(colours = c("#4E272A", "#683D4E", "#755977",
                                     "#727B9E", "#5F9FBA", "#4CC3C5"))+
  labs(x="Body mass (g)", y="Bill length (mm)", colour= "Bill depth (mm)")

#Discrete
plot_2 <- ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = as.factor(sex))) +
  scale_colour_manual(values = c("#AA005D", "#008100"),
                      labels = c("female", "male", "other")) + 
  labs(x="Body mass (g)", y= "Bill length (mm)", colour = "Sex") 


grid_colors <- ggarrange(plot_1, plot_2,
                    ncol = 2, nrow = 1,
                    # Label each figure
                    labels = c("A", "B"),
                    legend = "bottom")
grid_colors


ggsave(plot = grid_colors, filename = "figures/grid_colors.png", 
       width = 10, height = 8, dpi = 800)

# Exercise 2 --------------------------------------------------------------

# Create two versions of the same figure and combine
# Use a viridis colour palette against a default palette in a way that 
# allows features in the data to be more pronounced


# Default
plot_default <- ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm))+
  labs(x="Body mass (g)", y= "Bill length (mm)", colour = "Bill depth (mm)")

# Viridis
plot_viridis <- ggplot(data = penguins,
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(colour = bill_depth_mm)) +
  scale_colour_viridis_c(option = "B")+
  labs(x="Body mass (g)", y= "Bill length (mm)", colour = "Bill depth (mm)")

grid_colors2 <- ggarrange(plot_default, plot_viridis,
                         ncol = 2, nrow = 1,
                         # Label each figure
                         labels = c("A", "B"),
                         legend = "bottom")


grid_colors2

# Exercise 3 --------------------------------------------------------------

# Plot and combine t-test and ANOVA stats using sst_NOAA
# See this site for more info on plotting stats:
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/


# first plots to get overview

sst_monthly <- sst_NOAA %>% 
  mutate(month = month(t, label = T)) %>% 
  group_by(site, month) %>% 
  summarise(temp = round(mean(temp, na.rm = T), 3))

ggplot(data = sst_monthly, aes(x = month, y = temp)) +
  geom_point(aes(colour = site)) +
  geom_line(aes(colour = site, group = site)) +
  labs(x = NULL, y = "Temperature (°C)")  

# facet
ggplot(data = sst_monthly, aes(x = month, y = temp)) +
  geom_point(aes(colour = site)) +
  geom_line(aes(colour = site, group = site)) +
  labs(x = "", y = "Temperature (°C)") +
  facet_wrap(~site, ncol = 1) 


# ANOVA
compare_means(temp~site, data = sst_NOAA, method = "anova")

ggplot(data = sst_NOAA, aes(x = site, y = temp)) +
  geom_boxplot(aes(fill = site), show.legend = F) +
  labs(x=NULL, y="Temperature (°C)")+
  stat_compare_means(method = "anova",
                     aes(label = paste0("p ", ..p.format..)), 
                     label.x = 2) +
  theme_bw()


#Multiple means
#change site to factor
sst_NOAA$sites <- as.factor(sst_NOAA$site)

# Create a list of comparisons 
sites_levels <- levels(sst_NOAA$sites)
my_comparisons <- list(c(sites_levels [1], sites_levels [2]), 
                       c(sites_levels [2], sites_levels [3]),
                       c(sites_levels [1], sites_levels [3]))
# Stack it all together
sst_stats <- ggplot(data = sst_NOAA, aes(x = sites, y = temp)) +
  geom_boxplot(aes(fill  = sites), colour = "grey40", show.legend = F) +
  stat_compare_means(method = "anova", colour = "grey50",
                     label.x = 0.8, label.y = 0.8) +
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons,
                     label.y = c(30, 32, 34)) +
  # Perform t-tests between each group and the overall mean
  stat_compare_means(label = "p.signif", 
                     method = "t.test",
                     ref.group = ".all.") + 
  # Add horizontal line at base mean
  geom_hline(yintercept = mean(sst_NOAA$temp, na.rm = T), 
             linetype = 2) + 
  labs(y = "Temperature (°C)", x = NULL) +
  theme_bw()


ggsave(plot = sst_stats, filename = "figures/sst_stats.png", 
       width = 10, height = 8, dpi=800)

# BONUS -------------------------------------------------------------------

# Create a correlogram

ggpairs(penguins, title="Correlogram") 



ggcorr(penguins, method = c("site"))
ggcorr(data, method = c("everything", "pearson")) 

p_ <- GGally::print_if_interactive
p_(ggcorr(penguins[, 2]))
