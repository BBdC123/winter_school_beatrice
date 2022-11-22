# Complete_script
# 18.11.2022 - 

# DAY 1 Set-up R-----------------------------------------------------------------

library(tidyverse)
library(lubridate)

sst_monthly <- sst_NOAA %>% 
  mutate(month = month(t, label = T)) %>% 
  group_by(site, month) %>% 
  summarise(temp = round(mean(temp, na.rm = T), 3))
# mutate creates new variable -> here: t to months

# plots
ggplot(data = sst_monthly, aes(x = month, y = temp)) +
  geom_point(aes(colour = site)) +
  geom_line(aes(colour = site, group = site)) +
  labs(x = NULL, y = "Temperature (°C)")  

# facet plot
ggplot(data = sst_monthly, aes(x = month, y = temp)) +
  geom_point(aes(colour = site)) +
  geom_line(aes(colour = site, group = site)) +
  labs(x = "", y = "Temperature (°C)") +
  facet_wrap(~site, ncol = 1) # Create panels


# DAY 3 - PLOTTING -------------------------------------------------------------------

# Basic plotting ----------------------------------------------------------

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


# Faceting ----------------------------------------------------------------

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


#Linear model
lm_1 <- ggplot(data= penguins,
               aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth (method = "lm") +
  labs(x= "Body mass (g)", y="Bill length (mm)", colour = "Species")
lm_1



# Non-linear model 
nlm_1 <- ggplot(data= penguins,
               aes(x = body_mass_g, y = bill_length_mm, colour = species)) +
  geom_point() +
  geom_smooth () +
  labs(x= "Body mass (g)", y="Bill length (mm)", colour = "Species")
nlm_1


# Histogram
histogram_1 <- ggplot(data = penguins, 
                      # NB: There is no y-axis value for histograms
                      aes(x = body_mass_g)) + 
  geom_histogram(aes(fill = species), position = "stack", binwidth = 250) +
  # NB: We use 'fill' here rather than 'colour'
  labs(x = "Body mass (g)", fill = "Species")
histogram_1


# Boxplot 
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


# Colors ------------------------------------------------------------------

library(tidyverse) # Contains ggplot2
library(ggpubr) # Helps us to combine figures
library(palmerpenguins) # Contains dataset


### Continous color scales --------------------------------------------------
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


### Discrete color scales ---------------------------------------------------

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


# Plotting stats ----------------------------------------------------------

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


# DAY 4 - MAPPING ---------------------------------------------------------

library(tidyverse) # Contains most of what we need
library(ggpubr) # For combining figures
library(ggsn) # Contains code to make scale bars
library(palmerpenguins) # Data used in an example
library(maps)

# Basic mapping -----------------------------------------------------------


## Default map -------------------------------------------------------------

earth_1 <- ggplot() +
  # The global shape file
  borders(fill = "grey70", colour = "black") +
  # Equal sizing for lon/lat 
  coord_equal()
earth_1


## Cropping ----------------------------------------------------------------

green_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  # Force lon/lat extent
  coord_equal(xlim = c(-75, -10), ylim = c(58, 85)) 
green_1

#xlim sets limits for x axis, ylim for yaxis


## Extract a region --------------------------------------------------------

map_data('world') %>% 
  select(region) %>% 
  distinct() %>% 
  arrange(region)

map_data_green <- map_data('world') %>% 
  filter(region == "Greenland")
head(map_data_green)

#filter to just filter out when region column is same as Greenland
# == means equivalent to

#Map is the same as a scatterplot with x and y axis

ggplot(data = penguins, 
       aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point()

ggplot(data = map_data_green, 
       aes(x = long, y = lat)) +
  geom_point()


## Polygons ----------------------------------------------------------------

green_2 <- ggplot(data = map_data_green, aes(x = long, y = lat)) +
  # What is this doing?
  geom_polygon(aes(group = group), 
               # Note these are outside of aes() 
               fill = "chartreuse4", colour = "black")
green_2

# when using polygons we need to tell R what the group is
# map data is desgined to work with ggplot: column name is group
# group is a column inside the extracted data 
# group defines the different landmasses (i.e. islands, mainland)


## Labels ---------------------------------------------------------

green_3 <- green_2 +
  # Add Greenland text
  annotate("text", label = "Greenland", 
           x = -40, y = 75.0, size = 7.0, fontface = "bold.italic") +
  # Add North Atlantic Ocean text
  annotate("text", label = "North\nAtlantic\nOcean", 
           x = -20, y = 64.0, size = 5.0,  angle = 330, colour = "navy") +
  # Add Baffin Bay label
  annotate("label", label = "Baffin\nBay", 
           x = -62, y = 70, size = 5.0, fill = "springgreen") +
  # Add black line under Greenland text
  annotate("segment", 
           x = -50, xend = -30, y = 73, yend = 73)
green_3

# \n makes Zeilenbruch
# label is text box


## Scalebars ---------------------------------------------------------------

green_4 <- green_3 +
  # Set location of bar,
  scalebar(data = map_data_green, location = "bottomleft", 
           # Size of scale bar
           dist = 500, dist_unit = "km", transform = TRUE,
           # Set particulars
           st.size = 4, height = 0.03, st.dist = 0.04) 
green_4


## Insetting ---------------------------------------------------------------

earth_2 <- earth_1 + 
  geom_rect(aes(xmin = -75, xmax = -10, ymin = 58, ymax = 85),
            fill = NA, colour = "red") +
  # theme_void removes everything except geom
  # gives transparent background
  theme_void()
earth_2


# our world map on top of greenland map
green_5 <- green_4 +
  # Convert the earth plot to a grob
  annotation_custom(grob = ggplotGrob(earth_2), 
                    xmin = -30, xmax = -10,
                    ymin = 76, ymax = 84)
green_5

#make it more pretty
# x scale: go from -60 to -20 in steps of 20
# y scale: go from 60 to 80 in steps of 10
green_final <- green_5 +
  scale_x_continuous(breaks = seq(-60, -20, 20),
                     labels = c("60°W", "40°W", "20°w"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(60, 80, 10),
                     labels = c("60°N", "70°N", "80°N"),
                     position = "right") +
  labs(x = NULL, y = NULL) +
  theme_bw()
green_final


ggsave(plot = green_final, filename = "figures/greenland_final.pdf", 
       height = 6, width = 8)


# Style ---------------------------------------------------------------

library(tidyverse) # The base
library(marmap) # For downloading bathymetry data

# Base map
ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal()

## Fixing date-line issue ------------------------------------------------

# x axis is wrong because it goes to 200
# we rename long to lon
# use global fix everytime you map
map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon))


ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
  # The default coordinate system, with specific limits
  coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE)



# Bathymetric data --------------------------------------------------------

# Download bathymetric data
bathy_WA <-  getNOAA.bathy(lon1 = 111, lon2 = 117, 
                           # NB: smaller value first, i.e. more negative
                           lat1 = -36, lat2 = -19, 
                           # In degree minutes
                           resolution = 4)

# Convert to data.frame for use with ggplot2
bathy_WA_df <- fortify.bathy(bathy_WA) %>% 
  # Remove altimetry data (filters out depth data if depth > 0)
  filter(z <= 0) 

# Save
save(bathy_WA_df, file = "course_material/data/bathy_WA_df.RData")

# Report what was done in case someone else uses the script:
# Download bathy data
# bathy_WA <-  getNOAA.bathy(lon1 = 111, lon2 = 117, 
#                            lat1 = -36, lat2 = -19,
#                            resolution = 4)

# Convert to data.frame for use with ggplot2
# bathy_WA_df <- fortify.bathy(bathy_WA)

# Save
# save(bathy_WA_df, file = "../data/bathy_WA_df.RData")
# Load
load("course_material/data/bathy_WA_df.RData")


# Mapping bathymetry ---------------------------------------------------------

# Basic bathymetry map

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
  # Add 200 m contour
  geom_contour(data = bathy_WA_df, 
               aes(x = x, y = y, z = z),
               breaks = c(-200), 
               linewidth = c(0.3), colour = "grey") +
  coord_cartesian(xlim = c(111, 117), 
                  ylim = c(-36, -19), expand = FALSE)


# Contour lines, adding -2000

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
  # Add 200 and 2000 m contours
  geom_contour(data = bathy_WA_df, 
               aes(x = x, y = y, z = z),
               breaks = c(-200, -2000), 
               linewidth = c(0.3), colour = "grey") +
  coord_cartesian(xlim = c(111, 117), 
                  ylim = c(-36, -19), expand = FALSE)


# Change contour colors using aes

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
  # Rather use `aes()`
  geom_contour(data = bathy_WA_df, 
               aes(x = x, y = y, z = z, colour = after_stat(level)),
               linewidth = c(0.3)) +
  coord_cartesian(xlim = c(111, 117), 
                  ylim = c(-36, -19), expand = FALSE)


# Discrete color palettes

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
  # Combine `aes()` and `breaks = c()` for more control
  geom_contour(data = bathy_WA_df, 
               aes(x = x, y = y, z = z, colour = after_stat(level)),
               breaks = c(-50, -200, -1000, -2000), 
               linewidth = c(0.3)) +
  # Also change colour palette
  scale_colour_distiller(palette = "BuPu") + 
  coord_cartesian(xlim = c(111, 117), 
                  ylim = c(-36, -19), expand = FALSE)

# Tidy up appearance

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
  # create discrete factors
  geom_contour(data = bathy_WA_df, 
               aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))),
               breaks = c(-50, -200, -1000, -2000), 
               linewidth = c(0.3)) +
  # Use discrete palette
  scale_colour_brewer("Depth [m]", palette = "Set1", direction = -1) +  
  # Reverse legend order and make symbols thicker (otherwise it would be thinn lines)
  guides(colour = guide_legend(reverse = TRUE, 
                              override.aes = list(linewidth = 5))) +
  coord_cartesian(xlim = c(111, 117), 
                  ylim = c(-36, -19), expand = FALSE)



# Ocean temperature -------------------------------------------------------

load("course_material/data/OISST_2000.RData")


# Plot Western Australia

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), 
               colour = "black", fill = "grey60") +
  geom_raster(data = OISST_2000, aes(fill = temp)) +
  coord_cartesian(xlim = c(111, 117), 
                  ylim = c(-36, -19), expand = FALSE)


# Combining layers

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  # First layer
  geom_raster(data = OISST_2000, aes(fill = temp)) + 
  # Second layer
  geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
  # Third layer
  geom_contour(data = bathy_WA_df, 
               aes(x = x, y = y, z = z, colour = as.factor(after_stat(level))), 
               breaks = c(-50, -200, -1000, -2000), 
               linewidth = c(0.3)) +
  guides(color = guide_legend(reverse = TRUE, 
                              override.aes = list(linewidth = 5))) + 
  scale_fill_viridis_c("Temperature [°C]") +
  scale_colour_brewer("Depth [m]", palette = "BuPu") +
  coord_cartesian(xlim = c(111, 117), 
                  ylim = c(-36, -19), expand = FALSE)


# Finishing touches

final_map <- ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_raster(data = OISST_2000, aes(fill = temp)) +
  geom_polygon(aes(group = group), colour = "black", fill = "grey60") +
  geom_contour(data = bathy_WA_df,
               aes(x = x, y = y, z = z, 
                   colour = as.factor(after_stat(level))), 
               breaks = c(-50, -200, -1000, -2000), 
               linewidth = c(0.3)) +
  guides(color = guide_legend(reverse = TRUE, 
                              override.aes = list(linewidth = 5))) + 
  scale_fill_viridis_c("Temperature [°C]") +
  scale_colour_brewer("Depth [m]", palette = "BuPu") +
  coord_cartesian(xlim = c(111, 117), ylim = c(-36, -19), expand = FALSE) +
  # Put x axis labels on top of figure and assign °E
  scale_x_continuous(position = "top", 
                     breaks = c(112, 114, 116), 
                     labels = c("112°E", "114°E", "116°E")) + 
  # Put y axis labels on right of figure and assign °S
  scale_y_continuous(position = "right",
                     breaks = c(-34, -28, -22), 
                     labels = c("34°S", "28°S", "22°S")) +
  # Remove the axis label text
  theme(axis.title = element_blank(),
        # Add black border
        panel.border = element_rect(fill = NA, colour = "black"), 
        # Change text size in legend
        legend.text = element_text(size = 7), 
        # Change legend title text size
        legend.title = element_text(size = 7), 
        # Change size of legend
        legend.key.height = unit(0.5, "cm"),
        # Add legend background
        legend.background = element_rect(fill = "white", colour = "black"),
        # Change position of legend
        legend.position = c(0.9, 0.5)
  )


final_map

ggsave(plot = final_map, "figures/map_complete.pdf", height = 6, width = 9)


# Mapping the Arctic ------------------------------------------------------------------

library(tidyverse)
library(ggOceanMaps)
library(ggOceanMapsData)

# ggOceanMapsData is not on CRAN
# install remotely
remotes::install_github("MikkoVihtakari/ggOceanMapsData")


# Fixed base map
map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon))


# Load sea surface temperatures for 2022-01-01
load("course_material/data/OISST_2022.RData")



## Cartesian projections ---------------------------------------------------

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Numeric sizing for lon/lat 
  coord_cartesian()

## Equal projections -------------------------------------------------------

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Equal sizing for lon/lat 
  coord_equal()


# Fixed projections -------------------------------------------------------
# fixed values for x and y axis

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Ratio (Y divided by X) sizing for lon/lat 
  coord_fixed(ratio = 2)


# Quick map ---------------------------------------------------------------
# Good default

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  # Behind the scenes this adapts the "mercator" projection
  coord_quickmap()


# Polar projections -------------------------------------------------------

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  scale_y_reverse() +
  # A very different projection
  coord_polar()

#orthographic projection
ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  coord_map(projection = "ortho", orientation = c(90, 0, 0))



# ggOceanMaps -------------------------------------------------------------

# just one line for good plot, works everywhere, not only polar regions
basemap(limits = 60)

# has bathymetry build into it
basemap(limits = c(100, 160, -20, 30), bathymetry = TRUE)

citation("tidyverse")

citation("base")
