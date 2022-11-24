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
# map data is designed to work with ggplot: column name is group
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
                     labels = c("60°W", "40°W", "20°W"),
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


# DAY 5 - TIDY DATA ---------------------------------------------------------

library(tidyverse)

load("course_material/data/OISST_mangled.RData")


## Long and Wide format ----------------------------------------------------

# OISST1
head(OISST1)

# Simple line plot of the **tidy** `OISST1` dataset

ggplot(data = OISST1, aes(x = t, y = temp)) +
  geom_line(aes(colour = site)) +
  labs(x = NULL, y = "Temperature (°C)", colour = "Site") +
  theme_bw()


#OISST 2
head(OISST2)

# pivot_longer to convert from wide to long
OISST2_tidy <- OISST2 %>%
  pivot_longer(cols = c(Med, NW_Atl, WA), 
               names_to = "site", values_to = "temp")
head(OISST2_tidy)


#OISST3
head(OISST3)

#pivot_wider to convert from long to wide
OISST3_tidy <- OISST3 %>% 
  pivot_wider(id_cols = c(idx,temp), names_from = type, values_from = name)
head(OISST3_tidy)


## Separating and Uniting --------------------------------------------------

#sites a

head(OISST4b)

# Unite year, month, and datnd dates in same column, not useful
head(OISST4a)

#col= which column we want to separate
#into= new columns we want to create
#sep =separating by whatever vaues are separated in data
OISST4a_tidy <- OISST4a %>% 
  separate(col = index, into = c("site", "t"), sep = " ")
head(OISST4a_tidy)
e
OISST4b_tidy <- OISST4b %>% 
  unite(year, month, day, col = "t", sep = "-")
head(OISST4b_tidy)


# Joining
head(OISST4a_tidy)
head(OISST4b_tidy)

#4a no index data
#4b no temperature data
#left_join will joins left 4a to the left with 4b
OISST4_tidy <- left_join(OISST4a_tidy, OISST4b_tidy)
head(OISST4_tidy)


# Taming data ------------------------------------------------------------------

library(tidyverse)
library(lubridate) # For working with dates

load("course_material/data/OISST_mangled.RData")

# Tidy transformations:
# Arrange observations (rows) with `arrange()
# Select variables (columns) with`select()
# Filter observations (rows) with `filter()  
# Create new variables (columns) with `mutate()  
# Summarise data (rows+columns) with `summarise()


## Arrange -----------------------------------------------------------------

# arrange by site and temperature
OISST1 %>% 
  arrange(site, temp) %>% 
  head()


## Select ------------------------------------------------------------------

# Select columns individually by name
OISST1 %>% 
  select(site, t, temp)

# Select all columns between site and temp like a sequence
OISST1 %>% 
  select(site:temp)

# Select all columns except those stated individually
OISST1 %>% 
  select(-t, -temp)

# Select all columns except those within a given sequence
# Note that the '-' goes outside of a new set of brackets
# that are wrapped around the sequence of columns to remove
OISST1 %>% 
  select(-(site:temp))

# Change up order by specifying individual columns
OISST1 %>% 
  select(temp, t, site)

# Use the everything function to grab all columns 
# not already specified
OISST1 %>% 
  select(t, everything())

# Or go bananas and use all of the rules at once
# Remember, when dealing with tidy data,
# everything may be interchanged
OISST1 %>% 
  select(temp:t, everything(), -site)


## Filter ------------------------------------------------------------------
# filter automatically removes rows with NA

# filter site Med and values of December OR January
OISST1_sub <- OISST1 %>% 
  filter(site == "Med", month(t) == 12 | month(t) == 1)


OISST1 %>% 
  filter(site == "Med", 
         year(t) == 2008) %>% 
  head()


# %in% operator to include multiple statements
# we take sites that are in Med and WA for year 2009
OISST1 %>% 
  filter(site %in% c("Med", "WA"), 
         year(t) == 2009) %>% 
  tail()


## Mutate ------------------------------------------------------------------

# creating new variable (Kelvin)
OISST1 %>% mutate(kelvin = temp + 273.15) %>% head()

OISST1 %>% 
  mutate(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  head()
# not very useful, better to use summarize


## Summarize ---------------------------------------------------------------

OISST1 %>% summarise(mean_temp = mean(temp, na.rm = TRUE)) %>%
  head()

OISST1 %>% summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))

# still not very useful because we want to have the mean temp for sites

# group by sites beforehand
OISST1 %>% group_by(site) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>%
  head()

OISST1 %>% group_by(site) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))


## Grouping ----------------------------------------------------------------

library(tidyverse)
library(lubridate)

sst_NOAA <- read_csv("course_material/data/sst_NOAA.csv")


sst_NOAA_site <- sst_NOAA %>% group_by(site)
sst_NOAA %>% head()
sst_NOAA_site %>% head()
# looks the same but is not

sst_NOAA %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  head()
# one overall mean, does not make sense

sst_NOAA_site %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  head()
# now we get the means for all the groups

# ungrouping
sst_NOAA_ungroup <- sst_NOAA_site %>% ungroup()

sst_NOAA_site %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  head()

sst_NOAA_ungroup %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) %>% 
  head()

# Multiple groups

# Create groupings based on temperatures, takes temperatures that round to one digit
sst_NOAA_temp_group <- sst_NOAA %>% 
  group_by(round(temp))

# Create groupings based on site and month
sst_NOAA_temp_month_group <- sst_NOAA %>% 
  mutate(month = month(t)) %>% 
  group_by(site, month)


# Generally one does not group objects separately
# grouping is performed in chunks

sst_NOAA_site_mean <- sst_NOAA %>% 
  # Group by the site column
  group_by(site) %>% 
  # Calculate means
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            # Count observations 
            count = n(),
            # Ungroup results
            .groups = "drop") 
sst_NOAA_site_mean


# Examples ----------------------------------------------------------------

# Filter sites that don't have a max temperature above 20°C
sst_NOAA_20 <- sst_NOAA %>%
  group_by(site) %>%
  filter(temp > 20) %>% 
  ungroup()
unique(sst_NOAA_20$site)
head(sst_NOAA_20)

# Calculate anomalies for each site 
sst_NOAA_anom <- sst_NOAA %>%
  group_by(site) %>% 
  mutate(anom = temp - mean(temp, na.rm = T)) %>%
  ungroup()
head(sst_NOAA_anom)

# Calculate mean and standard deviations for two sites
sst_NOAA %>% 
  filter(site == "Med" | site == "WA") %>%
  group_by(site) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE)) %>%
  ungroup


# Calculate mean and standard deviations for two sites
# First create a character vector containing the desired sites
selected_sites <- c("Med", "WA")

# Then calculate the statistics
sst_NOAA %>% 
  filter(site %in% selected_sites) %>%
  group_by(site) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE))

# Only days with temperatures above 10°C and below 15°C
sst_NOAA %>% 
  filter(site == "Med", 
         temp > 10, temp < 15) %>% 
  nrow()

# Only days with temperatures not below 10 and not higher than 15°C
sst_NOAA %>% 
  filter(site == "Med", 
         !(temp <= 10 | temp  >= 15)) %>% 
  nrow()

# Change system language to english -> month in plot will be displayed in en
Sys.setlocale(category="LC_TIME", locale="en")
Sys.setlocale(category="LC_TIME", locale="german")

# The new age redux 
# Load the SACTN Day 1 data
read_csv("course_material/data/sst_NOAA.csv") %>%
  # Then create a month abbreviation column
  mutate(month = month(t, label = TRUE)) %>% 
  # Then group by sites and months
  group_by(site, month) %>% 
  # Lastly calculate the mean
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            # and the SD
            sd_temp = sd(temp, na.rm = TRUE)) %>% 
  # Begin ggplot
  ggplot(aes(x = month, y = mean_temp, group = site)) + 
  # Create a ribbon
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
              fill = "black", alpha = 0.4) + 
  # Create dots
  geom_point(aes(colour = site)) + 
  # Create lines
  geom_line(aes(colour = site, group = site)) + 
  # Change labels
  labs(x = "Month", y = "Temperature (°C)", colour = "Site") 


# Summary functions
# The proportion of recordings above 20°C per site

sst_NOAA %>%  
  group_by(site) %>%
  summarise(count = n(), 
            count_20 = sum(temp > 20)) %>% 
  mutate(prop_20 = count_20/count) %>% 
  arrange(prop_20) %>% ungroup()

# Look at all the datasets in R
data(package = .packages(all.available =TRUE))

MASS:crabs

crabs <- MASS::crabs
