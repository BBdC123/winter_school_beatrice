
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggOceanMaps)

# ggOceanMapsData is not on CRAN
# install remotely
remotes::install_github("MikkoVihtakari/ggOceanMapsData")

library(ggOceanMapsData)


# Data --------------------------------------------------------------------

# Sea surface temperatures for 2022-01-01
load("course_material/data/OISST_2022.RData")


# Example -----------------------------------------------------------------

# An Arctic plot
basemap(limits = c(-160, -80, 60, 85), rotate = TRUE)


# Exercise 1 --------------------------------------------------------------

# Directly access the shape of a region near a pole
# Plot with coord_map(projection = "ortho")

# Fixed base map
map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon))

ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group)) +
  coord_map(projection = "ortho", orientation = c(90, 0, 0),
            xlim = c(10,34), ylim = c(74,81))


# Exercise 2 --------------------------------------------------------------

# Add bathymetry to this plot

basemap(limits = c(10, 34, 74, 81), bathymetry = TRUE)


# Exercise 3 --------------------------------------------------------------

# Use ggoceanmaps to create a similar plot


# BONUS -------------------------------------------------------------------

# Create a workflow for creating a polar plot for any region
# Add a red bounding box to highlight a region
# And different coloured points to show study sites

# fix world data and filter region (here:Vietnam)
map_global_fix_vn <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon)) %>%
  filter(region == "Vietnam")

#plot
vietnam <- ggplot(data = map_global_fix_vn, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "gray", colour = "black")+
  coord_equal()

vietnam



# zooming into specific region (here:Nha Trang)
plot_nha_trang <- vietnam +
  coord_fixed(ratio = 0.5, xlim=c(108.75, 109.5), ylim=c(11,15))+
  geom_rect(aes(xmin=108.75, xmax=109.5, ymin=11, ymax=15),
            fill=NA, colour="red", linewidth = 1)+
  theme_void()

plot_nha_trang 


# region map on top of country map
vietnam_inset <- vietnam +
  annotation_custom(grob = ggplotGrob(plot_nha_trang), 
                    xmin = 102, xmax = 106,
                    ymin = 12, ymax = 16)
vietnam_inset

#make it more pretty
vn_final <- vietnam_inset +
  scale_x_continuous(breaks = seq(100, 112, 4),
                     labels = c("100°O", "104°O", "108°O","112°O"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(5, 30, 5),
                     labels = c("5°N", "10°N","15°N", "20°N", "25°N", "30°N"),
                     position = "right") +
  labs(x = NULL, y = NULL) +
  theme_bw()
vn_final

# instead of insetting one can create multiple plots and combine them


# add box around Nha Trang
plot_vn <- vietnam +
  coord_fixed(xlim=c(108, 110.25), ylim=c(10.5,15))+
  geom_rect(aes(xmin=108.5, xmax=109.5, ymin=11, ymax=15),
            fill=NA, colour="red", linewidth = 1)+
  coord_quickmap()

plot_vn

# create plot for Nha Trang




# combine plots
grid_vn <- ggarrange(vietnam, plot_nha_trang,
                       ncol = 2, nrow = 2,
                       labels = c("A", "B"),
                       legend = "bottom")
grid_vn


ggsave(plot = grid_vn, filename = "figures/grid_vn.pdf", 
       height = 6, width = 8)





