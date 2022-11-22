# Script name
# Author
# Date


# Libraries ---------------------------------------------------------------

# Which libraries should be loaded?
library(tidyverse) 
library(ggpubr) 
library(ggsn) 
library(maps)

# Data --------------------------------------------------------------------

# Call the global data to the environment
map_data_world <- map_data("world")


# Example -----------------------------------------------------------------

# The basic map
map_data_world %>% 
  filter(region == "Germany") %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group))


# Exercise 1 --------------------------------------------------------------

# Create maps of four regions and combine
# Use a mix of cropping and direct access 

map_data('world') %>% 
  select(region) %>% 
  distinct() %>% 
  arrange(region)


# Germany - direct access
# extract data
map_data_germany <- map_data('world') %>% 
  filter(region == "Germany")

#make map
germany <- ggplot(data = map_data_germany, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "#cbc9e2", colour = "black")
germany

germany_1 <- germany +
  annotate("text", label = "Germany", 
           x = 10, y = 51, size = 7.0, fontface = "bold.italic")
germany_1

germany_final <- germany_1 +
  scale_x_continuous(breaks = seq(5, 15, 5),
                     labels = c("5°O", "10°O", "15°O"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(45, 55, 5),
                     labels = c("45°N", "50°N", "55°N"),
                     position = "right") +
  labs(x = NULL, y = NULL) +
  theme_bw()
germany_final


# Germany - cropping
sylt <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  # Force lon/lat extent
  coord_equal(xlim = c(8.25, 8.65), ylim = c(54.5, 55.2)) 
sylt


# Vietnam
map_data_vietnam <- map_data('world') %>% 
  filter(region == "Vietnam")

vietnam <- ggplot(data = map_data_vietnam, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "#9e9ac8", colour = "black")
vietnam

vietnam_1 <- vietnam + 
  annotate("text", label = "Vietnam", 
            x = 105.25, y = 22, size = 5.0, fontface = "bold.italic")
  
vietnam_1


#Portugal
map_data_portugal <- map_data('world') %>%
  filter(region == 'Portugal')

portugal <- ggplot(data = map_data_portugal, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "#6a51a3", colour = "black")
portugal


# combine plots
grid_maps <- ggarrange(germany_final, sylt, vietnam_1, portugal,
                    ncol = 2, nrow = 2,
                    labels = c("A", "B", "C", "D"),
                    legend = "bottom")
grid_maps


ggsave(plot = grid_maps, filename = "figures/exercise_maps.pdf", 
       height = 6, width = 8)


# Exercise 2 --------------------------------------------------------------

# Create a map that benefits from a scale bar and add a North arrow
# Hint: use annotate("segment", ...) to accomplish this


# fix world data and filter Vietnam data
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



# add scalebar
vietnam_2 <- vietnam +
  scalebar(data = map_data_vietnam, location = "bottomright", 
           dist = 150, dist_unit = "km", transform = TRUE,
           st.size = 2, height = 0.02, st.dist = 0.03) 

vietnam_2

# add north arrow

vietnam_3 <- vietnam_2 +
  scalebar(data = map_data_vietnam, location = "bottomright", 
           dist = 150, dist_unit = "km", transform = TRUE,
           st.size = 2, height = 0.02, st.dist = 0.03) +
  annotation_north_arrow( height = unit(0.5, "cm"),
                           width = unit(0.5, "cm"),
                           pad_x = unit(0.25, "cm"),
                           pad_y = unit(0.25, "cm"),
                           rotation = NULL,
                           style = north_arrow_orienteering)

vietnam_3

# add nice axes
vietnam_4 <- vietnam_3 +
  scale_x_continuous(breaks = seq(100, 112, 4),
                     labels = c("100°O", "104°O", "108°O","112°O"),
                     position = "bottom") +
  scale_y_continuous(breaks = seq(5, 30, 5),
                     labels = c("5°N", "10°N","15°N", "20°N", "25°N", "30°N"),
                     position = "right") +
  labs(x = NULL, y = NULL) +
  theme_bw()
vietnam_4




# Exercise 3 --------------------------------------------------------------

# Create a meaningful inset map


#inset Nha Trang




# BONUS -------------------------------------------------------------------

# Plot maps using Google Maps

