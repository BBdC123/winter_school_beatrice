# NEW ---------------------------------------------------------------------

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggOceanMaps)
library(remotes)

#for borders
world_sf <- ne_countries(scale = "medium", returnclass = "sf")
class(world_sf)


#test if sf uploaded right
world_plot <- ggplot(data = world_sf) +
  geom_sf()+
  theme_set(theme_bw())
world_plot


#make fancy map

#note: SQ is Nora's status quo project
VN_Map <- basemap(limits = c(95, 120, 3, 35),
                  rotate = F, bathymetry = F, glaciers = F)+
  scale_x_continuous(breaks = seq(100, 112, 4),
                     labels = c("100°O", "104°O", "108°O","112°O"),
                     position ="bottom",
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(5, 30, 5),
                     labels = c("5°N", "10°N","15°N","20°N", "25°N", "30°N"),
                     position = "right")+
  labs(x = "", y = "")+
  ggtitle("Map Vietnam") 

VN_Map


###########################

# global map fix

map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon))

map_global <- ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "gray", colour = "black")+
  coord_quickmap()
map_global

###
# coord vn : lon 100 - 130, lat 10,25

#######

library(sf)
library(sfheaders)

# Load shapefile
coastline_full <- read_sf("C:/Users/bbrix/Documents/GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp")

# Convert to data.frame
coastline_full_df <- sf_to_df(coastline_full, fill = TRUE)
coastline_full <- read_sf("C:/Users/bbrix/Documents/GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp")
coastline_full_df <- sf_to_df(coastline_full, fill = TRUE)


# Filter to Vietnam and plot
# NB: filter much wider than necessary to ensure
# that you get enough of the polygon to avoid issues
vietnam_new <- coastline_full_df %>% 
  filter(between(x, 90, 150),
         between(y, 0, 30)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(group = id), 
               fill = "grey70", colour = "black") +
  coord_quickmap(expand = FALSE,
                 xlim = c(102, 110), ylim = c(7.5, 25))+
  borders()

vietnam_new
