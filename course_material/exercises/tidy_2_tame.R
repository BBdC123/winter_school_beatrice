
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Data --------------------------------------------------------------------

# SST time series
sst_NOAA <- read_csv("course_material/data/sst_NOAA.csv")

# Global SST layer for 2022-01-01
load("course_material/data/OISST_2022.RData")


# Example -----------------------------------------------------------------

# All five functions at once
sst_NOAA %>% 
  arrange(site, temp) %>% 
  select(t, temp) %>% 
  filter(temp >= 23) %>% 
  mutate(year = year(t)) %>% 
  summarise(mean_year = mean(year))


# Exercise 1 --------------------------------------------------------------

# Filter sst_NOAA to have only data for WA from 2005-2010
# Plot as a line plot
# Combine or inset with a map of Western Australia


sst_WA <- filter(sst_NOAA, site == "WA", year(t)>=2005, year(t)<=2010 )
head(sst_WA)

plot_WA <- ggplot()+
  geom_line(data=sst_WA, aes(x=t, y=temp, color=temp))
plot_WA 

# map
map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon)) %>%
  filter(region == "Australia")

map_australia <- ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "gray", colour = "black")+
  coord_quickmap(xlim=c(110,130), ylim=c(-36, -14))
map_australia


# inset plot
map_inset <- map_australia + 
  annotation_custom(grob = ggplotGrob(plot_WA),
                    xmin = 110, xmax = 120,
                    ymin = -15, ymax = -20)

map_inset

ggsave(plot =map_inset, filename = "figures/map_inset_WA.pdf", 
       height = 6, width = 8)

# ggraw

# Exercise 2 --------------------------------------------------------------

# Create an informative table of the 10 highest monthly temperature in the Med
# Inset this onto a map of the Med with the sea surface temperature from 'OISST_2022'


sst_Med <-  filter(sst_NOAA, site == "Med" ) %>% 
  group_by(month(t), year(t)) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE))
sst_Med 


sst_Med %>% arrange(desc(mean_temp))



# Exercise 3 --------------------------------------------------------------

# Plot the the annual mean temperatures of NW_Atl as a bar plot
# Inset a map of Atlantic Canada into the corner of the bar plot


# BONUS -------------------------------------------------------------------

# Find the mean temperature for 2002 in Med and 2005 in WA in the same code chunk
# Hint: The case_when() function will be useful for this
# In another single code chunk, extract the country shapes for Italy and Australia
# Inset plots of the Med temperatures over Italy, and WA over Australia
# Combine into one figure

