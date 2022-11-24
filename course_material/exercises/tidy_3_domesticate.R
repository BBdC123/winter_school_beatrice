
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Data --------------------------------------------------------------------

# SST data
sst_NOAA <- read_csv("course_material/data/sst_NOAA.csv")


# Example -----------------------------------------------------------------

# Whatever we can imagine!
sst_NOAA %>%  
  group_by(site) %>%
  summarise(count = n(), 
            count_15 = sum(temp > 20)) %>% 
  mutate(prop_15 = count_15/count) %>% 
  arrange(prop_15)


# Exercise 1 --------------------------------------------------------------

# Filter two sites and summarize six different statistics

#filter two sites
sst_NOAA_2 <- sst_NOAA %>% 
  filter(site=="Med" | site == "NW_Atl")


# mean temperature for the two sites with sd and min and max temperatures
sst_NOAA_3 <-  sst_NOAA_2 %>%
  group_by(site) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE)) %>% ungroup ()
sst_NOAA_3


# The proportion of recordings above 15°C per site
sst_NOAA_4 <- sst_NOAA_2 %>%  
  group_by(site) %>%
  summarise(count = n(), 
            count_15 = sum(temp > 15)) %>% 
  mutate(prop_15 = count_15/count) %>% 
  arrange(prop_15) %>% ungroup()

sst_NOAA_4


# Exercise 2 --------------------------------------------------------------

# Find the maximum temperature and SD per year per site
# Plot this as a bar plot with error bars
# Inset a map of each site over each bar plot

# sst max temp and mean with sd
sst_1<- sst_NOAA %>%
  mutate(year = year(t)) %>% 
  group_by(site, year) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))

sst_1



# per site 
sst_2<- sst_NOAA %>%
  mutate(year = year(t)) %>% 
  group_by(site) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))

sst_2


plot_2 <- ggplot () +
  geom_bar(data=sst_2, aes(x = site, y = mean_temp, fill = site),stat="identity")+
  geom_errorbar(data=sst_2,aes(x = site, ymin=mean_temp-sd_temp, ymax=mean_temp+sd_temp), width=.2)+
  theme_classic()+
  theme(axis.text=element_text(size=15, color="black"),
        axis.title=element_text(size=15, color="black"),
        legend.title=element_text(size=15, color="white"),
        legend.text=element_text(size=15, color="black"),
        legend.position="none") +
  labs(x=NULL, y = "Temperature (°C)")+
  geom_point(data=sst_2, aes(x = site, y = max_temp, color = site))
plot_2


# make maps

map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon)) 

# Map for Med
map_Med <- ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "gray", colour = "black")+
  coord_quickmap(xlim=c(-10,40), ylim=c(25,50))
map_Med


# Map for NW Atl
map_NW_Atl <- ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "gray", colour = "black")+
  coord_quickmap(xlim=c(-100,-50), ylim=c(25,50))
map_NW_Atl


# Map for WA
map_WA <- ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "gray", colour = "black")+
  coord_quickmap(xlim=c(110,130), ylim=c(-36, -10))
map_WA


# combine plots

map1 <- map_Med
map2 <- map_NW_Atl
map3 <- map_WA

combined_plot <- ggdraw()+
  draw_plot(plot_2, height = 0.7)+
  draw_plot(map1, x = -0.03, y= 0.7, width=0.5, height=0.3)+
  draw_plot(map2, x = 0.25, y= 0.7, width=0.5, height=0.3)+
  draw_plot(map3, x = 0.55, y= 0.7, width=0.5, height=0.3)
  
combined_plot

ggsave(plot =combined_plot, filename = "figures/barplot_maps.pdf", 
       height = 6, width = 8)

# Exercise 3 --------------------------------------------------------------

# From scratch, re-write the full analysis for exercise 1 'The new age'
# Inset maps for each at the end of each line on the Y axis




# BONUS -------------------------------------------------------------------

# Create a faceted heatmap showing the monthly climatologies per site

# Calculate anomalies for each site 
sst_NOAA_anom <- sst_NOAA %>%
  mutate(month = month(t, label = T))%>%
  group_by(site, month) %>% 
  mutate(anom = temp - mean(temp, na.rm = T)) %>%
  ungroup()
head(sst_NOAA_anom)

# creating heatmap

sst_heatmap <- ggplot()+
  geom_raster(data = sst_NOAA_anom, aes(x=month, y=site, fill=anom))+
  scale_fill_distiller(palette = "Spectral")

sst_heatmap
