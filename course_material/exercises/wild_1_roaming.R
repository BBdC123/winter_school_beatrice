
# Libraries ---------------------------------------------------------------

library(rgbif)

# Data --------------------------------------------------------------------


# Example -----------------------------------------------------------------

# Download occurrence data
Emax <- occ_search(scientificName = "Ecklonia maxima")

# create bar plot
Emax$data %>% 
  group_by(county) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = county, y = count)) +
  geom_bar(aes(fill = county), stat = "identity", show.legend = F)

# plot occurence points on map
Emax$data %>% 
  ggplot() +
  borders() +
  geom_point(aes(x = as.numeric(Emax$data$decimalLongitude), 
                 y = as.numeric(Emax$data$decimalLatitude),
                 colour = country)) +
  coord_quickmap(xlim = c(-1, 29), ylim = c(-36, 1))

# Exercise 1 --------------------------------------------------------------

# Download GBIF species presence data and a physical data layer

# Download occurrence data
Caulerpa <- occ_search(scientificName = "Caulerpa lentillifera")

# Plot them together on the same map

Caulerpa_map <- Caulerpa$data %>% 
  ggplot() +
  borders() +
  geom_point(aes(x = as.numeric(Caulerpa$data$decimalLongitude), 
                 y = as.numeric(Caulerpa$data$decimalLatitude),
                 colour = country)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=7))+
  coord_quickmap()


## Fixed map

map_global_fix <- map_data('world') %>% 
  rename(lon = long) %>% 
  mutate(group = ifelse(lon > 180, group+2000, group),
         lon = ifelse(lon > 180, lon-360, lon)) 

# plot map
global_map <- ggplot(data = map_global_fix, aes(x = lon, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "gray", colour = "black")+
  coord_quickmap()

global_map


# add Caulerpa data

Caulerpa_map <- ggplot() +
  geom_polygon(data = map_global_fix, aes(x = lon, y = lat, group = group), 
               fill = "gray", colour = "black")+
  coord_quickmap()+
  geom_point(aes(x = as.numeric(Caulerpa$data$decimalLongitude),
                 y = as.numeric(Caulerpa$data$decimalLatitude)), color = "navy")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=7))

Caulerpa_map



# Exercise 2 --------------------------------------------------------------

# Download a subset from a different dataset on the NOAA ERDDAP server
# Create a mean time series over the spatial extent
  # e.g. group_by() date and create daily means, thereby removing lon/lat 
# Plot the time series with a linear model, showing the slope, R^2, and p values


# Exercise 3 --------------------------------------------------------------

# Download OBIS data and combine these two plots
  # Scatterplot: data collection over time
  # Map: occurrence records
# See: https://ropensci.org/blog/2017/01/25/obis/


# BONUS -------------------------------------------------------------------

# Find the thermal envelope of a species 
  # Hint: Match species occurrence values to temperature time series pixels
  # Then find the 25th - 75th percentile of temperatures in those time series
# Then calculate the decadal rate of change for temperature in each pixel
  # Hint, get the annual mean values per pixel and calculate a linear model
  # Multiply the slope of that line by 10 to get the decadal trend
# For how many pixels where this species occurs may be too warm by 2100?


# MEGA BONUS --------------------------------------------------------------

# Subsetting and working with Copernicus Marine Environment Monitoring Service (CMEMS) data
# See: https://github.com/markpayneatwork/RCMEMS
# As with the NOAA data we access via the ERDDAP system,
# we can also access CMEMS data via the 'motuclient' system
# Unfortunately this is only supported with python
# Following the instructions in the link above,
# install the necessary software and download a subset of a dataset of your choice
# Plot it as a data layer on an Arctic projection

