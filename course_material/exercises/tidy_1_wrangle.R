
# Libraries ---------------------------------------------------------------

library(tidyverse)


# Data --------------------------------------------------------------------

# The mangled data
load("course_material/data/OISST_mangled.RData")

# The tidy data
sst_NOAA <- read_csv("course_material/data/sst_NOAA.csv")


# Example -----------------------------------------------------------------

# The first few rows of an untidy dataframe
head(OISST3)

# Pivot wide by date
  # NB: This is very untidy
OISST1_wide <- OISST1 %>% 
  pivot_wider(values_from = temp, names_from = t)

OISST1_wide

# Exercise 1 --------------------------------------------------------------

# Combine OISST4a and OISST4b into a new object

head(OISST4a)
OISST4a_tidy <- OISST4a %>% 
  separate(col = index, into = c("site", "t"), sep = " ")
head(OISST4a_tidy)

head(OISST4b)
OISST4b_tidy <- OISST4b %>% 
  unite(year, month, day, col = "t", sep = "-")
head(OISST4b_tidy)

OISST4_combined <- left_join(OISST4a_tidy,OISST4b_tidy)
head(OISST4_combined)

# Exercise 2 --------------------------------------------------------------

# Ensure that the date formatting is correct on your new object




# Exercise 3 --------------------------------------------------------------

# Split the date column on `sst_NOAA` and re-unite them

head(sst_NOAA)
sst_NOAA_split <- sst_NOAA %>% 
  separate(col = t, into = c("year","month", "day"), sep = "-")
head(sst_NOAA_split)

sst_NOAA_united <- sst_NOAA_split %>% 
  unite(year, month, day, col = "t", sep = "-" )
head(sst_NOAA_united )


# BONUS -------------------------------------------------------------------

# Plot the temperatures of two time series against each other as a scatterplot
# Meaning temperature from time series 1 are the X axis, and time series 2 on the Y axis
# Hint: This requires pivoting the temperatures wide into columns

#filter Med data for 1982
sst_Med_new <- filter(sst_NOAA, site == "Med", year(t) ==1982) 
 

sst_Med <- filter(sst_NOAA_split, year==1982 & site=="Med")
# unite date columns
sst_Med_tidy <- sst_Med %>% 
  unite(year, month, day, col = "t", sep = "-" )
head(sst_Med_tidy)


sst_Med2 <- filter(sst_NOAA_split, year==2000 & site=="Med")
# unite date volumns
sst_Med2_tidy <- sst_Med %>% 
  unite(year, month, day, col = "t", sep = "-" )
head(sst_Med2_tidy)


# add monthly variable
sst_Med_monthly<- sst_Med_tidy %>% 
  mutate(month = month(t, label = T)) %>% 
  group_by(month) %>% 
  summarise(temp = round(mean(temp, na.rm = T), 3))

sst_Med2_monthly<- sst_Med2_tidy %>% 
  mutate(month = month(t, label = T)) %>% 
  group_by(month) %>% 
  summarise(temp = round(mean(temp, na.rm = T), 3))


# long format into wide format for sst_Med2

sst_Med2_monthly_wide <- sst_Med2_monthly %>% 
  pivot_wider(id_cols = c(month,temp), names_from = month, values_from = temp)
head(sst_Med2_monthly_wide)



# plotting 


ggplot(data = sst_NOAA, aes(x = site, y = temp)) +
  geom_line(aes(colour = site)) +
  labs(x = NULL, y = "Temperature (°C)", colour = "Site") +
  theme_bw()





