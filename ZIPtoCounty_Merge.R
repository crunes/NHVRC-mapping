# Date: June 26, 2018
# Project: National Home Visiting Resource Center
# Author: Charmaine Runes
# Task: Merge LIA ZIP data to countyfips crosswalk from MABLE/GeoCORR

library(ggplot2)
library(urbnthemes)
set_urban_defaults()

library(tidyverse)
library(urbnmapr)

# Read in CSV file for LIAs
LIA_zip <- read_csv("H:/HVShare/2018 Yearbook/Mapping/LIA_zip.csv")

# Read in CSV file for Zip-to-COUNTYFIPS crosswalk (courtesy of MABLE/GeoCORR)
crosswalk <- read_csv("H:/HVShare/2018 Yearbook/Mapping/ZIPtoCountyFIPS.csv",
                      col_types = cols(
                        ZIP = col_character(),
                        COUNTYFIPS = col_character(),
                        CountyName = col_character(),
                        ZIPname = col_character(),
                        Allocation = col_double()
                      ))

crosswalk$county_fips <- str_pad(crosswalk$COUNTYFIPS, width=5, side="left", pad="0") 

# Merge files by ZIP
LIA_county <- merge(crosswalk, LIA_zip,by = "ZIP")

LIA_county %>% group_by(Program) %>% summarise(n())

# Merge LIA_county data with all counties data, just for HFA LIAs  
HFA <- LIA_county %>% 
  filter(Program == "HFA") %>% 
  left_join(counties, by = "county_fips")

# Check if there were any mismatched joins
LIA_county %>% 
  filter(Program == "HFA") %>% 
  anti_join(counties, by = "county_fips")

# County-level map!
allprograms <- LIA_county %>% 
  left_join(counties, by = "county_fips")

ggplot(data = counties, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "#ffffff", fill = "gray", size = 0.05) +
  geom_polygon(data = allprograms,
               mapping = aes(x = long, y = lat, group = group),
               color = "#ffffff",
               fill = "#00aac3",
               size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Counties with at least one local implementing agency",
       subtitle = "Using evidence-based home visiting models") +
  theme_urban_map(scale = "discrete")
