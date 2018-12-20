# Date: August 8, 2018
# Project: National Home Visiting Resource Center
# Author: Charmaine Runes
# Task: Merge LIA ZIP data to countyfips crosswalk from MABLE/GeoCORR

library(ggplot2)
library(urbnthemes)
library(tidyverse)
library(urbnmapr)

set_urban_defaults(style = "map")

territories_counties <- get_urbn_map("territories_counties")

# Read in CSV file for LIAs - might need to change file path!
LIA_zip <- read_csv("H:/HVShare/2018 Yearbook/Mapping/LIA_zip_updated.csv",
                    col_types = cols(
                      Program = col_character(),
                      City = col_character(),
                      State = col_character(),
                      ZIP = col_character()
                    ))

# Make sure ZIP is in the right format
LIA_zip$ZIP <- str_pad(LIA_zip$ZIP, width=5, side="left", pad="0") 

# Read in CSV file for Zip-to-COUNTYFIPS crosswalk (courtesy of MABLE/GeoCORR) - might need to change file path!
crosswalk <- read_csv("H:/HVShare/2018 Yearbook/Mapping/ZIPtoCountyFIPS.csv",
                      col_types = cols(
                        ZIP = col_character(),
                        COUNTYFIPS = col_character(),
                        CountyName = col_character(),
                        ZIPname = col_character(),
                        Allocation = col_double()
                      ))

# Make sure county_fips is in the right format
crosswalk$county_fips <- str_pad(crosswalk$COUNTYFIPS, width=5, side="left", pad="0") 

# Merge files by ZIP
LIA_county <- merge(crosswalk, LIA_zip,by = "ZIP")

LIA_county %>% group_by(Program) %>% summarise(n())

# Merge LIA_county data with all counties data, just for HFA LIAs, as a check  
# HFA <- LIA_county %>% 
#   filter(Program == "HFA") %>% 
#   left_join(territories_counties, by = "county_fips")

# Check if there were any mismatched joins
# LIA_county %>% 
#   filter(Program == "HFA") %>% 
#   anti_join(territories_counties, by = "county_fips")

# County-level map!
allprograms <- LIA_county %>% 
  left_join(territories_counties, by = "county_fips")

# Aaron's code: Looks for counties that appear on the map but are not part of the LIA data
test_counties <- territories_counties %>%
  select(county_fips, county_name, state_abbv) %>%
  group_by(county_fips, county_name, state_abbv) %>%
  summarize(n())

mismatches <- anti_join(test_counties, LIA_county, by = "county_fips") %>% 
  left_join(territories_counties, by = "county_fips")

# Checks (if you need them)
# summary <- allprograms %>% group_by(State) %>% summarise(n())
# summary_county <- allprograms %>% group_by(CountyName) %>% summarise(n())

# Add all 120 of Kentucky's counties - this might need to be a manual addition each year
HANDS <- counties %>% 
  filter(state_abbv == "KY") 

# Check that MN counties are correct - this will need to be a manual check each year (boo)
# MNcheck <- allprograms %>% 
#  select(county_name, state_abbv) %>% 
#  filter(state_abbv == "MN") 

# temp <- MNcheck %>% 
#  group_by(county_name) %>% 
#  summarize(n())

minnesota_counties <- c("Big Stone County", "Douglas County", "Grant County", 
                        "Pipestone County", "Pope County", "Traverse County", 
                        "Stevens County", "Yellow Medicine County", "Lac Qui Parle County",
                        "Chippewa County", "Lincoln County", "Lyon County",
                        "Murray County","Rock County", "Swift County")

MN_add <- territories_counties %>%
  filter(state_abbv == "MN" & county_name %in% minnesota_counties)

ggplot(data = territories_counties, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "#ffffff", fill = "#00aac3", size = 0.1) +
  geom_polygon(data = mismatches,
               mapping = aes(x = long, y = lat, group = group),
               color = "#ffffff",
               fill = "gray",
               size = 0.1) +
  geom_polygon(data = HANDS,
               mapping = aes(x = long, y = lat, group = group),
               color = "#ffffff",
               fill = "#00aac3",
               size = 0.1) +
  geom_polygon(data = MNadd,
               mapping = aes(x = long, y = lat, group = group),
               color = "#ffffff",
               fill = "#00aac3",
               size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Counties with at least one local implementing agency",
       subtitle = "Using evidence-based home visiting models") 

ggsave("H:/HVShare/2018 Yearbook/Mapping/LIAsbyCounty_MNadd_2018.png")
ggsave("H:/HVShare/2018 Yearbook/Mapping/LIAsbyCounty_MNadd_2018.pdf")

# Calculations: county coverage
anticoverage <- mismatches %>% group_by(state_name) %>% summarise(n_distinct(county_fips))
coverage <- territories_counties %>% group_by(state_name) %>%  summarise(n_distinct(county_fips))

MS_pull <- mismatches %>% 
  filter(state_abbv.x == "MS") %>% 
  group_by(county_name.x) %>% 
  summarise(n_distinct(county_fips))
