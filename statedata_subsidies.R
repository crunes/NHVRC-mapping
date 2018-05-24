# Date: May 24, 2018
# Author: Charmaine Runes
# Project: National and State Data on Average Monthly Percentage of Children Subsidized by CCDBG Served in Child Care Centers 1998-2015
# Submitted to: Gina Adams, Julia Henly

# Install packages
# install.packages("tidyverse")
library(tidyverse)

install.packages("devtools")
library(devtools)
devtools::install()
library(urbnthemes)
set_urban_defaults(style = "map")

devtools::install_github("UrbanInstitute/urbnmapr")

library(urbnmapr)
states %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "grey", color = "#ffffff", size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# Read in CSV file
statedata_subsidies <- read_csv("H:/Miscellaneous/AECF/statedata_subsidies.csv")
View(statedata_subsidies)


statedata_subsidies <- statedata_subsidies %>%
  mutate_all(funs(stringr::str_replace(., "%", ""))) %>%
  mutate_at(vars("y1998","y2006","y2014","y2015","change98-14"), as.numeric)


statedata_subsidies %>%
  left_join(states, by = "state_name") %>%
  ggplot(mapping = aes(long, lat, group = group, fill = y2015 / 100)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradientn(labels = scales::percent) +
  labs(fill = "Percent of children subsidzied\nby CCDBG in child care centers")

ggsave("H:/Miscellaneous/AECF/statedata_subsidies_2015.png", width = 12, height = 9)
