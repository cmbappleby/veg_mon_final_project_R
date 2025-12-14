library(here)
library(tidyverse)

# https://irma.nps.gov/DataStore/Reference/Profile/2305079
# Load NCPN point-intercept data and filter for Arches National Park
arches_veg <- read_csv(here('NCPN_IntegratedUplands_PointIntercept.csv')) %>%
  filter(Unit_Code == 'ARCH')

# Remove duplicate species at one point before counting. There are 100 points 
# per transect, so the counts equals the percent cover.
arches_counts <- arches_veg %>%
  filter(!is.na(Scientific_Name) | Species %in% c("U", "S"),
         Status == 1 | is.na(Status)) %>%
  select(Start_Date:Layer, Species, Scientific_Name) %>%
  distinct(Start_Date, Plot_ID, Transect, Point, Species, Scientific_Name) %>%
  group_by(Start_Date, Plot_ID, Species, Scientific_Name) %>%
  dplyr::summarize(
    Count = n(),
    Cover_pct = round(Count / 300 * 100, 0),
    .groups = "drop"
  )

plots_years <- arches_veg %>%
  select(c(Visit_Year, Plot_ID)) %>%
  distinct(Visit_Year, Plot_ID) %>%
  arrange(Plot_ID, Visit_Year)

arches_class <- arches_veg %>%
  filter(Unit_Code == 'ARCH', 
         !is.na(Scientific_Name) | Species %in% c("U", "S"),
         Status == 1 | is.na(Status)) %>%
  select(Start_Date:Layer, Species, Scientific_Name) %>%
  distinct(Start_Date, 
           Plot_ID, 
           Transect, 
           Point, 
           Species, 
           Scientific_Name) %>%
  group_by(Start_Date, Plot_ID, Transect, Species, Scientific_Name) %>%
  dplyr::summarize(
    Count = n(),
    Cover_pct = round(Count / 300 * 100, 0),
    .groups = "drop"
  ) %>%
  mutate(
    Cover_Class = cut(
      Cover_pct,
      breaks = c(0, 5, 25, 50, 75, 95, 100),
      labels = c(1, 2, 3, 4, 5, 6),
      include.lowest = TRUE,
      right = TRUE
    )
  )


  