library(here)
library(tidyverse)

### ANALYSIS ###
# https://irma.nps.gov/DataStore/Reference/Profile/2305079
# Load plants lookup table
tlu_plants <- read_csv(here('NCPN_IntegratedUplands_PlantsLookup.csv')) %>%
  select(c(Master_Plant_Code, Master_Common_Name))

# Load NCPN point-intercept data and filter for Arches National Park
arches_veg <- read_csv(
  here('data/NCPN_IntegratedUplands_PointIntercept.csv')
  ) %>%
  filter(Unit_Code == 'ARCH')

# Save CSV so the data can be uploaded to GitHub
write.csv(arches_veg, 
          file = here('data/NCPN_IntegratedUplands_PointIntercept_ARCH.csv'),
          row.names = FALSE)

# Remove duplicate species at one point before counting. There are 100 points 
# per transect, so the counts equals the percent cover.
arches_counts <- arches_veg %>%
  filter(!is.na(Scientific_Name), Status == 1) %>%
  select(Start_Date:Layer, Species, Scientific_Name) %>%
  distinct(Start_Date, Plot_ID, Transect, Point, Species, Scientific_Name) %>%
  group_by(Start_Date, Plot_ID, Species, Scientific_Name) %>%
  summarize(
    Count = n(),
    Cover_pct = round(Count / 300 * 100, 0),
    .groups = "drop"
  ) %>%
  mutate(
    Cover_Class = cut(
      Cover_pct,
      breaks = c(0, 5, 25, 50, 75, 95, 100),
      labels = c('1', '2', '3', '4', '5', '6'),
      include.lowest = TRUE,
      right = TRUE
    ),
    Cover_Class = as.numeric(Cover_Class)
  )

# Calculate percent change for each species
plots_veg_cover <- arches_counts %>%
  group_by(Plot_ID, Species) %>%
  summarize(
    Max_cover = max(Cover_pct, na.rm = TRUE),
    Min_cover = min(Cover_pct, na.rm = TRUE),
    Change_cover = Max_cover - Min_cover,
    .groups = 'drop'
  )

# Calculate total change for each plot and keep the top five
plots_change <- plots_veg_cover %>%
  group_by(Plot_ID) %>%
  summarize(
    Change_total = sum(Change_cover, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Change_total)) %>%
  slice_head(n = 5)

# Select the data for the top five plots with change
arches_high_change <- arches_counts %>%
  filter(Plot_ID %in% plots_change$Plot_ID) %>%
  arrange(Plot_ID, Start_Date)

# Calculate species trends
plot_trend <- arches_counts %>%
  mutate(Visit_Year = year(Start_Date)) %>%
  group_by(Plot_ID, Species, Scientific_Name) %>%
  summarize(
    First_yr_cover_pct = Cover_pct[Visit_Year == min(Visit_Year)],
    Last_yr_cover_pct = Cover_pct[Visit_Year == max(Visit_Year)],
    Cover_change = Last_yr_cover_pct - First_yr_cover_pct,
    .groups = 'drop'
  ) %>%
  left_join(tlu_plants, by = join_by(Species == Master_Plant_Code))


### 
seug_taxa <- read_csv(here('data/taxaList.csv')) %>%
  select(taxaCode:scientificName_ITIS)

seug_arches <- read_csv(here('data/SEUG_vegData.csv')) %>%
  filter(str_starts(locationID, 'A'), year(eventDate) >= 2010) %>%
  select(c(locationID, eventDate, taxaCode, coverValue))

arches_seug_taxa <- arches_counts %>%
  left_join(seug_taxa, by = join_by(Scientific_Name == scientificName_ITIS)) %>%
  left_join(seug_taxa, by = join_by(Scientific_Name == scientificName_SEUG)) %>%
  mutate(Taxa_Code = if_else(is.na(taxaCode.x), taxaCode.y, taxaCode.x)) %>%
  select(-c(taxaCode.x:scientificName_ITIS))

arches_locID <- arches_seug_taxa %>%
  left_join(seug_arches, by = join_by(Start_Date == eventDate, 
                                      Taxa_Code == taxaCode,
                                      Cover_Class == coverValue)) %>%
  mutate(Visit_Year = year(Start_Date)) %>%
  distinct(Visit_Year, Plot_ID, locationID)
            