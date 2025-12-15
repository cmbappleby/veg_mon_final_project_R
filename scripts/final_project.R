library(here)
library(tidyverse)

### CALCULATE PERCENT COVER ###
# Data source:
# https://irma.nps.gov/DataStore/Reference/Profile/2305079

# Load plants lookup table
tlu_plants <- read_csv(here('NCPN_IntegratedUplands_PlantsLookup.csv')) %>%
  select(c(Master_Plant_Code, Master_Common_Name))

# Load NCPN point-intercept data and filter for Arches National Park
arches_veg <- read_csv(
  here('data/NCPN_IntegratedUplands_PointIntercept.csv')
  ) %>%
  filter(Unit_Code == 'ARCH')

# Save CSV so the raw data can be uploaded to GitHub
write.csv(arches_veg, 
          file = here('data/NCPN_IntegratedUplands_PointIntercept_ARCH.csv'),
          row.names = FALSE)

# Keep only live vegetation records
# Remove duplicate species at one point before counting.
# Calculate cover percentage and assign cover class for later use
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
  ) %>%
  mutate(Visit_Year = year(Start_Date))

### EXPLORE THE DATA ###
# Since there are 84 plots, I wanted to explore the data to find the a 
# meaningful but efficient way to show vegetation cover changes over time

## INITIAL EXPLORATION
# MAXIMUM CHANGE PER PLOT

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
    Num_species = sum(Change_cover > 0, na.rm = TRUE),
    Max_species_change = max(Change_cover, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Change_total)) %>%
  slice_head(n = 5)

# find max species and add it to table above

# Select the data for the top five plots with change
arches_high_change <- arches_counts %>%
  filter(Plot_ID %in% plots_change$Plot_ID) %>%
  arrange(Plot_ID, Start_Date)

# SPECIES-LEVEL TRENDS BY PLOT

# Calculate species trends to see if any other plots stand out
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

# EXPLORE SELECT PLOTS

# After reviewing the species trends, I decided to add plot 92
# to the high change table
arches_change_plots <- arches_high_change %>%
  bind_rows(
    arches_counts %>% filter(Plot_ID == 92)
  )

# Save to CSV to use in dashboard
write.csv(arches_change_plots, here('data/change_plots.csv'), row.names = FALSE)

# Create and save plots of species change over time to visually explore
lapply(unique(arches_change_plots$Plot_ID), function(plot_id) {
  
  plot_data <- arches_change_plots %>%
    filter(Plot_ID == plot_id) %>%
    arrange(Visit_Year)
  
  if (nrow(plot_data) == 0) return(NULL)
  
  ggplot(plot_data, aes(x = Visit_Year, y = Cover_pct, fill = Species)) +
    geom_col(position = "stack") +
    scale_fill_viridis_d(option = 'turbo') +
    labs(title = paste0('Vegetation Cover for Plot ', plot_id),
                  x = "Year",
                  y = "Percent Cover",
                  fill = "Species") +
    theme_minimal()
  
  ggsave(paste0('plots/Plot_', plot_id, '.png'))
})

## FINAL PRODUCT
# After reviewing the selected plots, it was clear that BRTE showed the most 
# change over the sampling years.  Without spatial data or knowledge of 
# management actions, I felt the best way to communicate vegetation cover 
# changes was to show park-level species change instead of plot level.

# Calculate area covered by each species (each plot is 250 sq. meters)
arches_veg_area <- arches_counts %>%
  mutate(Cover_Area_m2 = Cover_pct / 100 * 250)

# Calculate area sampled each year
arches_area_year <- arches_veg_area %>%
  distinct(Visit_Year, Plot_ID) %>%
  group_by(Visit_Year) %>%
  summarize(
    Area_Sampled_m2 = n() * 250,
    .groups = 'drop'
  )

# Calculate total cover area for each species
species_area_year <- arches_veg_area %>%
  group_by(Visit_Year, Species) %>%
  summarize(
    Species_Area_m2 = sum(Cover_Area_m2),
    .groups = 'drop'
  ) %>%
  left_join(arches_area_year, by = join_by(Visit_Year)) %>%
  mutate(
    Total_Cover_pct = Species_Area_m2 / Area_Sampled_m2 * 100
  )

# Only keep the top five species cover area per year because there are 106
# different species
species_area_top <- species_area_year %>%
  group_by(Visit_Year) %>%
  arrange(desc(Total_Cover_pct)) %>%
  slice_head(n = 5) %>%
  ungroup()

# Write to CSV to use in dashboard
write.csv(species_area_top, here('data/species_area.csv'), row.names = FALSE)

# Create and save plot
ggplot(
  species_area_top, 
  aes(x = Visit_Year, y = Total_Cover_pct, fill = Species)
) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(option = 'turbo') +
  labs(title = paste0('Vegetation Cover Area - Top 5 Species Each Year'),
       x = "Year",
       y = "Percent Cover",
       fill = "Species") +
  theme_minimal()

ggsave(here('plots/species_area.png'))


### FAILED ATTEMPT TO ADD COORDINATES ###
# The SEUG dataset includes upland veg data from ARCH, so I attempted to join 
# the location ID from the SEUG data to the NCPN data, so I could get plot 
# coordinates, but it didn't work
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
            