library(here)
library(tidyverse)

### CALCULATE PERCENT COVER ###
# Data source:
# https://irma.nps.gov/DataStore/Reference/Profile/2305079

# Load plants lookup table, just in case
tlu_plants <- read_csv(here('data/NCPN_IntegratedUplands_PlantsLookup.csv')) %>%
  select(c(Master_Plant_Code, Master_Common_Name))

# Load NCPN point-intercept data and filter for Arches National Park
arches_veg <- read_csv(
  here('data/NCPN_IntegratedUplands_PointIntercept.csv')
  ) %>%
  filter(Unit_Code == 'ARCH')

# Keep only live vegetation records
# Remove duplicate species at one point before counting.
# Calculate cover percentage and assign cover class for later use.
arches_cover <- arches_veg %>%
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
# meaningful but efficient way to show vegetation cover changes over time or
# identify particular plots to focus on

## SPECIES WITH THE MOST CHANGE ##

# Calculate percent change for each species using min and max values, regardless
# of year. Keep species with >= 10% change.
species_change <- arches_cover %>%
  group_by(Plot_ID, Species, Scientific_Name) %>%
  summarize(
    Max_Cover = max(Cover_pct, na.rm = TRUE),
    Min_Cover = min(Cover_pct, na.rm = TRUE),
    Cover_Change = Max_Cover - Min_Cover,
    .groups = 'drop'
  ) %>%
  filter(Cover_Change >= 10) %>%
  arrange(desc(Cover_Change))

# Save as CSV to use in dashboard
write.csv(species_change, 
          here('data/species_change.csv'), 
          row.names = FALSE)

# Calculate percent change for each species from first to last survey year. Keep
# species with >= 10% absolute change.
species_trend <- arches_cover %>%
  mutate(Visit_Year = year(Start_Date)) %>%
  group_by(Plot_ID, Species, Scientific_Name) %>%
  summarize(
    First_Yr_Cover = Cover_pct[Visit_Year == min(Visit_Year)],
    Last_Yr_Cover = Cover_pct[Visit_Year == max(Visit_Year)],
    Cover_Change = Last_Yr_Cover - First_Yr_Cover,
    .groups = 'drop'
  ) %>%
  mutate(
    Trend = case_when(
      Cover_Change > 0 ~ 'Increase',
      Cover_Change < 0 ~ 'Decrease',
      Cover_Change == 0 ~ 'None'
      ),
    Cover_Change = abs(Cover_Change)
  ) %>%
  filter(Cover_Change >= 10) %>%
  mutate(Cover_Change = if_else(Trend == 'Decrease', 
                                Cover_Change * -1, 
                                Cover_Change)
  ) %>%
  arrange(Cover_Change)

# Write to CSV to use in dashboard
write.csv(species_trend, 
          here('data/species_trend.csv'), 
          row.names = FALSE)

## EXPLORATION CONCLUSION ##
# The top three species with change are BRTE, PLPA2, and SATR12. The plots that
# that appear most frequently between both exploration tables were 
# 26, 91, 92, and 101. My analysis focuses on these species and plots.

### ANALYSIS & FINAL PRODUCTS ###

species <- c('BRTE', 'PLPA2', 'SATR12')
plots <- c(26, 91, 92, 101)

# Create and save graphs of species change for selected plots
lapply(plots, function(plot_id) {
  
  plot_data <- arches_cover %>%
    filter(Plot_ID == plot_id) %>%
    mutate(yr_date = ymd(Visit_Year, truncated = 2L))
  
  if (nrow(plot_data) == 0) return(NULL)
  
  ggplot(plot_data, aes(x = yr_date, y = Cover_pct, fill = Species)) +
    geom_col(position = "stack") +
    scale_x_date(
      date_breaks = '1 year', date_labels = '%Y'
    ) +
    scale_fill_viridis_d(option = 'turbo') +
    labs(title = paste0('Vegetation Cover for Plot ', plot_id),
                  x = "Year",
                  y = "Percent Cover",
                  fill = "Species") +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank())
  
  ggsave(paste0('plots/Plot_', plot_id, '.png'))
})

# Note: After looking at the plots, I chose not to include Plot 26 in the
# dashboard.

## PARK-LEVEL CHANGE OF SELECT SPECIES & TOP FIVE SPECIES ##

# Calculate area covered by each species (each plot is 250 sq. meters)
species_area <- arches_cover %>%
  mutate(Cover_Area_m2 = Cover_pct / 100 * 250)

# Calculate area sampled each year
tot_plot_area <- species_area %>%
  distinct(Visit_Year, Plot_ID) %>%
  group_by(Visit_Year) %>%
  summarize(
    Area_Sampled_m2 = n() * 250,
    .groups = 'drop'
  )

# Calculate total cover area for each species
species_area_year <- species_area %>%
  group_by(Visit_Year, Species) %>%
  summarize(
    Species_Area_m2 = sum(Cover_Area_m2),
    .groups = 'drop'
  ) %>%
  left_join(tot_plot_area, by = join_by(Visit_Year)) %>%
  mutate(
    Total_Cover_pct = Species_Area_m2 / Area_Sampled_m2 * 100
  )

# Select species cover area per year
species_area_select <- species_area_year %>%
  filter(Species %in% species)

# Save CSV for use in dashboard
write.csv(species_area_select, 
          here('data/species_area_select.csv'), 
          row.names = FALSE)

# Create graph and save
ggplot(data = species_area_select %>% mutate(yr_date = ymd(Visit_Year, truncated = 2L)), 
  aes(x = yr_date, y = Total_Cover_pct, fill = Species)
) +
  geom_col(position = "stack") +
  scale_x_date(
    date_breaks = '1 year', date_labels = '%Y'
  ) +
  scale_fill_viridis_d(option = 'turbo') +
  labs(title = paste0('Arches Veg Cover Area - BRTE, PLPA2, SATR12'),
       x = "Year",
       y = "Percent Cover",
       fill = "Species") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45))

ggsave(here('plots/species_area_select.png'))

# Top five species cover area per year
species_area_top <- species_area_year %>%
  group_by(Visit_Year) %>%
  arrange(desc(Total_Cover_pct)) %>%
  slice_head(n = 5) %>%
  ungroup()

# Save CSV for use in dashboard
write.csv(species_area_top, 
          here('data/species_area_top.csv'), 
          row.names = FALSE)

# Create graph and save
ggplot(data = species_area_top %>% mutate(yr_date = ymd(Visit_Year, truncated = 2L)), 
       aes(x = yr_date, y = Total_Cover_pct, fill = Species)
) +
  geom_col(position = "stack") +
  scale_x_date(
    date_breaks = '1 year', date_labels = '%Y'
  ) +
  scale_fill_viridis_d(option = 'turbo') +
  labs(title = paste0('Arches Veg Cover Area - Top 5 Species Each Year'),
       x = "Year",
       y = "Percent Cover",
       fill = "Species") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45))

ggsave(here('plots/species_area_top.png'))



### FAILED ATTEMPT TO ADD COORDINATES ###
# The SEUG dataset includes upland veg data from ARCH, so I attempted to join 
# the location ID from the SEUG data to the NCPN data, so I could get plot 
# coordinates, but it didn't work

# Get SEUG taxa list
seug_taxa <- read_csv(here('data/taxaList.csv')) %>%
  select(taxaCode:scientificName_ITIS)

# Get SEUG data and filter out Arches data for years matching NCPN data
seug_arches <- read_csv(here('data/SEUG_vegData.csv')) %>%
  filter(str_starts(locationID, 'A'), year(eventDate) >= 2010) %>%
  select(c(locationID, eventDate, taxaCode, coverValue))

# Add SEUG taxa code to Arches data
arches_seug_taxa <- arches_cover %>%
  left_join(seug_taxa, by = join_by(Scientific_Name == scientificName_ITIS)) %>%
  left_join(seug_taxa, by = join_by(Scientific_Name == scientificName_SEUG)) %>%
  mutate(Taxa_Code = if_else(is.na(taxaCode.x), taxaCode.y, taxaCode.x)) %>%
  select(-c(taxaCode.x:scientificName_ITIS))

# Join SEUG data to Arches data
arches_locID <- arches_seug_taxa %>%
  left_join(seug_arches, by = join_by(Start_Date == eventDate, 
                                      Taxa_Code == taxaCode,
                                      Cover_Class == coverValue)) %>%
  mutate(Visit_Year = year(Start_Date)) %>%
  distinct(Visit_Year, Plot_ID, locationID)
            