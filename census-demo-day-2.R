# Load Required Packages ###########################################################################
install.packages('crsuggest')
install.packages('viridis')
require(tidyverse)
require(sf)
require(mapview)
require(tidycensus)
require(tigris)
options(tigris_use_cache = TRUE)
require(clipr)
require(crsuggest)
require(viridis)

# Load Analysis Areas ##############################################################################

analysis_areas = st_read('bistate_corridor_segments.gpkg') |> # Each of the sub-areas
  st_transform('ESRI:102698') # Transform to state plane MO West Feet, NAD 1983

corridor = st_read('bistate_corridor.geojson') |>
  st_transform('ESRI:102698') 

mo_cities = places(state = 'MO')
ks_cities = places(state = 'KS')

mo_study_cities = mo_cities |>
  filter(NAME %in% c('Kansas City', 'Independence', 'Sugar Creek')) |>
  st_transform('ESRI:102698')

ks_study_cities = mo_cities |>
  filter(NAME %in% c('Kansas City')) |>
  st_transform('ESRI:102698')

kcmsa = core_based_statistical_areas() |>
  filter(NAME == 'Kansas City, MO-KS') |>
  st_transform('ESRI:102698')

# Please refer to section 7.3 of the TidyCensus textbook for this analysis https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html#small-area-time-series-analysis

acs_vars <- load_variables(2022, 'acs5')


# Acquire Data
vehicles_tract = get_acs(
  geography = 'tract',
  cache = TRUE,
  state = c('MO', 'KS'),
  table = 'B08201',
  output = 'wide',
  year = 2022,
  geometry = TRUE) |>
  st_transform('ESRI:102698') |>
  st_filter(kcmsa |> st_buffer(-500), .predicate = st_intersects) |> # The negative buffer of 500' helps us grab all the tracts that fall completely within the KC metro area, and non that overlap.
  mutate(zero_veh_hh_pct = 100*(B08201_002E / B08201_001E)) |>
  select(GEOID, NAME, zero_veh_hh = B08201_002E, zero_veh_hh_pct, geometry)

mapview(list(vehicles_tract,kcmsa))



ggplot(data = vehicles_tract, aes(fill = zero_veh_hh_pct)) + 
  geom_sf() + 
  scale_fill_viridis(option = 'mako', direction = -1) +
  geom_sf(data = corridor, fill = NA, color = "black", linewidth = 1) + 
  theme_void()


mapview(vehicles_tract, zcol = 'zero_veh_hh_pct')



# Interpolate Data for Study Area and Sub Area
 
blocks_jacomo = blocks(state = 'MO', county = '095') |>
  st_transform('ESRI:102698')
  
blocks_wycoks = blocks(state = 'KS', county = '209') |>
  st_transform('ESRI:102698')

blocks_ja_wy = bind_rows(blocks_jacomo, blocks_wycoks)
