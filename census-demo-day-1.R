# Exploring Census Data with R ################################################


# Load Required Packages #######################################################

require(tidyverse)
require(sf)
require(mapview)
require(tidycensus)
require(tigris)
require(clipr)

# Enter Census API Key #########################################################
# Get a census API Key from http://api.census.gov/data/key_signup.html
# set up census API key for use in your environment (one-time if on same computer/R enviro)

census_api_key("YOUR_CENSUS_API_KEY", install = TRUE) 

# Retrieve installed census API key if needed
Sys.getenv("CENSUS_API_KEY") 


# Load Area(s) of Interest #####################################################

analysis_areas = st_read('bistate_corridor_segments.gpkg')

mapview(metros)

# Load Base Geographies ########################################################

metros = tigris::core_based_statistical_areas()

kcmsa = metros |>
  filter(NAME == 'Kansas City, MO-KS')


# Load Census Variables ########################################################

acs_vars <- load_variables(2022, 'acs5')

# Exploring the variables, we decide that we want to understand the number of vehicles available by household in the study area. This information is contained in table "B08201"

vehicles_msa <- get_acs(
  geography = 'cbsa',
  table = 'B08201'
  )

# This returns each estimate and margin of error for each Metropolitan/Micropolitan error for every variable in the Vehicles Available by Household Size. This could be more helpful if we had the variable labels added in. Let's add them in with a join

vehicles_msa <- get_acs(
  geography = 'cbsa',
  table = 'B08201') |>
  left_join(acs_vars |> select(name, label), by = c('variable' = 'name'))

# Let's take a look at Kansas City's numbers...

vehicles_msa |>
  filter(NAME == 'Kansas City, MO-KS Metro Area')

# How would we quickly calculate the percentage of zero car households for metro/micro areas nationwide? There are a few options when dealing with tidy data. 

# First, let's just get the variables pertaining to the non-crosstabbed version of the data: just vehicles available by household, no need to look at the differences between household sizes.

vehicles_msa |>
  group_by(GEOID) |>
  slice(1:6)

# The magic of working with grouped dataframes is that the operations are applied group-wise. "Slice" is taking the first 1-6 rows of each group. Since we're grouping by metro area, we're getting just the relevant rows from this data table. Let's pivot the table "wide" to do some quick math.

vehicles_msa |>
  group_by(GEOID) |>
  slice(1:6) |>
  select(-label, -moe) |>
  spread(variable, estimate) |>
  mutate(pct = 100*(B08201_002 / B08201_001 ))

# Which metro has the highest share of zero car households? Let's use "arrange"

vehicles_msa |>
  group_by(GEOID) |>
  slice(1:6) |>
  select(-label, -moe) |>
  spread(variable, estimate) |>
  mutate(pct = 100*(B08201_002 / B08201_001 )) |>
  arrange(desc(pct))


# We can also calculate a rank and see where Kansas City ranks...
vehicles_msa |>
  group_by(GEOID) |>
  slice(1:6) |>
  select(-label, -moe) |>
  spread(variable, estimate) |>
  mutate(pct = 100*(B08201_002 / B08201_001 )) |>
  arrange(desc(pct)) |>
  ungroup() |>
  mutate(rank = row_number()) |>
  filter(NAME == 'Kansas City, MO-KS Metro Area')


# 5.45% of households in the KC metro don't have a car, which is 582 out of 939 metros.

# Let's try another way of calculating this data, also taking advantage of grouped operations.

vehicles_msa_pct = vehicles_msa |>
  group_by(NAME) |>
  slice(1:6) |>
  mutate(pct_of_total =100*(estimate / estimate[1]))

vehicles_msa_pct

# This groupwise operation allows us to take the value for each row for each metro and divide it by the total for that variable universe (ie the number of households in each metro).

# To take this data table and use it elsewhere (like Excel), we can use "clipr" to copy the table output to the clipboard and paste it in Excel.

clipr::write_last_clip()


# Let's make an interactive map of the data. To do this, we'll join the data to our metros.

vehicles_msa_geos = left_join(
  metros,
  vehicles_msa_pct |> filter(variable == 'B08201_002'),
  by = 'GEOID')

mapview(vehicles_msa_geos, zcol = 'pct_of_total')

