#==============================================================================#
#Tidy Tuesday 2025-07-22 MTA Art------------------------------------------------
#==============================================================================#

#==============================================================================#
# Library and Data Load-in------------------------------------------------------
#==============================================================================#
## Libraries--------------------------------------------------------------------
library(tidyverse) # For everything data
library(sf) # Makes the maps go burrr
library(showtext) # For using pretty fonts
library(ggtext) #For easier text aesthetics
library(tidytuesdayR) # Pull in TT data
library(nycgeo) #For pulling in better NYC shapefiles- #remotes::install_github("mfherman/nycgeo") 
library(tigris) # For general Jersey and NY shapefiles
library(artpack) # For colors devtools::install_github("Meghansaha/artpack") - for dev version

## Initial Data Import and Manipulation-----------------------------------------
# I want to plot out the most used materials at each station
# Need to join TT art material data to publicly available geo data
df_mta_art_materials <-
  tidytuesdayR::tt_load("2025-07-22") |>
  pluck("mta_art") |>
  select(
    agency,
    station_name,
    art_material
  ) |>
  # Create a key to join station names on between the TT data and geo data.
  mutate(
    # Pull out the first words that come before a separator (- or /)
    station_key = str_extract(station_name, "^[^-/]+") |>
      # clean up whitespace
      str_trim() |>
      # Change to lowercase
      str_to_lower() |>
      # remove any special chars - keep letters and numbers only
      str_remove_all("[^a-z0-9 ]") |>
      # fully cleanup remaining whitespace
      str_squish(),
    # Make the art maeterials lower case for future operations
    art_material = art_material |> str_to_lower()
  )

# Geo data was found here: (https://data.ny.gov/Transportation/MTA-Subway-Stations/39hk-dx4f/about_data)
df_subway_stations <-
  read_csv("data/MTA_Subway_Stations_20250723.csv") |>
  janitor::clean_names() |>
  select(
    division,
    line,
    stop_name,
    gtfs_latitude,
    gtfs_longitude
  ) |>
  mutate(
    # Same as above
    station_key = str_extract(stop_name, "^[^-/]+") |> 
      str_trim() |>
      str_to_lower() |>
      str_remove_all("[^a-z0-9 ]") |>
      str_squish()
  ) 

#==============================================================================#
# Additional Data Wrangling-----------------------------------------------------
#==============================================================================#
## Materials Vectors------------------------------------------------------------
# Set search terms for scanning the materials variable so each station gets put into a category
# I'm not here for accuracy, I'm here for pretty maps...
# So go do it yourself if this isn't robust enough for you  (づ๑•ᴗ•๑)づ
vec_metals <- c(
  "steel",
  "bronze",
  "copper",
  "aluminium",
  "iron",
  "stainless",
  "zinc"
) |>
  paste0(collapse = "|")

vec_minerals <-
  c(
    "concrete",
    "marble",
    "granite",
    "stone",
    "terracotta"
  ) |>
  paste0(collapse = "|")

vec_ceramic_glass <-
  c(
    "ceramic",
    "glass",
    "tile",
    "porcelain",
    "celadon"
  ) |>
  paste0(collapse = "|")

# For turning into a factor to reorder the legend
vec_material_levels <-
  c(
    "Ceramic/Glass",
    "Mosaic",
    "Metal",
    "Paint",
    "Stone/Minerals",
    "Other",
    "None"
  )

## Establish the crs for geo data-----------------------------------------------
# Don't know what this is? Read here if you want: 
# https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf 
crs <- 4326

## Joining Art and Geo data together--------------------------------------------
df_art_stations <-
  df_mta_art_materials |>
  full_join(
    df_subway_stations,
    by = "station_key",
    relationship = "many-to-many"
  ) |>
  # Want to reduce data down to one art material per station
  distinct(
    art_material, station_key,
    .keep_all = TRUE
  ) |>
  # create a flag for those rows that couldn't be mapped 
  # Because there's no time to sit here and figure out the stations that SHOULD be mapped
  # Some won't be mapped as they are LIRR or Metro North and not subway which is fine.
  mutate(
    flag_missing_geo = case_when(
      is.na(gtfs_latitude) | is.na(gtfs_longitude) ~ TRUE,
      .default = FALSE
    ),
    # Pull out the main material used
    # Picking arbitrary groupings based on what I spotted in the data
    main_material = case_when(
      is.na(art_material) ~ "None",
      str_detect(art_material, "paint") ~ "Paint",
      str_detect(art_material, "mosaic") ~ "Mosaic",
      str_detect( art_material, vec_metals) ~ "Metal",
      str_detect( art_material, vec_ceramic_glass) ~ "Ceramic/Glass",
      str_detect( art_material, vec_minerals) ~ "Stone/Minerals",
      .default = "Other"
    ) |>
      fct_relevel(vec_material_levels),
    # Will be used for color coding on the map
    material_badge_color = case_match(
      main_material,
      "None" ~ "#000000",
      "Paint" ~ "#8c007e",
      "Mosaic" ~ "#ad2b0e",
      "Metal" ~ "#cf933a",
      "Ceramic/Glass" ~ "#135aa1",
      "Stone/Minerals" ~ "#6d7070",
      "Other" ~ "#15540d"
    )
  ) |>
  # Only keep rows that have geodata matched to it
  # Because we are here for PRETTY and NOT ACCURACY
  filter(flag_missing_geo == FALSE) |>
  # Need to convert coordinates into geo data that will map w/ sf
  st_as_sf(coords = c("gtfs_longitude", "gtfs_latitude"), crs = crs) 

## Creating separate data for the actual subway lines, not the stations---------
df_subway_lines <-
  # Convert data into preojections that can be mapped
  st_as_sf(df_subway_stations, coords = c("gtfs_longitude", "gtfs_latitude"), crs = crs) |>
  # Try to connect stations together by grouping the stations by their divisions and lines
  group_by(division, line) |>
  # This will help us to get "separate" lines on the map. Not perfect, but better than nothing.
  summarise(do_union = FALSE) |>
  # Cast linestring to get actual "lines" when mapped
  st_cast("LINESTRING")

## Get the geo data of the boroughs---------------------------------------------
df_nyc_boroughs <-
  nyc_boundaries(geography = "borough") |>
  # Convert geo data to projections that can actually be mapped
  st_transform(crs = crs)

# nycgeo is a great, but old, package with old crs/geo stuff under the hood
# This tries to force the old geo data, into a "newer" format for the plot
# Not sure if it's working, but leaving it in isn't breaking it, so meh.
st_crs(df_nyc_boroughs) <- st_crs(df_nyc_boroughs)

## Generate a bounding box based on the borough data----------------------------
nyc_bbox <- st_bbox(df_nyc_boroughs)

## Create geo data for the surrounding NY and NJ land---------------------------
# These are the surrounding counties we'll want to include
vec_nearby_counties <-
  c(
    "Hudson",
    "Bergen",
    "Essex",
    "Union",
    "Richmond",
    "Passaic",
    "Westchester"
  )

# We'll grab all the NY/NJ counties
# Filter for the ones we want
# Then transform t for mapping
df_surrounding_counties <-
  counties(state = c("NJ", "NY")) |>
  filter(NAME %in% vec_nearby_counties) |>
  # Fun fact, NY ALSO has a county called Richmond
  # But just like most new yorkers consensus
  # We don't acknowledge it unless we're talking about Wu-tang or Method man
  # So kick it out
  filter(!(NAME == "Richmond" & STATEFP == 36)) |>
  st_transform(crs = crs)

## Create a single df to hold all the landmass data
df_landmass <-
  df_surrounding_counties |>
  bind_rows(df_nyc_boroughs) |>
  select(geometry)

#==============================================================================#
# Plot Aesthetics---------------------------------------------------------------
#==============================================================================#
## Font Options-----------------------------------------------------------------
# MTA uses Helvetica, I've downloaded these locally
font_add("helvetica-bold", "assets/Helvetica-Bold.ttf")
font_add("helvetica", "assets/Helvetica Bold Condensed.otf")

# Enable showtext for graphics devices so fonts actually show up
showtext_auto()

## Water color options and data-------------------------------------------------
# I want the background to look pretty, like water, or something special
# Grab some good colors for water
vec_water_pal <- art_pals("ocean", n = 10)

# We'll create a grid of points with varying aesthetics that will act as a water "texture"
# This grid will be a little bigger than the bounding box to ensure everything is covered
vec_length <- 100

df_water_texture <-
  tibble(
    x = seq(nyc_bbox[1] - .05, nyc_bbox[3] - .05, l = vec_length),
    y = seq(nyc_bbox[2], nyc_bbox[4] + .03, l = vec_length )) |>
  # Make a grid
  expand.grid() |>
  mutate(
    # The color of the shape/point
    fill = colorRampPalette(vec_water_pal)(vec_length) |> sample(vec_length^2, replace = TRUE),
    # The transparency
    alpha = seq(.001,.1, l = vec_length) |> sample(vec_length^2, replace = TRUE),
    # The size
    size = seq(30,20, l = vec_length) |> sample(vec_length^2, replace = TRUE),
    # The size of the shape/point's border
    stroke = seq(.1,.01, l = vec_length) |> sample(vec_length^2, replace = TRUE)
  )

## Custom Text Options----------------------------------------------------------
title <- "<span style = 'color:#ffffff;font-family:helvetica-bold'>What is the </span><span style = 'color:#888888;font-family:helvetica'>  underground  art</span><span style = 'color:#ffffff;font-family:helvetica-bold'> of the concrete jungle  </span><span style = 'color:#888888;font-family:helvetica'>made of </span><span style = color:#ffffff;font-family:helvetica-bold>?     </span>"
caption <- "Data Source: Data.ny.gov (MTA Permanent Art Catalog & MTA Subway Stations) <br><img src='assets/icons/bluesky.png' width='12' height='12' /> &#64;meghansharris.bsky.social <br><img src='assets/icons/mastodon.png' width='12' height='12' /> &#64;Meghansharris&#64;fosstodon <br><img src='assets/icons/linkedin.png' width='12' height='12' /> &#64;meghan-harris"

## General color options--------------------------------------------------------
landmass_fill <- "#F2E2C6"
landmass_border <- "#b8a68a"
base_water <- "#95CFEC"
vec_materials_colors <-
  c(
    "Paint" = "#8c007e",
    "Mosaic" = "#ad2b0e", 
    "Metal" = "#cf933a",
    "Ceramic/Glass" = "#135aa1",
    "Stone/Minerals" = "#6d7070",
    "Other" = "#15540d",
    "None" = "#000000"
  )

#==============================================================================#
# Actual Plotting---------------------------------------------------------------
#==============================================================================#
ggplot() +
  ## General theming------------------------------------------------------------
theme_void() +
  theme(
    # General plot options
    plot.background = element_rect(fill = base_water),
    plot.margin = margin(-10, 0, 0, 0),
    # Legend options
    legend.position = c(0.23, 0.65),
    legend.background = element_rect(fill = "#000000"),
    legend.title = element_text(color = "#ffffff", family = "helvetica-bold", size = 45, lineheight = 0.4),
    legend.text = element_text(color = "#ffffff", family = "helvetica-bold", size = 35),
    legend.key = element_rect(fill = "#000000"),
    legend.key.width = unit(1, "cm"),
    legend.margin = margin(5, 0, 35, 0),
    legend.byrow = TRUE
  ) +
  ## The water texture layer----------------------------------------------------
geom_point(
  data = df_water_texture, aes(x,y),
  shape = 23, color = "#ffffff",
  stroke = df_water_texture$stroke,
  alpha = df_water_texture$alpha,
  fill = df_water_texture$fill,
  position = position_jitter(width = .1, height = .1),
  size = df_water_texture$size
) +
  ## The landmass layer---------------------------------------------------------
geom_sf(
  data = df_landmass,
  fill = landmass_fill,
  size = 0.3,
  color = landmass_border,
  linewidth = .2
) +
  ## The subway line layer------------------------------------------------------
geom_sf(
  data = df_subway_lines,
  aes(group = division),
  linewidth = .6,
  color = "#38332b",
  linejoin = "round"
) +
  ## The subway stations layer--------------------------------------------------
geom_sf(
  data = df_art_stations,
  aes(fill = main_material),
  shape = 21, 
  color = "#ffffff", 
  size = 1.5,
  stroke = .2,
) +
  # To apply the the art material colors to the stations#
  scale_fill_manual(
    name = "___________________________\nArt Materials\n", # Hacky top strip for the legend sign
    values = vec_materials_colors
  ) +
  ## Custom title box-----------------------------------------------------------
geom_richtext(
  aes(label = title),
  x = nyc_bbox[1] - .05,
  y = nyc_bbox[4] + .03,
  fill = "#000000",
  size = 20,
  label.padding = unit(c(4, 15, 2, .2), "lines"),
  hjust = 0,
  vjust = .3,
  label.colour = NA
)  +
  # Top strip for title box
  geom_hline(yintercept =  nyc_bbox[4] + .05, color = "#ffffff", linewidth = .5) +
  ## Custom caption box---------------------------------------------------------
geom_richtext(
  aes(label = caption),
  x = nyc_bbox[1] - .05,
  y = nyc_bbox[2] - .08,
  fill = "#000000",
  color = "#ffffff",
  family = "helvetica-bold",
  size = 10,
  lineheight = 0.6,
  label.colour = NA,
  label.padding = unit(c(1.5, 18, 0.5, 0.2), "lines"),
  hjust = 0,
  vjust = 0.02
) +
  # Top strip for caption box
  geom_hline(yintercept =  nyc_bbox[2] - .02, color = "#ffffff", linewidth = .5) +
  ## Custom legend sizing and specs---------------------------------------------
guides(
  fill = guide_legend(
    override.aes = list(
      size = 6,
      color = "#ffffff"
    ),
    ncol = 2
  )
) +
  ## Mapping sf coordinate system-----------------------------------------------
coord_sf(
  crs = crs, 
  expand = FALSE, 
  clip = "on",
  xlim = c(nyc_bbox[1] - .05, nyc_bbox[3] - .05),
  ylim = c(nyc_bbox[2] - .08, nyc_bbox[4] + .07)
)

## Save-------------------------------------------------------------------------
ggsave(
  "art_materials_nyc_subway.png",
  bg = "transparent",
  dpi = 300,
  width = 13,
  height = 10
)