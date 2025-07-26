#==============================================================================#
#Tidy Tuesday 2025-07-22 MTA Art------------------------------------------------
#==============================================================================#

#==============================================================================#
# Library and Data Load-in------------------------------------------------------
#==============================================================================#
## Libraries----
library(tidyverse) # For everything data
library(sf) # For mapping
library(showtext) # For using pretty Google fonts
library(ggtext) #For easier text aesthetics
library(glue) #Also for easier text aesthetics
library(nycgeo) # Pulling in borough geo files
library(tidytuesdayR) # Pull in TT data
library(nycgeo) #For NYC shapefiles- #remotes::install_github("mfherman/nycgeo") 
library(tigris)
library(artpack) # devtools::install_github("Meghansaha/artpack") - for dev version

caption <- "Data Source: Data.ny.gov (MTA Permanent Art Catalog & MTA Subway Stations) <br><img src='icons/bluesky.png' width='12' height='12' /> &#64;meghansharris.bsky.social <br><img src='icons/mastodon.png' width='12' height='12' /> &#64;Meghansharris&#64;fosstodon <br><img src='icons/linkedin.png' width='12' height='12' /> &#64;meghan-harris"

## Data----
# Set the CRS 
crs <- 4326

# I want to plot out the most used material at each station
# Need to join art material data geo data
# Geo data was found here: (https://data.ny.gov/Transportation/MTA-Subway-Stations/39hk-dx4f/about_data)
df_mta_art_materials <-
  tidytuesdayR::tt_load("2025-07-22") |>
  pluck("mta_art") |>
  select(
    agency,
    station_name,
    art_material
  ) |>
  # Create a key to join station names on to try to match as much as possible
  mutate(
    # Pull out the first words that come before a seperator (- or /)
    station_key = str_extract(station_name, "^[^-/]+") |>
      # clean whitespace
      str_trim() |>
      # Change to all lower
      str_to_lower() |>
      # remove any special chars - keep letters and numbers only
      str_remove_all("[^a-z0-9 ]") |>
      # fully cleanup whitespace
      str_squish(),
    art_material = art_material |> str_to_lower()
  )

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

# Materials Vectors
## Metals
vec_metals <- c(
  "steel",
  "bronze",
  "copper",
  "aluminium",
  "iron",
  "stainless",
  "zinc"
) |> paste0(collapse = "|")

vec_minerals <-
  c(
    "concrete",
    "marble",
    "granite",
    "stone",
    "terracotta"
) |> paste0(collapse = "|")

vec_ceramic_glass <-
  c(
    "ceramic",
    "glass",
    "tile",
    "porcelain",
    "celadon"
  ) |> paste0(collapse = "|")


# Join together by station key
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
      fct_relevel("Ceramic/Glass", "Mosaic", "Metal", "Paint", "Stone/Minerals", "Other", "None"),
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
  filter(flag_missing_geo == FALSE) |>
  # Need to convert coordinates into geo data that will map w/ sf
  st_as_sf(coords = c("gtfs_longitude", "gtfs_latitude"), crs = crs) 


df_subway_lines <-
  st_as_sf(df_subway_stations, coords = c("gtfs_longitude", "gtfs_latitude"), crs = crs) |>
  group_by(division, line) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING") |>
  mutate(
    color = case_when(
      division == "BMT" ~ "#fccc0a",
      division == "IND" & str_detect(line, "6th Av|63rd St|Concourse") ~ "#FF6319",
      division == "IND" & str_detect(line, "Liberty Av|Queens - Archer|Queens Blvd|Rockaway") ~ "#08179C",
      division == "IND" & str_detect(line, "Second Av") ~ "#fccc0a",
      division == "IND" & str_detect(line, "8th Av") ~ "#0062CF",
      division == "IND" & line == "Crosstown" ~  "#00933c",
      division == "IRT" & line == "Broadway - 7Av" ~  "#ee352e",
      division == "IRT" & line == "Flushing" ~  "#b933ad",
      division == "IRT" & str_detect(line, "Lexington") ~  "#b933ad",
      division == "IRT" ~ "#00933c",
      division == "SIR" ~ "#2360A5"
    )
  )

# get sf boundaries
all_nyc_boroughs <- nyc_boundaries(geography = "borough") |> st_transform(crs = crs)
st_crs(all_nyc_boroughs) <- st_crs(all_nyc_boroughs)

nyc_bbox <- st_bbox(all_nyc_boroughs)

nj_counties <- counties(state = "NJ", year = 2020) |>
  st_transform(crs = crs)

ny_counties <- counties(state = "NY", year = 2020) |>
  st_transform(crs = crs)

nj_nearby_counties <- c("Hudson", "Bergen", "Essex", "Union", "Richmond", "Passaic")
ny_nearby_counties <- c( "Westchester" )

nj_filtered <- nj_counties |>
  filter(NAME %in% nj_nearby_counties)

ny_filtered <- ny_counties |>
  filter(NAME %in% ny_nearby_counties)


font_add("helvetica-bold", "assets/Helvetica-Bold.ttf")
font_add("helvetica", "assets/Helvetica Bold Condensed.otf")

# Enable showtext for graphics devices
showtext_auto()

vec_water_pal <- art_pals("ocean", n = 10)



# Transform to fit NYC bounding box
# Calculate target dimensions
nyc_width <- (nyc_bbox[3] - .05) - (nyc_bbox[1] - .05)  # xmax - xmin
nyc_height <- (nyc_bbox[4] + .03) - nyc_bbox[2]         # ymax - ymin

df_water_texture <-
  tibble(x = seq(nyc_bbox[1] - .05, nyc_bbox[3] - .05, l = 100),
         y = seq(nyc_bbox[2], nyc_bbox[4] + .03, l = 100 )) |>
  expand.grid() |>
  mutate(fill = colorRampPalette(vec_water_pal)(100 * 100))


df_title <-
  tibble(
    label ="<span style = 'color:#ffffff;font-family:helvetica-bold'>What is the </span><span style = 'color:#888888;font-family:helvetica'>  underground  art</span><span style = 'color:#ffffff;font-family:helvetica-bold'> of the concrete jungle  </span><span style = 'color:#888888;font-family:helvetica'>made of </span><span style = color:#ffffff;font-family:helvetica-bold>?     </span>",
    fill = "#000000",
    color = "#ffffff"
    
  )
  


# plot----
ggplot() +
  geom_point(
    data = df_water_texture, aes(x,y),
    shape = 23, color = "#ffffff",
    stroke = .1, alpha = seq(.001,.1, l = 100) |> sample(100*100, replace = TRUE), fill = df_water_texture$fill |> sample(),
    position = position_jitter(width = .1, height = .1),
    size = seq(30,20, l = 100) |> sample(10000, replace = TRUE)
    ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#95CFEC"),
    plot.margin = margin(-10, 0, 0, 0),
    legend.position = c(0.23, 0.65),
    legend.background = element_rect(fill = "#000000"),
    legend.title = element_text(color = "#ffffff", family = "helvetica-bold", size = 45, lineheight = 0.4),
    legend.text = element_text(color = "#ffffff", family = "helvetica-bold", size = 35),
    legend.key = element_rect(fill = "#000000"),
    legend.key.width = unit(1, "cm"),
    legend.margin = margin(5, 0, 35, 0),
    legend.byrow = TRUE) +
  guides(
    fill = guide_legend(
      override.aes = list(
        size = 6,
        color = "#ffffff"
      ),
      ncol = 2
    )
  ) +
  geom_sf(data = nj_filtered, fill = "#F2E2C6", size = 0.3, color = "#b8a68a", linewidth = .2) +
  geom_sf(data = ny_filtered, fill = "#F2E2C6", size = 0.3, color = "#b8a68a", linewidth = .2) +
  geom_sf(data = all_nyc_boroughs$geometry, fill = "#F2E2C6", color = "#b8a68a", linewidth = .2) +
  geom_sf(data = df_subway_lines, aes(group = division), linewidth = 1.3, color = df_subway_lines$color, linejoin = "round") +
  geom_sf(data = df_art_stations,
          aes(fill = main_material),  # Use aes() instead of fill = df_art_stations$material_badge_color
          shape = 21, 
          color = "#ffffff", 
          size = 1.5,
          stroke = .2, na.rm = TRUE) +
  scale_fill_manual(
    name = "___________________________\nArt Materials\n",
    values = c(
      "Paint" = "#8c007e",
      "Mosaic" = "#ad2b0e", 
      "Metal" = "#cf933a",
      "Ceramic/Glass" = "#135aa1",
      "Stone/Minerals" = "#6d7070",
      "Other" = "#15540d",
      "None" = "#000000"
    )
  ) +
  coord_sf(
    crs = crs, 
    expand = FALSE, 
    clip = "on",
    xlim = c(nyc_bbox[1] - .05, nyc_bbox[3] - .05),
    ylim = c(nyc_bbox[2] - .08, nyc_bbox[4] + .07)
  ) +
  geom_richtext(
    data = df_title, aes(label = label),
    x = nyc_bbox[1] - .05,  # Left edge of plot area (same as xlim)
    y = nyc_bbox[4] + .03,
    fill = "#000000",
    size = 20,
    label.padding = unit(c(4, 15, 2, .2), "lines"),
    hjust = 0,  # Left align the text within the box
    vjust = .3,
    label.colour = NA
  )  +
  geom_hline(yintercept =  nyc_bbox[4] + .05, color = "#ffffff", linewidth = .5) +
  geom_richtext(
    aes(label = caption),
    x = nyc_bbox[1] - .05,  # Left edge of plot area (same as xlim)
    y = nyc_bbox[2] - .08,  # Bottom edge of plot area (same as ylim)
    fill = "#000000",
    color = "#ffffff",
    family = "helvetica-bold",
    size = 10,
    lineheight = 0.6,
    label.colour = NA,
    label.padding = unit(c(1.5, 18, 0.5, 0.2), "lines"),  # top, right (extended), bottom, left (minimal)
    hjust = 0,  # Left align text
    vjust = 0.02   # Anchor to bottom of text box
  ) +
  geom_hline(yintercept =  nyc_bbox[2] - .02, color = "#ffffff", linewidth = .5)


## Save----

ggsave("map.png", bg = "transparent", dpi = 300, width = 13, height = 10)