#===== Tidy Tuesday - Video Games + Sliced - 3-16-21 =====#

# Loading necessary packages====
pacman::p_load("tidytuesdayR",
               "ggplot2",
               "tidyverse",
               "magick",
               "ggtext",
               "extrafont",
               "glue")

#Want to try out the "elementalist" package for my idea===
devtools::install_github("teunbrand/elementalist", dependencies = TRUE)

# Data Load-in====
TTdata <- tt_load(2021, week = 12)
Games <- TTdata$games

# Data Wrangling====
#We HAVE to think with portals! Pulling Portal Data!#

#Extracting all portal entries and only keeping time data and peak data===
Portal_data <- Games %>%
  filter(gamename %in% c("Portal","Portal 2")) %>%
  select(c(gamename:month,peak))

#Collapsing peak counts into a single "year" variable to prep data for line graph. Will remove first and last years due to not having a full year's worth of data for those years===
Portal_data_mod <- Portal_data %>% 
  group_by(gamename,year) %>%
  summarise(total = sum(peak)) %>%
  filter(year != c(2012,2021))

# Graph Creation====

#A simple line graph will do. But want to try to make the lines "glow" a little.#

Portal_data_mod %>%
ggplot(aes(x = year, y = total, color = gamename))+
  geom_line() +
  scale_fill_gradientn(colors=additive_alpha(c("black", "purple")))


