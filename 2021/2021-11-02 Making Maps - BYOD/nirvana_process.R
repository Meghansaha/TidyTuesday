#====Nirvana Data Processing====

# Library Load-in====
library(tidyverse) # For everything data
library(tidygeocoder) # For getting geocodes of the cities
library(ggimage) # For getting album art onto the visual
library(showtext) # For loading in Google Fonts
library(glue) #For the customized title and caption
library(ggtext) #For the customized title and caption


# Data Load-in====
##Loading in the scraped data with manually added States. Having states makes the geocoding results more accurate====
Nirvana_Concerts <- read_csv("Concerts - States Added.csv")

# Data Transformation====
##Fixing the dates and performing the first-pass of geocoding on the set====
Nirvana_Concerts <- Nirvana_Concerts %>%
  mutate(Date = lubridate::dmy(Date)) %>%
  geocode(city = City,
          state = State,
          country = Country)

##Isolating and running a second pass through the missing locations by removing the "State" from the query==== 
#(These are popular/unique cities. some don't have "States" like the U.S and thus can be found by omitting it)#
no_geos <- Nirvana_Concerts %>%
  filter(is.na(lat)) %>%
  select(-c(State,lat,long)) %>%
  mutate(Country = if_else(Country == "United Kingdom","Ireland",Country)) %>%
  geocode(city = City,country = Country) %>%
  select(-c(Category, Country))


##Merging the missing geocodes back into the original set====
Nirvana_Concerts_geo <- left_join(Nirvana_Concerts, no_geos,
                                  by = c("Date","City"))

##Merging the latitude and longitude columns together====
Nirvana_Concerts_geo <- Nirvana_Concerts_geo %>%
  mutate(lat =  coalesce(lat.x,lat.y),
         lon = coalesce(long.x,long.y)) %>%
  select(-c(lat.x,lat.y,long.x,long.y))

##Creating clean categories for the tour types====
categories <- tibble(Category = unique(Nirvana_Concerts_geo$Category),
                     Tour_Type = c("Early Gigs", "Bleach - Club Shows", "1990-1991 Club Shows",
                                   "Nevermind Clubs and Festival Shows", "1992-1993 Arena, Stadium, and Festival Shows",
                                   "1993 Intermittent Arena Shows", "In Utero Arena Shows"))

##Merging the categories in====
final_concerts <- left_join(Nirvana_Concerts_geo, categories,
                            by = c("Category")) %>%
  select(-Category) %>%
  mutate(State = if_else(is.na(State),City,State)) %>%
  relocate(Tour_Type, .after = Country)


# Random Summary Statistics====

##Seeing which months Nirvana played shows in the most====
months_tally <- final_concerts %>%
  select(Date) %>%
  mutate(Month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>%
  count(Month, name = "Total") 

max_months <- rev(as.character(months_tally$Month[kit::topn(months_tally$Total,2, decreasing = TRUE)]))
maxmonths_count <- rev(months_tally$Total[kit::topn(months_tally$Total,2, decreasing = TRUE)])

##And the least====
min_months <- rev(as.character(months_tally$Month[kit::topn(months_tally$Total,1, decreasing = FALSE)]))
minmonths_count <- rev(months_tally$Total[kit::topn(months_tally$Total,1, decreasing = FALSE)])

##And the average amount of shows per month====
avg_shows <- floor(mean(months_tally$Total))

# Mapping ...FINALLY=====

##Pulling "world map" polygons====
world_map <- map_data("world") %>%
  filter(region != "Antarctica")

##Getting a unique list of all "regions" and concert countries for shading work====
regions <- tibble(region = unique(world_map$region))

countries <- tibble(region = unique(final_concerts$Country)) %>%
  mutate(region = case_when(region == "United States" ~ "USA",
                            region == "United Kingdom" ~ "UK",
                            TRUE ~ region))

regions$fill <- regions$region %in% countries$region

##Merging and coding for country shading====
world_map <- left_join(world_map,regions,
                      by = "region") %>%
  mutate(fill = if_else(fill == TRUE, "#33332e","#1e1e1f")) 

##Adding line color to the path====
final_concerts <- final_concerts %>%
  mutate(line_color = case_when(Tour_Type == "Early Gigs" ~ "#ffffff",
                                Tour_Type == "Bleach - Club Shows" ~ "#000000",
                                Tour_Type == "1990-1991 Club Shows" ~ "#116776",
                                Tour_Type == "Nevermind Clubs and Festival Shows" ~ "#23ceec",
                                Tour_Type == "1992-1993 Arena, Stadium, and Festival Shows" ~ "#61D6A8",
                                Tour_Type == "1993 Intermittent Arena Shows" ~ "#B0E054",
                                TRUE ~ "#FFEA01"),
         fill = line_color)


# Final Plot work====
##Loading in the blur face===
blur_face <- png::readPNG("blur face.png")

##Making the legend====
#making a function because i'm TIRED and am not calcuating this by hand#
legend_maker <- function(n,height){
  x <- list()
  y <- list()
  
  for(i in seq_along(1:n)){
    x[[i]] <- c(0,1,1,0) + i
    y[[i]] <- c(0,0,height,height)
  }
  
  return(tibble(x = unlist(x),
                y = unlist(y),
                group = rep(1:n, each = 4)))
}

#Creating the legend data set, adding color values, and images====
legend_data <- legend_maker(7,1) %>%
  mutate(fill = rep(unique(final_concerts$fill), each = 4)) %>%
  mutate(Period = rep(unique(final_concerts$Tour_Type), each = 4))

##Prepping for pulling in the album images====
images <- c(NA,"Bleach.png",NA,"Nevermind.png",NA,NA,"In Utero.png")
names(images) <- unique(legend_data$Period)

##Need to calculate the midpoints for the images====
files <- images[c(2,4,7)]

midpoint_calc <- function(files, height){
  x <- c()
  
  for(i in seq_along(1:length(files))){
    x[i] <- mean(range(legend_data$x[legend_data$Period == names(files[i])]))
  }
  return(tibble(x = x,
                y = height))
}

album_data <- midpoint_calc(files,1.2) %>%
  mutate(album = files)

period_data <- tibble(x = seq(1.5,7.5, by = 1),
                      y = .5,
                      period = unique(legend_data$Period))

# Font Load in====
font_add_google("Bodoni Moda")
showtext_auto()

# Making the legend====
legend <- legend_data %>%
  ggplot(aes(x,y,group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = NA, color = NA),
        panel.background =  element_rect(fill = NA, color = NA),
        panel.grid = element_blank())+
  geom_image(data = album_data, aes(x,y,image = album), size = .2, by = "width", inherit.aes = FALSE)+
  geom_polygon(fill = legend_data$fill)+
  coord_cartesian(expand = FALSE, xlim =c(1,8), ylim = c(0,1.55), clip = "off") +
  geom_text(data = period_data, aes(x,y, label =str_to_upper(str_wrap(period, width = 15))),
            family = "Bodoni Moda", 
            color = c("#000000","#ffffff",rep("#000000",5)),
            size = 4.2,
            fontface = "bold",
            inherit.aes = FALSE)+
  annotate("text",
           x = 1, y = 1.5,
           family = "Bodoni Moda", 
           color = "#ffffff",
           size = 6,
           fontface = "bold",
           hjust = 0,
           label = "TOUR LEGEND:")

##Saving the legend out====
ggsave("legend.png",legend, 
       device = "png", 
       width = 1200, 
       height = 600, 
       units = "px", 
       bg = "transparent")


legend_image <- png::readPNG("legend.png")


# Setting some css tags for easier subtitle creation====
yellow <- "<b><span style = 'color:#FFEA01'>"
white <- "<b><span style = 'color:#ffffff'>"
blue <- "<b><span style = 'color:#23ceec'>"
closing <- "</b></span>"

final_map <- world_map %>%
  ggplot(aes(long,lat, group = group)) +
  theme_void()+
  ggpubr::background_image(blur_face)+
  theme(plot.background = element_rect(fill = "#1e1e1f", color = NA),
        panel.background =  element_rect(fill = "#1e1e1f", color = NA),
        panel.grid = element_blank(),
        plot.title = element_textbox_simple(
          padding = margin(0,0,10,0),
          halign = 0,
          valign = 0),
        plot.subtitle = element_textbox_simple(
          family = "Pathway Gothic One",
          padding = margin(0,.95,0,.5, unit = "in"),
          color = "#ffffff",
          lineheight = 2,
          size = 12),
        plot.caption = element_textbox_simple(size = 12,
                                              family = "Pathway Gothic One",
                                              color = "#ffffff",
                                              padding = margin(.1,.1,.1,.1, unit = "in"),
                                              hjust = 1))+
  geom_polygon(fill = world_map$fill, color = "#292926", size = .6)+
  geom_point(data = final_concerts, aes(x = lon, y = lat),
             fill = final_concerts$fill, 
             color = "#000000", 
             shape = 21, 
             size = 2, 
             stroke = 1.5, 
             inherit.aes = FALSE)+
  geom_path(data = final_concerts, aes(x = lon, y = lat), 
            size = .5, 
            color = final_concerts$line_color, 
            alpha = .4,
            inherit.aes = FALSE)+
  coord_cartesian(ylim = c(-130,80))+
  annotation_raster(legend_image, xmin = 20, xmax =Inf, ymin = -140, ymax = -40)+
  labs(title = paste0('<img style="display: inline-block;"  src="Title.png" alt="Where in the World has Nirvana been?" width="1000" height="50" />'),
                                         subtitle = paste0("This visual maps Nirvana concert/touring data from <i>Wikipedia.org</i> and shows that the band toured in ",yellow,
                                                          length(unique(final_concerts$City)),closing," different cities in ",yellow,nrow(countries),closing," countries in the span of nearly ",
                                                          yellow,lubridate::year(max(final_concerts$Date)) - lubridate::year(min(final_concerts$Date)),closing," years. Excluding cancelled concerts, Nirvana held ",
                                                          yellow,nrow(final_concerts),closing," shows in this time frame. Nirvana's history of shows are broken down into 7 main time periods starting with the band's ",
                                                          white,"'Early Gigs' ",closing," before their first studio album, <i><b>Bleach</b></i> was released and ending with the ",yellow,"In Utero Arena era ",closing,
                                                          "that took place after the release of their final studio album <i><b>In Utero</b></i>. Each point of the map represents a city where a show was held. 
                                                          Each point is connected by lines that run in chronolgical order. Each line and point are color-coded to correspond to the time period in which the shows took place. ",
                                                          "Nirvana's album, ",blue,"<i>Nevermind</i>",closing," was easily their most successful album to gain mainstream attention. This is also the period of time when they traveled the farthest across the world."),
       caption = glue("Data Source: Wikipedia.org | Created by: @Meghansharris ", '<img style="display: inline-block;"  src="twitter.png" alt="90s" width="12" height="12" />'))


