#Billboard 100 Process

#Library load in====
library(tidyverse)
library(showtext)
library(ggimage)
library(ggrepel)



#Data load in====
#Realized the TTdata wasn't the "complete" data (missing ALOT of 90's entries and that just won't fly here - 90's or BUST)
billboard_data <- read_csv("data/Hot Stuff.csv", 
                           col_types = cols(url = col_skip(), WeekID = col_date(format = "%m/%d/%Y"), 
                                            SongID = col_skip(), Instance = col_skip(), 
                                            `Previous Week Position` = col_skip()))
# Data Transformations and Wrangling====
#Filtering for 90s entries===
billboard_90s <- billboard_data %>%
  filter(lubridate::year(WeekID) %in% c(1990:1999))

#Want to get the top "overall" hits for each year in the 90's. - Need to group by year and filter===
billboard_90s_mod <- billboard_90s %>%
  mutate(Year = lubridate::year(WeekID)) %>%
  filter(`Peak Position` == 1) %>%
  filter(`Peak Position` == `Week Position`)

#Now only want the top three songs for each year (spent the most time on the billboards)===
billboard_90s_top <- billboard_90s_mod %>%
  group_by(Year) %>%
  arrange(Year,desc(`Weeks on Chart`))%>%
  filter(`Weeks on Chart` %in% head(`Weeks on Chart`,3))

#Pulling the max values for the chart===
billboard_90s_top_max <-billboard_90s_top %>%
  select(Year,`Weeks on Chart`) %>%
  group_by(Year) %>%
  summarise(.groups = "keep", max_week = max(`Weeks on Chart`))%>%
  mutate(Year = as.character(Year))

#Have an idea for a radial lollipop chart#

#Adding in fonts===
font_add_google("Pathway Gothic One")
font_add_google("Open Sans")
showtext_auto()

#Creating a manual color gradient for the years===
Year_colors <- c("1990" = "#af3918", 
                 "1991" = "#a21152", 
                 "1992" = "#822b75",
                 "1993" = "#612884",
                 "1994" = "#154baf",
                 "1995" = "#0b82b9",
                 "1996" = "#277e9d",
                 "1997" = "#488e35",
                 "1998" = "#e3a934",
                 "1999" = "#b2336a")


# Polygon data creation====

#Making functions to generate polygon points

#Function to generate the "X" points of the rectangle===
rectangle_x <- function(n,by){
  first <- n 
  second <- n + by
  third <- second + 0
  fourth <- third - by
  
  return(c(first,second,third,fourth))
  
}

#Function to generate the "Y" points of the rectanlge===
rectangle_y <- function(base,height){
  first <- base
  second <- first
  third <- base + height
  fourth <- third
  
  return(c(first,second,third,fourth))
}

#Polygons dataframe===
Year_polygons <- tibble(x = unlist(lapply(seq(0,90, by = 10), function(x) rectangle_x(x,10))),
                        y = rep(rectangle_y(19.5,20),10),
                        year = as.character(unlist(lapply(1990:1999, function(x) rep(x,4)))))

#Merging polygon data with billboard data===
Year_polygons_mod <- Year_polygons %>%
  left_join(billboard_90s_top_max, by = c("year" = "Year"), keep = FALSE) %>%
  mutate(year = fct_inorder(year))

#Setting angle specs for better text clarity===
Custom_angles <- c(-20,-50,90,50,20,-20,-50,-90,50,20)


# Label Transformations and work====
billboard_90s_top_final <- billboard_90s_top %>%
  filter(`Weeks on Chart` == head(`Weeks on Chart`,1)) %>%
  select(Song, Performer, `Weeks on Chart`, Year) 

#Pulling out Duplicates===
year_dupes <- duplicated(billboard_90s_top_final$Year)
names(year_dupes) <- billboard_90s_top_final$Year
year_dupes <- as.numeric(names(year_dupes[year_dupes == TRUE]))

#Label creation for final maapping===
billboard_90s_top_final <- billboard_90s_top_final %>%
  mutate(Song_label = paste(Song,"\n",Performer,"\n","Weeks on the Chart:",`Weeks on Chart`)) %>%
  select(-c(Song:Performer)) %>%
  mutate(color = case_when(Year %in% names(Year_colors) ~ Year_colors[as.character(Year)],
                           TRUE ~ NA_character_))
  
#Linking the top song images====
imagefiles <- paste0("images/", list.files("images"))
                     
imagefiles <- imagefiles[grep(".svg", imagefiles)]

billboard_90s_top_final$images <- imagefiles

#Final Plot====
base_plot <- Year_polygons_mod %>%
ggplot(aes(x = x, y = y, fill = year, color = year))+
  scale_fill_manual(values = Year_colors)+
  scale_colour_manual(values = Year_colors)+
  geom_polygon()+ # Year "rectangles"====
  ylim(0,110)+
  xlim(0,100)+
  layer(geom = "segment", # Lollipop sticks====
        position  = "identity",
        stat = "identity",
        inherit.aes = FALSE,
        data = billboard_90s_top_max,
        mapping = aes(x = seq(5,95, by = 10),
                    y = 39.5,
                    xend = seq(5,95, by = 10),
                    yend = max_week + 39),
        params = list(color = "#636262",
                      size = 1,
                      linetype = "dotted"))+
  annotate(geom = 'text', # Year text on circle====
           x = seq(5,95, by = 10),
           y = 29.5,
           label = unique(Year_polygons_mod$year),
           size = 8,
           family = "Pathway Gothic One",
           angle = Custom_angles,
           color = "#ffffff")+
  coord_polar("x")+
  theme_void() +
  theme(legend.position = "none")

final_plot <- base_plot +
  geom_label_repel(mapping = aes(x = c(2.5,7.5,15,22.5,27.5,seq(35,95, by = 10)),
                                y = `Weeks on Chart` + 55,
                                label = Song_label),
                  data = billboard_90s_top_final,
                  nudge_y = 39,
                  segment.color = billboard_90s_top_final$color,
                  segment.size = 1.5,
                  family = "Open Sans",
                  color = "#ffffff",
                  fill = billboard_90s_top_final$color,
                  inherit.aes = FALSE)+
  geom_point(mapping = aes(x = c(2.5,7.5,15,22.5,27.5,seq(35,95, by = 10)), # Adding image borders====
                           y = `Weeks on Chart` + 55),
             data = billboard_90s_top_final,
             shape = 19,
             size = 60,
             color = billboard_90s_top_final$color,
             inherit.aes = FALSE)+
  geom_image(mapping = aes(x = c(2.5,7.5,15,22.5,27.5,seq(35,95, by = 10)), # Adding artist images====
                           y = `Weeks on Chart` + 55,
                           image = images),
             data = billboard_90s_top_final,
             size = .1, 
             by = "width",
             inherit.aes = FALSE)

                  

             
             
  

ggsave("test.svg",final_plot, 
       device = "svg", 
       width = 8.5, 
       height = 8, 
       units = "in", 
       bg = "#171616",
       scale = 2)






















