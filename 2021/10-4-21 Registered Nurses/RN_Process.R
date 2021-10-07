#Tidy Tuesday - Registered Nurses#
# Library Load-in====
library(tidyverse) #For Data wrangling fun
library(tidytuesdayR) #For grabbing the data
library(showtext) #For using "pretty" Google Fonts
library(ggrepel) #For easier text/label manipulation
library(fiftystater) #For adding Alaska and Hawaii neatly onto the legend - Needs to be installed directly from the repo (devtools::install_github("wmurphyrd/fiftystater"))
library(cowplot) #For converting the map legend into a grob

##Data Load-in====
TTdata <- tidytuesdayR::tt_load('2021-10-05')
Nurses_data <- TTdata$nurses

#Want to do some type of comparison by region#

# Data Wrangling/Transformation====
##Creating a region lookup table based on census region designations====
#Using the state constants in base===
Regions_ref <- tibble(State = state.name,
                      Region = state.region,
                      state_abb = state.abb) 

##Merging the tables to add the region variable to the original table====
Nurses_data_regions <- Nurses_data %>%
  left_join(.,Regions_ref, by = "State")

#Finding instances that didn't have a region attached===
Region_non_matches <- unique(Nurses_data_regions$State[which(is.na(Nurses_data_regions$Region))])


#DC is technically apart of the "South" region according to the census.. adding it in===
Regions_ref <- Regions_ref %>%
  add_case(State = "District of Columbia",
           Region = "South",
           state_abb = "DC") %>%
  arrange()

#Taking DC out of the non-matches vector===
Region_non_matches <- Region_non_matches[-1]

#Redoing the merge and taking out places without a region===
Nurses_data_regions <- Nurses_data %>%
  left_join(.,Regions_ref, by = "State") %>%
  filter(!is.na(Region))

#Will compare oldest and newest average salaries for all states#

# Final summarization of the data====
Nurses_data_summarized <- Nurses_data_regions %>%
  select(Region,Year,`Annual Salary Avg`, `Total Employed RN`) %>%
  filter(Year %in% range(Year)) %>%
  group_by(Region,Year) %>%
  summarise(Regional_average = weighted.mean(`Annual Salary Avg`,`Total Employed RN`)) %>%
  ungroup(-Region) %>%
  group_by(Year) %>%
  pivot_wider(names_from = Year, values_from = Regional_average, values_fill = 0) 

##Adding Ranks, salary differences, and custom palettes====
Final_Nurses_Data <- Nurses_data_summarized %>%
  arrange(desc(`2020`)) %>%
  mutate(Difference = `2020` - `1998`) %>%
  mutate(Rank = row_number()) %>%
  mutate(Color = case_when(Region == "West" ~ "#d73d6c",
                           Region == "Northeast" ~ "#d57276",
                           Region == "North Central" ~ "#65b2c6",
                           Region == "South" ~ "#d6c2bc",
                           TRUE ~ NA_character_)) %>%
  mutate(secondary_color = case_when(Color == "#d73d6c" ~ "#e687a4",
                                     Color == "#d57276" ~ "#d1b2b3",
                                     Color == "#65b2c6" ~ "#216475",
                                     Color == "#d6c2bc" ~ "#704d42"))

# Design work====
##Creating a data set for the points and text====
point_data <- Final_Nurses_Data %>%
  select(c(Region:`2020`,Color)) %>%
  pivot_longer(c(`1998`,`2020`), names_to = "Year", values_to = "Regional_Average" ) %>%
  left_join(.,Final_Nurses_Data[,c("Region","Rank")], by = "Region") %>%
  mutate(Regional_Average_pos = case_when(Year == 1998 ~ Regional_Average - 4000,
                                      Year == 2020 ~ Regional_Average + 4000))

##Creating a data set for the background of the graph====
#total_ranks is the number of ranks/row in total you wish to display#
#max_y is the upper y-axis limit of the graph#
#min_y is the lower y-axis limit of the graph#
#bg_data_gen will generate the coordinate for plotting "bars" in the background of the plot#

##Creating the function to make background bars====
bg_data_gen <- function(total_ranks,max_y,min_y){
  x1 <- rep(-Inf,max_y)
  x2 <- rep(Inf, max_y)
  x3 <- x2
  x4 <- x1
  
  y1 <- rep(Inf,max_y)
  y2 <- y1
  y3 <- min_y:max_y
  y4 <- y3
  
  x <- c(x1,x2,x3,x4)
  y <- c(y1,y2,y3,y4)
  
  group_calc <- rep(rep(1:2, each = 4),ceiling(total_ranks/2))
  total_length <- total_ranks*4
  if(length(group_calc) > total_length){group_calc = group_calc[1:total_length]}
  
  bars <- list()
  
  for(i in seq_along(x1)){
    bars[[i]] <- tibble(
      x = c(x1[i],x2[i],x3[i],x4[i]),
      y = c(y1[i],y2[i],y3[i],y4[i]),
      group = group_calc[i]
    )}
    
   returnValue(bind_rows(bars))
  }
  
#4 total ranks (groups), 4.5 is the y axis max 0.5 is the y-axis min#
background_bars <- bg_data_gen(4,4.5,0.5)

##Creating the map legend====
#First need to merge existing ggplot state data with the previously made region categories===
map_legend_data <- fifty_states %>%
  rename("State" = "id") %>%
  mutate(State = str_to_title(State))

map_legend_data_mod <- map_legend_data %>%
  left_join(.,Regions_ref, by = "State") %>%
  filter(State != "District Of Columbia") %>% #"fifty states" data doesn't recognize DC as a state#
  select(long,lat,Region,group)

##Pulling out the created "Region" color palette and merging with the map data====
Region_colors <- Final_Nurses_Data %>%
  select(Region,Color,secondary_color)

map_legend_data_final <- map_legend_data_mod %>%
  left_join(.,Region_colors, by = "Region")

##Making a design data set for the map legend labels====
map_legend_labels <- tibble(x = c(-113,-92,-90,-75),
                            y = c(41,43,33,42),
                            label = c("West","North Central","South","Northeast"))

##Adding custom colors====
map_legend_labels <- map_legend_labels %>%
  left_join(.,Region_colors, by = c("label" = "Region"))

##Making a map background====
#base color===
map_legend_bkgd <- tibble(x = c(-126,-65,-65,-126),
                          y = c(20,20,52,52))

#border chunk===
map_legend_border <- tibble(x = c(-125,-66,-66,-125),
                            y = c(21,21,51,51))

##Plotting the legend====
map_legend <-  map_legend_data_final %>%
  ggplot(aes(x = long, y = lat, group = group)) + 
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = map_legend_bkgd,
        mapping = aes(x = x, y = y),
        params = list(fill = "#ffe5d9",alpha = .2),
        inherit.aes = FALSE)+
  layer(geom = "polygon",
        stat = "identity",
        position = "identity",
        data = map_legend_border,
        mapping = aes(x = x, y = y),
        params = list(fill = NA, color = "#ffffff", size = 3),
        inherit.aes = FALSE)+
  geom_polygon(color= map_legend_data_final$secondary_color, fill= map_legend_data_final$Color) + 
  layer(geom="label",
        stat = "identity",
        position = "identity",
        data = map_legend_labels,
        mapping = aes(x =x , y = y, label = label),
        params = list(color = map_legend_labels$Color, fill = map_legend_labels$secondary_color, family ="Actor", fontface = "bold"),
        inherit.aes = FALSE)+
  coord_equal()+
  theme_void()

##Saving the map legend as a grob====
map_legend <- as_grob(map_legend)


##Loading in Google Font====
font_add_google("Alatsi")
font_add_google("Actor")
font_add_google("Candal")
showtext_auto()

# Final plot layout====
Final_plot <- Final_Nurses_Data %>%
  ggplot(aes(y = Rank, yend = Rank, x = `1998`, xend = `2020`)) +
  theme_void()+
  coord_cartesian(clip = "on")+
  scale_x_continuous(labels = scales::dollar_format(), limits =c(20000,120000), breaks = scales::pretty_breaks(n = 4))+
  scale_y_reverse(limits = c(4.5,0.5))+
  theme(axis.text.x.bottom = element_text(size = 6,family = "Alatsi", color = "#666666"))+
  
  ##Background bars====
  layer(geom = "polygon", 
        inherit.aes = FALSE,
        data = background_bars,
        position = "identity",
        stat = "identity",
        params = list(fill = "#ffe5d9",alpha = .2),
        mapping = aes(x=x,y=y, group = group))+
  
  ##The "Region" bars====
  geom_segment(size = 30, lineend = "square", color = Final_Nurses_Data$Color)+ 
  
  ##The dotted "Difference" lines====
  layer(geom = "segment",
        data = Final_Nurses_Data,
        position = "identity",
        stat = "identity",
        mapping = aes(y = Rank-.3, yend = Rank-.3, x = `1998` - 4000, xend = `2020` + 4000),
        params = list(size = 1, linetype = "dotted", color = "#666666"),
        inherit.aes = FALSE)+
  
  ##The circle end points====
  annotate(geom = "point",  
           x = point_data$Regional_Average_pos, 
           y = point_data$Rank-.3,
           shape = 21,
           size = 5,
           color = "#666666",
           fill = point_data$Color)+
  
  ##The averages for 1998 and 2020====
  annotate(geom = "text",  
           x = ifelse(point_data$Year == 1998, point_data$Regional_Average_pos - 5000, point_data$Regional_Average_pos + 5000), 
           y = point_data$Rank-.3,
           label = paste0(scales::dollar(point_data$Regional_Average),"\n(",point_data$Year,")"),
           size = 5,
           family = "Alatsi",
           color = point_data$Color)+
  
  ##The Region labels====
  annotate(geom = "text", 
           x = ((Final_Nurses_Data$Difference/2) + Final_Nurses_Data$`1998`),
           y = Final_Nurses_Data$Rank,
           label = str_to_upper(Final_Nurses_Data$Region),
           size = 10,
           family = "Actor",
           color = Final_Nurses_Data$secondary_color,
           fontface = "bold")+
  
  ##The delta (change) in average salary====
  annotate(geom = "text", 
           x = ((Final_Nurses_Data$Difference/2) + Final_Nurses_Data$`1998`),
           y = Final_Nurses_Data$Rank - .45,
           label = paste(as.character("\u394"),scales::dollar(Final_Nurses_Data$Difference)),
           size = 6,
           family = "Alatsi",
           color = "#666666",
           fontface = "bold")+
  
  ##The vertical lines connecting the deltas and the dotted lines====
  layer(geom = "segment",
        data = Final_Nurses_Data,
        position = "identity",
        stat = "identity",
        mapping = aes(y = Rank-.3, yend = Rank-.4, x = ((Difference/2) + `1998`), xend = ((Difference/2) + `1998`)),
        params = list(size = 1, linetype = "dotted", color = "#666666"),
        inherit.aes = FALSE)+
  theme(axis.line.x.bottom = element_line(color = "#666666"),
        axis.text.x.bottom =  element_text(size = 12),
        axis.title.x.bottom = element_text(size = 12, color = "#666666", family = "Alatsi"),
        plot.title = element_text(family = "Candal", color = "#666666", size = 30, face = "bold"),
        plot.subtitle = element_text(family = "Actor", color = "#666666", size = 20),
        plot.caption =  ggtext::element_textbox_simple(size = 12,
                                              family = "Alatsi",
                                              color = "#666666"))+
  xlab("\nAverage Registered Nurse (RN) Salary")+
  labs(title = "Average RN Salaries In U.S Regions (1998-2020)\n",
       caption = glue::glue("<br>Data Source: U.S. Bureau of Labor Statistics | Created by: @Meghansharris ", 
                      '<img style="display: inline-block;"  src="images/twitter.png" alt="90s" width="12" height="12" />'))+
  annotation_custom(map_legend, xmin = 85000, xmax =125000, ymin = -Inf, ymax = -3)


# I don't have time to try to figure out the sizing, so I'm using the UI and cheating. I need to stop looking at screens today so forgive me.#
