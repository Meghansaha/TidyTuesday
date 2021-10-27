#===== Tidy Tuesday - Makeup Shades - 3-29-21 =====#
#Sources: Super Mario 256 Font: https://www.dafont.com/super-mario-256.font
        # GameCube Font: https://www.dafont.com/gamecuben.font
        # Dimbo font: https://www.dafont.com/dimbo.font
        # Mario Kart wiki for track distances: https://www.mariowiki.com/Mario_Kart_64

# Loading necessary packages====
pacman::p_load("tidytuesdayR",
               "tidyverse",
               "extrafont",
               "ggtext",
               "glue",
               "shadowtext",
               "patchwork",
               "rlist")

# Special Font Load-in====
fontpath <- readRDS('fontpath.RDS')
font_import(path = fontpath) #Only ran initially (once) after installing new fonts.
loadfonts(device = "win") #Loaded each time after fonts are installed onto system.

# Data Load-in====
TTdata <- tt_load(2021, week = 22)
Records <- TTdata$records

# Initial Data Exploration/Transformations====
#Want to explore data for no shortcuts and only single laps.
Records_filtered <- Records %>%
  filter(shortcut == "No", type == "Single Lap") %>%
  select(-c(type,shortcut,system_played,record_duration)) %>%
  arrange(time)

#Want to pull in track distance data from the Mario Kart wiki.
#Pulling all track names
track <- sort(unique(Records_filtered$track))
distance <- c(747,777,687,893,734,753,691,717,567,527,2000,1025,756,1036,1591,772) #meters pulled from mario wiki.
track_distances <- data.frame("track" = track, 
                              "distance" = distance)

#Arranging tracks by distance and prepping for plotting.
track_distances_mod <- track_distances %>%
  arrange(desc(distance)) %>%
  mutate(track = factor(track, levels = track))

#Merging distance data and filtering for the fastest times from each track
Records_w_distance <-left_join(Records_filtered,track_distances,
                               by = "track") %>%
  group_by(track) %>%
  filter(time == min(time)) %>%
  filter(date == min(date)) %>%
  mutate(speed = (distance/time)*3.6) %>%
  mutate(distance = distance/1000) %>%
  arrange(desc(distance))

#Changing distances to km for track chart
track_distances_mod <- track_distances %>%
  mutate(distance = distance/1000) %>%
  arrange(desc(distance)) %>%
  mutate(track = factor(track, levels = track))
  
#Want to see the track's distances compared, will highlight the longest and shortest
#wrapping the track names
track_distances_mod$track <- str_wrap(track_distances_mod$track, width = 10)

# Creation of Tracks plot (Mario pipes)====
track_distances <- ggplot(data = track_distances_mod, aes(x = forcats::fct_inorder(track), y = distance, fill = distance))+
  geom_bar(stat = "identity", color = "black", width = .7)+
  scale_fill_gradient(low = "#fff170", high = "#ee2727")+
  theme_void()+
  theme(legend.position = "none",
        axis.text.x.bottom = element_text(family = "GAMECUBEN", size = 6),
        axis.title.y.left = element_text(family = "GAMECUBEN", size = 8, hjust = .5),
        axis.title.x.bottom  = element_text(family = "GAMECUBEN", size = 8, hjust = .5))+
  labs(y ="  Track  \n  Length  \n  (Km)  ", x = " ")+
  geom_rect(data = track_distances_mod, color = "black", aes(xmin = .55:15.55, ymin = distance, xmax = 1.45:16.45, ymax = distance + 0.3, fill = distance))+
  scale_color_gradient(low = "#fff170", high = "#ee2727") +
  geom_text(data = track_distances_mod,
            label = scales::number(track_distances_mod$distance,accuracy = .01),
            nudge_y = 0.15,
            family = "GAMECUBEN")

#Saving out to the directory
ggsave("tracks.png", height = 2.34375, width = 12.5, units = "in", dpi = 100, bg = "transparent")


# Speedometer Work====
#Want to test out making a speedometer for a facet wrap chart.
#Creating a mock data frame for the top speeds of each track using a loop to make lists since each top speed varies. goal is to visual make a mark for every 5th unit 
speed_increments <- list()
for(i in seq_along(Records_w_distance$track)){
  speed_increments[[i]] <- data.frame("Track" = Records_w_distance$track[i],
                                      "Speed_cumulative" = seq(from =0, to =plyr::round_any(Records_w_distance$speed[i], accuracy = .5), by = 5),
                                      "bar_height" = 100)

}

names(speed_increments) <- Records_w_distance$track

#final merge of all tracks and addition of player and dates, and time_period
for (i in seq_along(speed_increments)){
speed_increments[[i]] <- left_join(speed_increments[[i]],Records_w_distance, by = c("Track" = "track"))
}

#Generating speedometer gradient scale for each track
colfunc <- colorRampPalette(c("#fff170","#ee2727"))
colors_plot <- colfunc(18) #max speed iterations out of all tracks.
names(colors_plot) <- unique(seq(0,85, by =  5)) #Speed increments from Yoshi's Valley (largest top average speed)

#Copying lookup data over for the hex color transfer.
for (i in seq_along(speed_increments)){
speed_increments[[i]]$color_hex <- as.character(speed_increments[[i]]$Speed_cumulative)
}

#Applying hex colors to applicable speedometer bar with a lookup function
#Creating a lookup function to pull birthplace data into mediumdata by artist name.#
hexlookup <- setNames(as.character(colors_plot),names(colors_plot))

for (i in seq_along(speed_increments)){
speed_increments[[i]]$color_hex <- as.character(lapply(speed_increments[[i]]$color_hex , function(i) hexlookup[i]))
}



#Creating an empty list to generate speedometer plots.
speedometers <- list()

# Speedometer creation through looping function====
for (i in seq_along(speed_increments)){
speedometers[[i]] <- ggplot(speed_increments[[i]], aes(x = as.factor(Speed_cumulative), y = bar_height)) +
  geom_bar(stat = "identity", fill = speed_increments[[i]]$color_hex)+
  ylim(-150,100) +
  scale_x_discrete(limits = as.character(seq(0,200,5)))+
  coord_polar(start =180.65, clip = "on") +
  theme_void() +
  annotate(geom = "text",
           x = 1,
           y = -150,
           label = paste0(scales::number(unique(speed_increments[[i]]$speed), accuracy = .1, suffix = " km/h"),"\n"),
           family = "alarm clock",
           color = tail(speed_increments[[i]]$color_hex,1),
           size = 4) +
  geom_shadowtext(mapping = aes(x = 1, y = -150),
                  data = data.frame(label = "Top Average Speed:\n\n\n\n", x = 1, y = -150),
                  label = data.frame(label = "Top Average Speed:\n\n\n\n", x = 1, y = -150)$label,
                  size = 3,
                  family = "Dimbo",
                  nudge_y = 10)+
  geom_shadowtext(mapping = aes(x = 1, y = -150),
                  data = data.frame(label = paste0("\nPlayer: ",unique(speed_increments[[i]]$player)), x = 1, y = -150),
                  label = data.frame(label = paste0("\nPlayer: ",unique(speed_increments[[i]]$player)), x = 1, y = -150)$label,
                  size = 4,
                  family = "Dimbo",
                  nudge_y = 10)+
  geom_shadowtext(mapping = aes(x = 1, y = -150),
                  data = data.frame(label = paste0(unique(speed_increments[[i]]$Track),":\n\n\n\n\n\n"), x = 1, y = -150),
                  label = data.frame(label = paste0(unique(speed_increments[[i]]$Track),":\n\n\n\n\n\n"), x = 1, y = -150)$label,
                  size = 5.5,
                  family = "Dimbo",
                  nudge_y = 100)}

#Naming the graphs
names(speedometers) <- names(speed_increments)

# Arranging all speedometers into a single grid with cowplot====
grids <- cowplot::plot_grid(plotlist=speedometers, nrow=4)

# Using patchwork (package) to align grid components====
complete_grid <- grids / track_distances + plot_layout(ncol = 1, heights =  c(5, 1)) + plot_annotation(theme = theme(plot.background = element_rect(fill = "#ffffff", color ="#fcba03", linetype = "dashed", size = 3 ),                                                                                       panel.background = element_blank()))

#Saving out main grid into directory to feed back in for final plot.
ggsave(filename= "maingrid.png",
       plot = complete_grid,
       device = "png",
       height = 12.5, 
       width = 12.5, 
       units = "in", 
       dpi = 100, 
       bg = "transparent")
  
# Creation the final plot====
#Loading in background image.
img.file <- "mario background.png"
img <- png::readPNG(img.file)

#Loading in main grid image
final_grid<- png::readPNG("maingrid.png")
final_grid <- grid::rasterGrob(final_grid, interpolate = TRUE)

#Setting up title colors
red <- "<span style = 'color:#d50000'>"
blue <- "<span style = 'color:#0008ff'>"
green <- "<span style = 'color:#0a690a'>"
yellow <- "<span style = 'color:#fecd2a'>"
title_colors <- c(red,blue,green,yellow)
span_tag <- "</span>"

#Forming the words with each letter colored differently
Who <- paste0(red,"W",span_tag,
              blue,"h",span_tag,
              green,"o ",span_tag)

Got <- paste0(blue,"G",span_tag,
              yellow,"o",span_tag,
              green,"t ",span_tag)

There <- paste0(yellow,"T",span_tag,
                red,"h",span_tag,
                blue,"e",span_tag,
                red,"r",span_tag,
                green,"e ",span_tag)

First <- paste0(blue,"F",span_tag,
                red,"i",span_tag,
                blue,"r",span_tag,
                yellow,"s",span_tag,
                green,"t",span_tag,
                red,"?",span_tag)

#Creating the dataframe for the caption label so ggtext can be used.
captiondf <- data.frame(label = "Data Source- *Mario Kart World Records<br> <i>(Only times with no shortcuts were used)</i>. <br>*Track length estimates- Mario Wiki <br> Created By- <img src='https://image.flaticon.com/icons/png/512/23/23931.png', width ='12', height = '12'> meghansharris",
                        x = 90,
                        y = 3,
                        color = "#ffffff")

#Final Plotting
final_plot <- ggplot()+
  labs(title = paste0(Who,Got,There,First),
       subtitle = "*Estimated Top Average Speeds for Mario Kart 64 Tracks")+
  ggpubr::background_image(img)+
  theme_void()+
  theme(panel.background = element_rect(color = "#5B89FE", fill = "#5B89FE"),
        plot.background = element_rect(color = "#5B89FE", fill = "#5B89FE"),
        plot.title = element_textbox_simple(
          family = "Super Mario 256",
          size = 50,
          padding = margin(8,5,10,5),
          margin = margin(0,0,10,0),
          halign = .5,
          valign = 0),
        plot.subtitle = element_textbox_simple(
          family = "Dimbo",
          size = 20,
          color = "#ffffff",
          padding = margin(8,5,2,5),
          margin = margin(0,0,0,0),
          halign = .5,
          valign = .5))+
  ylim(0,100) +
  xlim(0,100) +
  annotation_custom(final_grid, xmin = 0, xmax = 100, ymin = 2, ymax = Inf)+
  geom_richtext(data = captiondf, aes(x=x,y=y,label=label), text.colour = "#ffffff", color = NA, fill = NA, family ="Dimbo")

#Saving it out to the directory
ggsave("mario plot.png", height = 16, width = 12.5, units = "in", dpi = 100, bg = "transparent")
