# Tidy Tuesday Week 10 2023 - Numbats#

#=============================================================================#
#Library & Text Load-in--------------------------------------------------------
#=============================================================================#
library(showtext) #For pretty fonts
font_add_google("Cabin Sketch") #Need to load this in first for me, or else fonts won't work
font_add_google("Urbanist")
showtext_auto()
library(tidytuesdayR) # For TT data
library(tidyverse) #For Everything else
library(ggrepel) #For arrows
library(ggtext) #For easier subtitle manipulation
library(glue) #For icons and easier text manipulation

#=============================================================================#
#Data Load-in and Wrangling----------------------------------------------------
#=============================================================================#
numbat_data <- tt_load('2023-03-07')$numbats |>
  select(c(hour)) |>
  mutate(hour = case_when(between(hour,21,23) | between(hour,0,4) ~ "Night",
                          between(hour,5,11) ~ "Morning",
                          between(hour,12,17) ~ "Afternoon",
                          between(hour,18,20) ~ "Evening",
         TRUE ~ NA)) |>
  drop_na()

#Want to look at weekday sightings and times of sightings#
summary_data <- count(numbat_data, hour, name = "total") |>
  mutate(hour = factor(hour, levels = c("Morning","Afternoon","Evening","Night")),
         percent = total/sum(total)) |>
  arrange(hour)

#Year Data#
year_data <- tt_load('2023-03-07')$numbats |>
  select(c(year,hour)) |>
  drop_na() |>
  distinct() |>
  arrange(year)|>
  pull(year) 

year <- paste0(min(year_data), " to ", max(year_data))

#=============================================================================#
#Visual Manipulation/Work/Set-up-----------------------------------------------
#=============================================================================#

#Visual Limits#
xlim <- c(0,20)
ylim <- c(0,10)

#=============================================================================#
#Sky Work----------------------------------------------------------------------
#=============================================================================#

#Sky Section Options#
sky_width <- xlim[2]/length(unique(numbat_data$hour))
sky_height <- ylim[2]
sky_slice_n <- 100
sky_slice_height <- 1
sky_x_starts <- seq(xlim[1],20 - sky_width, length = length(unique(numbat_data$hour)))
sky_x_ends <- seq(xlim[1] + sky_width, xlim[2], length = length(unique(numbat_data$hour)))
sky_y_start <- ylim[1]
sky_y_end <- sky_height
sky_colors <- list("Morning" = c("#E8F0DE","#8AD0CF", "#60BEF2","#182546"),
                   "Afternoon" = c("#BBEEF2", "#8CDEF0", "#04A4E4","#182546"),
                   "Evening" = c("#DE757A","#923072", "#242B6F","#182546"),
                   "Night" = c("#5954A4","#4D439F","#182546"))
sky_slice_colors <- map(sky_colors, ~colorRampPalette(.x)(sky_slice_n)) 
sky_groups <- names(sky_colors)
sky_opts <- list(sky_x_starts,
                 sky_x_ends,
                 sky_y_start,
                 sky_slice_height,
                 sky_groups,
                 sky_slice_colors)

#Sky Slice transformations#
sky_slice_trans <- seq(sky_y_start, (sky_y_end - sky_slice_height), length = sky_slice_n)
sky_slice_iter <- 1:sky_slice_n

#Base sky sections w/o transformation#
sky_slice_data <- map2(sky_slice_trans, sky_slice_iter, ~tibble(x = 0,
                                                                y = rep(.x,5),
                                                                group = rep(paste0("slice_0",.y),5))
)
  
#Final Sky data with aesthetics and transformations#
skies_data <- pmap(sky_opts, ~map(sky_slice_iter, ~sky_slice_data[[.x]]) |>
                     list_rbind() |>
                     mutate(x = x + c(..1,..2,..2,..1,..1),
                            y = y + c(..3,..3,..4,..4,..3),
                            group = paste0(group,"_",..5),
                            fill = rep(..6, each = 5))
)|>
  list_rbind() 

#=============================================================================#
#Border Work-------------------------------------------------------------------
#=============================================================================#

borders<- map(sky_x_ends[1:3], ~tibble(x = rep(.x,100),
                                       y = seq(sky_y_start, sky_y_end, length = 100),
                                       alpha = c(rep(1,20),seq(1,0, length = 80)),
                                       group = paste0("border_",.x))
) |> list_rbind()
                     
#=============================================================================#
#Sun and Moon Work-------------------------------------------------------------
#=============================================================================#

#Sun and Moon data#
theta <- seq(0,2*pi, length = 100)
circle <- tibble(x = cos(theta) * 1.5,
                 y = sin(theta) * 1.5,
                 group = "_circle")

#Grabbing midpoint of each sky section#
midpoints_x <- map_dbl(sky_x_starts, ~.x + 2.5)
midpoints_y <- c(4,6,4,6)

#Sun/Moon Colors#
sun_cols <- c("#f2e8b8","#d6a56f","#d9751e","#D2E7FF")

#list opts#
sun_opts <- list(midpoints_x,
                 midpoints_y,
                 sky_groups,
                 sun_cols)

sun_moons <- pmap(sun_opts, ~circle |>
                    mutate(x = x + ..1,
                           y = y + ..2,
                           group = paste0(..3,group),
                           fill = ..4)) |>
  list_rbind()

#Sun/Moon Fill work#
suns <- sun_moons |> group_split(group)
suns <- suns[c(3,1,2,4)]
sun_bottoms <- map_dbl(suns, ~min(.x[["y"]]))
sun_tops <- map_dbl(suns, ~max(.x[["y"]]))
sun_percents <- summary_data$percent
fill_heights <- ((sun_tops-sun_bottoms)*sun_percents) + sun_bottoms
sun_fills <- map_chr(sun_cols, ~colorRampPalette(c(.x,"#000000"))(10)[4])

fill_opts <- list(suns,
                  fill_heights,
                  sun_fills)

fill_df <- pmap(fill_opts, ~..1 |>
                  mutate(x = x,
                         y = y,
                         fill = ..3)|>
                  filter(y <= ..2)) |> 
  list_rbind()

#sun_texture Work#
size <- .01
trans <- seq(min(sun_moons$x),max(sun_moons$x), by =size)

texture <- tibble(x = c(0,size,size,0,0),
                  y = c(0,0,size,size,0),
                  group = "grid_0")

fills <- sample(colorRampPalette(c("#000000","#ffffff"))(100),(length(trans)*length(trans)), replace = TRUE)


x1 <- rep(unlist(map(trans, ~c(texture$x) + .x) |> flatten()), length(trans))
y1 <- rep(unlist(map(trans, ~rep(c(texture$y),length(trans)) + .x), length(trans)))

grids <- tibble(x = x1,
                y = y1,
                fill = rep(fills,5),
                group = rep(1:(length(trans)*length(trans)), each = 5)) 

grids_final <- map(suns, ~grids |>
                     mutate(logic = sp::point.in.polygon(x,y,.x[["x"]],.x[["y"]])) |>
                     filter(logic == 1)) |>
  list_rbind()




#=============================================================================#
#Star Work---------------------------------------------------------------------
#=============================================================================#
stars <- tibble(x = seq(sky_x_starts[1], sky_x_ends[length(sky_x_ends)], length = 20),
                y = seq(9,10, length = 20),
                alpha = sample(seq(0,1, length = 50), 20, replace = TRUE)) |>
  expand.grid() |>
  mutate(x = jitter(x, amount = .5),
         y = jitter(y, amount = .5)) |>
  filter(x >= sky_x_starts[1] & x <= sky_x_ends[length(sky_x_ends)],
         y <= 11) |>
  slice_sample(prop = .05) 

#=============================================================================#
#Ground Work---------------------------------------------------------------------
#=============================================================================#
ground_top <- tibble(x = seq(sky_x_starts[1], sky_x_ends[length(sky_x_ends)], length = 100),
                     y = (sin(x*(2+x^3))/20) + 1)

ground <- rbind(ground_top,
                tail(ground_top,1),
                tibble(x = c(sky_x_ends[length(sky_x_ends)], sky_x_starts[1]), 
                       y = 0),
                head(ground_top,1)
                ) |>
  mutate(group = "ground")


#=============================================================================#
#Text Work---------------------------------------------------------------------
#=============================================================================#


fills <- fill_df |> group_split(group)
fills <- fills[c(3,1,2,4)]
font_heights <- map_dbl(fills, ~max(.x[["y"]]))
percents <- scales::percent(summary_data$percent)

percent_text <- tibble(x = midpoints_x,
                       y = font_heights,
                       label = percents,
                       group = sky_groups,
                       color = c("#000000","#ffffff","#ffffff","#000000"))

time_text <- tibble(x = midpoints_x, 
                    y = midpoints_y + c(.7,0,.7,.7),
                    label = paste0(sky_groups,c("\n 5:00-11:59",
                                                "\n 12:00-17:59",
                                                "\n 18:00-23:59",
                                                "\n 0:00-04:59")),
                    group = sky_groups,
                    color = c("#000000","#ffffff","#ffffff","#000000"))

plot_title <- "\"It's High In The Afternoon\""

sub_title <- paste0("The Numbat is an insect-eating marsupial that is native to Australia. According to the Atlas of Living Australia, from ",year," most numbats sightings were recorded in the afternoon between 12:00 and 18:00. The least sightings were recorded in the evening time between 18:00 and midnight.")

caption <- glue("Data Source: Atlas of Living Australia <br> <img style='display: inline-block;'  src='icons/twitter.png' width='12' height='12' />  @Meghansharris <br> <img style='display: inline-block;'  src='icons/mastodon.png'  width='12' height='12' />  @Meghansharris@fosstodon")

#=============================================================================#
#Final Plot--------------------------------------------------------------------
#=============================================================================#

skies_data |>
  ggplot(aes(x,y, group = sort(group))) +
  theme_void() +
  theme(plot.title = element_text(family = "Cabin Sketch", 
                                  color = "#ffffff", 
                                  size = 50, 
                                  hjust = .5),
        plot.background = element_rect(fill = "#182546"),
        plot.subtitle = element_textbox_simple(family = "Urbanist", 
                                               color = "#ffffff", 
                                               size = 15, 
                                               margin = unit(c(.4,.3,.4,.3),"cm")),
        plot.caption = element_textbox_simple(family = "Urbanist", 
                                              color = "#ffffff", 
                                              size = 15, 
                                              fill = "#000000"))+
  geom_polygon(fill = skies_data$fill) +
  geom_polygon(data = ground, fill = "#EF5936", 
               color = "#401006", linewidth = .8) +
  geom_polygon(fill = skies_data$fill, alpha = .1) +
  geom_path(data = borders, color = "#000000", 
            alpha = borders$alpha)+
  geom_point(data = stars, aes(group = "stars"), 
             alpha = stars$alpha, 
             color = "#ffffff", 
             size = sample(seq(.02,.4, length = 50), nrow(stars), replace = TRUE)) +
  labs(title = plot_title,
       subtitle = sub_title,
       caption = caption)+
  geom_polygon(data = sun_moons, aes(group = group),
               fill = sun_moons$fill, 
               linewidth = 2, 
               color = sun_moons$fill)+
  geom_polygon(data = fill_df, aes(group = group), 
               fill = fill_df$fill, 
               linewidth = 1, 
               color = fill_df$fill, 
               linejoin = "mitre")+
  geom_polygon(data = grids_final, aes(group = group), 
               fill = grids_final$fill, 
               alpha = .2)+
  geom_text_repel(data = percent_text, aes(label = label), 
                  color = percent_text$color,
                  family = "Cabin Sketch", 
                  nudge_y = 1,
                  size = 15,
                  arrow = arrow(length = unit(0.25, 'cm'), type = 'closed'))+
  geom_text(data = time_text, aes(label = label), 
                  color = percent_text$color,
                  family = "Cabin Sketch", 
                  size = 10)+
  coord_equal(expand = FALSE)

#I really dont feel like fighting with the sizing. I've had enough. Use the console to export it out 🤷
#One day i'll take the time to learn the specifics about exporting images with proper scaling, today is NOT the day.
