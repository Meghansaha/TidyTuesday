#===== Tidy Tuesday - Wealth & Income - 2-9-21 =====#

# Library Load-in====
`Required Packages` <- c("tidytuesdayR","knitr","ggplot2","tidyverse","extrafont","glue","ggtext","textclean","scales", "magick") 
`New Packages` <- `Required Packages`[!(`Required Packages` %in% installed.packages()[,"Package"])]
if(length(`New Packages`)) install.packages(`New Packages`)
invisible(suppressPackageStartupMessages(suppressWarnings(lapply(`Required Packages`, require, character.only=T))))

# Data Load-in====
TTdata <- tidytuesdayR::tt_load('2021-02-09')
Homedata <- TTdata$home_owner

#Font load-in===
loadfonts(quiet = TRUE)
windowsFonts()


# It's 4:30am in the morning, I had a terrible day yesterday, and I can't sleep. So why not visualize something that's affected me personally out of low-key spite? Home Ownership :p !! 

# Need to filter for "summary" data. ===
Homedata_summary1976 <- Homedata %>%
  filter(year == "1976")

Homedata_summary2016 <- Homedata %>%
  filter(year == "2016")

#Loading in the "house" icon made in canva and converting to a raster===
house <- image_read("House_transparent.png")
house <- image_fill(house, "white")
raster <- as.raster(house)

# Plotting the summary data in a facet wrap====
#1976===
housefacet1976 <- Homedata_summary1976 %>%
  ggplot(aes(x = 0, y = home_owner_pct, fill = race)) +
  geom_bar(stat = "identity") + 
  ylim(0,1)+
  geom_hline(aes(yintercept = Homedata_summary1976$home_owner_pct), color = "#333333", size = 2 ) +
  annotation_raster(raster,-Inf,Inf,0,Inf)+
  scale_fill_manual(values = c("Black" = "#836d9c",
                                "Hispanic" = "#4EA2A0",
                                "White" = "#518557"))+
  facet_wrap(~race) +
  theme_void()+
  theme(
    legend.position = "none",
    text = element_text(family = 'AvantGarde Bk BT'),
    plot.title = element_textbox_simple(
      size = 18,
      color = "#333333",
      padding = margin(5,5,5,5),
      margin = margin(19,0,0,0),
      halign = 0,
      family = "AvantGarde Bk BT",
      face = "bold"),
    plot.subtitle = element_textbox_simple(
      size = 16,
      color = "#333333",
      padding = margin(5,5,5,5),
      margin = margin(0,0,0,0),
      halign = 0,
      family = "AvantGarde Bk BT",
      face = "italic"),
    strip.text = element_blank()
  ) +
  labs(title = "Percentage of Home Ownership in 1976", 
       subtitle = "By Race")+
  geom_label(aes(y = Homedata_summary1976$home_owner_pct,
                   label = percent(Homedata_summary1976$home_owner_pct)),
               hjust = .5,
               fill = "#333333",
               color = "#ffffff",
               label.size = NA)

ggsave("1976.png", 
       height = 3.88,
       width = 8,
       units = "in",
       dpi = 100)

#2016===
housefacet2016 <- Homedata_summary2016 %>%
  ggplot(aes(x = 0, y = home_owner_pct, fill = race)) +
  geom_bar(stat = "identity") + 
  ylim(0,1)+
  geom_hline(aes(yintercept = Homedata_summary2016$home_owner_pct), color = "#333333", size = 2 ) +
  annotation_raster(raster,-Inf,Inf,0,Inf)+
  scale_fill_manual(values = c("Black" = "#836d9c",
                               "Hispanic" = "#4EA2A0",
                               "White" = "#518557"))+
  facet_wrap(~race) +
  theme_void()+
  theme(
    legend.position = "none",
    text = element_text(family = 'AvantGarde Bk BT'),
    plot.title = element_textbox_simple(
      size = 18,
      color = "#333333",
      padding = margin(5,5,5,5),
      margin = margin(19,0,0,0),
      halign = 0,
      family = "AvantGarde Bk BT",
      face = "bold"),
    plot.subtitle = element_textbox_simple(
      size = 16,
      color = "#333333",
      padding = margin(5,5,5,5),
      margin = margin(0,0,0,0),
      halign = 0,
      family = "AvantGarde Bk BT",
      face = "italic"),
    strip.text = element_blank()
  ) +
  labs(title = "Percentage of Home Ownership in 2016", 
       subtitle = "By Race")+
  geom_label(aes(y = Homedata_summary2016$home_owner_pct,
                 label = percent(Homedata_summary2016$home_owner_pct)),
             hjust = .5,
             fill = "#333333",
             color = "#ffffff",
             label.size = NA)

ggsave("2016.png", 
       height = 3.88,
       width = 8,
       units = "in",
       dpi = 100)

#Loading in PNG facets===
#1976===
facet1976 <- png::readPNG("1976.png")
facet1976 <- grid::rasterGrob(facet1976, interpolate = TRUE)

#2016===
facet2016 <- png::readPNG("2016.png")
facet2016 <- grid::rasterGrob(facet2016, interpolate = TRUE)

#Legend Points===
legenddata <- data.frame(x = rep(4.2,3),
                         y =c(5.35,4.8,4.25),
                         label = c("Black","Hispanic","White"))

# Final Plot Creation====
completehomeplot <- ggplot()+
  xlim(0,5)+
  ylim(0,10)+
  annotation_custom(facet1976, xmin = 0, xmax = 5, ymin = 5, ymax = 10) +
  annotation_custom(facet2016, xmin = 0, xmax = 5, ymin = 0, ymax = 5) +
  theme_void()+
  theme(
  text = element_text(family = 'AvantGarde Bk BT'),
  plot.title = element_textbox_simple(
  size = 22,
  color = "#333333",
  padding = margin(5,5,5,5),
  margin = margin(19,0,0,0),
  halign = 0,
  family = "AvantGarde Bk BT",
  face = "bold"),
  plot.subtitle = element_textbox_simple(
    size = 16,
    color = "#333333",
    padding = margin(5,5,5,5),
    margin = margin(10,0,0,0),
    halign = 0,
    family = "AvantGarde Bk BT"),
  plot.caption = element_textbox_simple(
    size = 12,
    color = "#333333",
    padding = margin(5,5,5,5),
    margin = margin(20,0,0,0),
    halign = 1))+
  labs(title = "Houses Built on Inequality:",
       subtitle = "Out of <b><span style = 'color:#836d9c'>Black</span></b>, <b><span style = 'color:#4EA2A0'>Hispanic</span></b>, and <b><span style = 'color:#518557'>White</span></b> Races, only <b><span style = 'color:#836d9c'>Black</span></b> people have seen an overall net <b>decrease</b> in home ownership from 1976 to 2016. The data provided by the <b>Urban Institute</b> and <b>US Census</b> shows that <b><span style = 'color:#518557'>White</span></b> and <b><span style = 'color:#4EA2A0'>Hispanic</span></b> Races have seen overall <b>increases</b> in home ownership by <b><span style = 'color:#4EA2A0'>3.3%</b></span> and <b><span style = 'color:#518557'>0.4%</b></span> respectively. <b><span style = 'color:#836d9c'>Black</span></b> people have seen a net <b>decrease</b> of<b> <span style = 'color:#836d9c'>2.6%</b>.",caption = "<b>Data Source: Urban Institute/US Census | Created By: @meghansharris</b>")+
  geom_rect(aes(xmin=4.15, xmax =5, ymin = 3.8, ymax = 6),
            fill = "#333333",
            linetype = "dotted",
            color = "#333333",
            alpha = .8)+
  geom_point(data = legenddata, aes(x=x, y = y),
             color = c("#836d9c","#4EA2A0","#518557"),
             size = 6)+
  geom_text(data = legenddata, aes(x = x, y = y), 
            nudge_x = 0.1, 
            label = legenddata$label,
            size = 6,
            hjust = 0,
            color = c("#836d9c","#4EA2A0","#518557"),
            fontface = "bold",
            family = "AvantGarde Bk BT")+
  annotate(geom = "text", 
           label = "Races:",
           color = "#ffffff",
           fontface = "bold",
           size = 5,
           family = 'AvantGarde Bk BT',
           x = 4.45, 
           y = 5.7,
           hjust = 0,
           vjust = 0)

#Saving PNG for export====
ggsave("Complete_homeplot.png", height = 8, width = 11, units = "in", dpi = 100)
  


