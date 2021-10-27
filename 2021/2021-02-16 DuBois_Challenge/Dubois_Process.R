#===== Tidy Tuesday - W.E.B. Du Bois Challenge - 2-16-21 =====#

# Library Load-in====
`Required Packages` <- c("tidytuesdayR","knitr","ggplot2","tidyverse","extrafont","glue","ggtext","textclean","scales","ggpubr", "magick") 
`New Packages` <- `Required Packages`[!(`Required Packages` %in% installed.packages()[,"Package"])]
if(length(`New Packages`)) install.packages(`New Packages`)
invisible(suppressPackageStartupMessages(suppressWarnings(lapply(`Required Packages`, require, character.only=T))))

# Data Load-in====
TTdata <- tt_load('2021-02-16')
Income <- TTdata$income

#Have an idea to do a circular heatmap..is that possible? idk let's find out.#

#Font import (Use the direct path to your fonts on your actual PC, not the default within the R library. This only works with True Type fonts also===
font_import(paths = "c:/users/meghansa/appdata/local/microsoft/windows/fonts", pattern = "Schluber")
font_import(paths = "c:/users/meghansa/appdata/local/microsoft/windows/fonts", pattern = "atwriter")
loadfonts(device = "win")

# Data Wrangling====
#Transposing to long format===
Income_long <- Income %>%
pivot_longer(Rent:Other,
             names_to = "Expenses",
             values_to = "Percentage")

#Taking care of NAs (practicing with dplyr)===
Income_long <- Income_long %>%
mutate(Percentage = coalesce(Percentage,0))

#Figuring out total expenses percentages for graph ordering===
Expense_factors <- names(sort(colSums(Income[,3:7], na.rm = TRUE), decreasing = TRUE))

Income_long$Expenses <- factor(Income_long$Expenses, levels = Expense_factors)


#Loading in background image===
img <- png::readPNG("dubois bckgd.png")

# Foundation of the plot====
incomewedges <- Income_long %>%
ggplot(aes(x=Class, y=Percentage, fill=Expenses)) + 
  geom_bar(stat = "identity", color = "#f4ebd8", alpha = .9) +
  scale_fill_manual(values = c("#00aa00","#dc143c", "#d2b48c","#654321","#000000")) +
  coord_polar() +
  theme_void() +
  theme(plot.title = element_text(size = 20, hjust = .5),
        plot.subtitle = element_textbox_simple(
          family = "Another Typewriter",
          size = 12,
          color = "#000000",
          padding = margin(5,5,5,5),
          margin = margin(30,0,0,0),
          halign = 0),
        text = element_text(family = 'Schluber'),
        axis.title=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(),
        legend.position = "right",
        legend.title = element_text(family = "Another Typewriter"),
        legend.text = element_text(family = "Another Typewriter"),
        plot.caption = element_textbox_simple(
          family = "Another Typewriter",
          size = 12,
          color = "#000000",
          padding = margin(5,5,5,5),
          margin = margin(20,0,0,0),
          halign = .5)) + 
  labs(title = "Income and Expenditure of 150 Negro Families in Atlanta, GA., U.S.A",
       subtitle = "<br>Each wedge represents annual income bracket groups (in U.S Dollars $) and the proportion of expenses spent by each bracket on average. Across most brackets, <span style = 'color:#00aa00'>Food</span> was the highest expense. The highest income bracket (Over $1000) spent the most on  <span style = 'color:#dc143c'>'Other'</span> expenses which included things like art, sickness, savings and travels.<br><br><br>",
       caption = "Data Source: Du Bois Data Challenge <br> Created By: <img src='https://image.flaticon.com/icons/png/512/23/23931.png', width ='12', height = '12'> meghansharris")

#saving the coord plot out to embed onto the "old paper" background===
ggsave("income_wedges.png", height = 8, width = 11, units = "in", dpi = 100, bg = "transparent")

#Loading the plot back in as an image to overlay on the "old paper"====
wedges <- png::readPNG("income_wedges.png")
wedges <- grid::rasterGrob(wedges, interpolate = TRUE)

#Loading in the "old" paper plot
Finalplot <- ggplot()+
  theme_void()+
  background_image(img) +
  xlim(0,5)+
  ylim(0,10)+
  annotation_custom(wedges, xmin = -Inf, xmax = Inf, ymax = Inf) 

#Exporting out the final plot====
ggsave("final_wedges.png", height = 8, width = 11, units = "in", dpi = 100, bg = "transparent")
