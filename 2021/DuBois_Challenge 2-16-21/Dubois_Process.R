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
loadfonts()

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

# Foundation of the plot====
Income_long %>%
ggplot(aes(x=Class, y=Percentage, fill=Expenses)) + 
  geom_bar(stat = "identity", color = "#f4ebd8") +
  scale_fill_manual(values = c("#00aa00","#dc143c", "#d2b48c","#654321","#000000")) +
  coord_polar() +
  theme_void() +
  theme(plot.background = element_rect(fill =NA),
        text = element_text(family = 'Schluber'),
        axis.title=element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right")
  


  
