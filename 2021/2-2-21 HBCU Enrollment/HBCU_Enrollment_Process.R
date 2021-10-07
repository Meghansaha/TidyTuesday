#===== Tidy Tuesday - HBCU - 2-2-21 =====#

# Library Load-in====
`Required Packages` <- c("tidytuesdayR","knitr","ggplot2","tidyverse","extrafont","glue","ggtext","textclean","scales","gganimate", "magick") 
`New Packages` <- `Required Packages`[!(`Required Packages` %in% installed.packages()[,"Package"])]
if(length(`New Packages`)) install.packages(`New Packages`)
invisible(suppressPackageStartupMessages(suppressWarnings(lapply(`Required Packages`, require, character.only=T))))

# Data Load-in====
TTdata <- tidytuesdayR::tt_load('2021-02-02')
HBCUdata <- TTdata$hbcu_black

#Font Load-in===
loadfonts()
windowsFonts()

#Have the idea to make an animated donut chart comparing enrollment total between females and males across the years. Maybe an animated donut chart?.#

#Data Carpentry====
#Selecting what's needed, pivoting sex columns===
HBCUgenderenroll <- HBCUdata %>%
  select(Year:Females) %>%
  pivot_longer(cols = Males:Females, names_to = "Sex", values_to = "Enrollment")

#Calculating percentages, grouping by year and calculating cumsums (ymax)=== 
HBCUgenderenroll <- HBCUgenderenroll  %>%
  mutate(Percentage = Enrollment/`Total enrollment`) %>%
  group_by(Year) %>%
  mutate(ymax = cumsum(Percentage)) 

#Calculating the y mins for the donut chart plotting.===
HBCUgenderenroll <- HBCUgenderenroll  %>%
  mutate(ymin = c(0, head(ymax, n=-1)))

#Calculating the label positions for the donut chart===
HBCUgenderenroll <- HBCUgenderenroll  %>%
  mutate(donutlabels = (ymax + ymin) / 2 )


# Making the donut gif====
donutgif <- ggplot(HBCUgenderenroll, aes(ymax=ymax, ymin=ymin, xmax=5.5, xmin=3, fill=Sex)) +
  scale_fill_manual(values = c("#a34986","#2396d9"))+
  geom_rect() +
  theme_void() +
  theme(plot.background = element_rect(fill ="#000000"),
        text = element_text(family = 'AvantGarde Bk BT'),
        axis.title=element_blank(),
        legend.position = "none")+
  coord_polar(theta="y") +
  xlim(c(0,6)) +
  geom_text(data=HBCUgenderenroll, 
            aes(x=0, y=.75, label=Year),
            fontface = "bold",
            size=13,
            family = "AvantGarde Bk BT",
            color = "#ffffff") +
  geom_text(data = HBCUgenderenroll,
            aes(x=4.2, y=donutlabels, label = paste0(Sex,":\n", percent(Percentage, accuracy = .1))),
            fontface = "bold",
            size = 3,
            color = "#ffffff")+
  transition_states(Year,
                    transition_length = 3,
                    state_length = 20,
                    wrap = TRUE)+
  ease_aes('circular-in-out')

#Saving the gif===
anim_save("donutgif.gif",donutgif)

#Reading in the gif===
donut <- image_read("donutgif.gif")


plotbase <- ggplot() +
  theme_void() +
  coord_cartesian(clip = "off")+
  xlim(0,15)+
  ylim(-40,15)+
  theme(plot.background = element_rect(fill ="#000000"),
        text = element_text(family = 'AvantGarde Bk BT'),
        axis.title=element_blank(),
        legend.position = "none",
        plot.title = element_textbox_simple(
          size = 23,
          color = "#ffffff",
          padding = margin(5,5,5,5),
          margin = margin(19,0,10,0),
          halign = .5,
          family = "The Bambank Script"),
        plot.subtitle = element_textbox_simple(
          size = 12,
          color = "#ffffff",
          padding = margin(5,5,5,5),
          margin = margin(20,0,20,0),
          halign = .5),
        plot.caption = element_textbox_simple(
          size = 12,
          color = "#ffffff",
          padding = margin(5,5,5,5),
          margin = margin(20,0,0,0),
          halign = 1))+
  labs(title = "<b> <span style = 'color:#a34986'>Battle</span> of the <span style = 'color:#2396d9'>Sexes</span></b>",
       subtitle = paste0('<i><b>"Female & Male Proportions of Black HBCU Enrollees Through The Years"</i></b><br><br>',"From<b> ",min(HBCUgenderenroll$Year)," </b>to<b> ", max(HBCUgenderenroll$Year),"</b> an increasing shift in the reported amount of HBCU enrollees that identified as <span style = 'color:#a34986'><b>Female</b></span> has been observed in data provided by <span style = 'color:#2396d9'><b>Data.World</b></span> and the  <span style = 'color:#2396d9'><b>National Center for Education Statistics (NCES)</b></span>. Previous literature suggests  that this gap may be due to males having a lower academic performance in high school as well as other environmental factors.[1,2]"), caption = "Data Source: Data.World/NCES | Created By: @meghansharris")+
  annotate(geom = "text", 
           label = "1) Gasman, M., Abiola, U., & Freeman, A. (2014). Gender Disparities at Historically Black Colleges and Universities.\n Higher Education Review, 47 (1), 56-76. Retrieved from http://repository.upenn.edu/gse_pubs/351 \n\n2) Richardson, S., Jones-Fosu, S., &amp; Lewis, C. W. (2019). Black Men are Present: Examining Enrollment Patterns\nin Education Degree Programs. Journal of African American Males in Education, 10(1), spring 2019, 20-36.",
           color = "#ffffff",
           fontface = "italic",
           size = 3,
           family = 'AvantGarde Bk BT',
           x = -Inf, 
           y = -Inf,
           hjust = 0,
           vjust = 0)


#Saving the plot===
ggsave("plotbase.png", dpi = 100,  width = 7 , height = 9.7, units = "in")

#Loading in the plot===
base <- image_read("plotbase.png")

# Making the final image====
#Overlaying the gif and plot===
finalframes <- image_composite(base, donut, operator = "blend", offset = "+0,+700", gravity = "center")

#Compiling into a gif===
HBCUgif <- image_animate(finalframes , fps = 10)

#Final write-out===
image_write(HBCUgif, "HBCU.gif")

#Sound to let me know it's done because this is taking too long and I have a migraine and need to lay down :p
beepr::beep(sound = "Time-Bomb.wav" )

