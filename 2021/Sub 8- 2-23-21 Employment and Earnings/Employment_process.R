#===== Tidy Tuesday - Employment and Earnings - 2-23-21 =====#

# Loading necessary packages====
pacman::p_load("tidytuesdayR",
               "ggplot2",
               "tidyverse",
               "ggrepel",
               "imager",
               "ggtext",
               "extrafont",
               "glue")

# Data Load-in====
TTdata <- tt_load(2021, week = 9)
Employment <- TTdata$employed

# Light Data Wrangling====
#Fixing naming inconsistency in the "minor occupation" category, only selecting "black" observations, and collapsing into summaries by year, race, and occupation. Removing the race category, grouping by year, then arrange the dataset by year and total tmployment numbers===

Black_employment <- Employment %>%
  filter(race_gender == "Black or African American") %>%
  mutate(minor_occupation = ifelse(minor_occupation == "Manage-ment, business, and financial operations occupations", "Management, business, and financial operations occupations", minor_occupation)) %>%
  group_by(year,minor_occupation,race_gender) %>%
  summarise(employ_n = sum(employ_n, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-race_gender) %>%
  group_by(year) %>%
  arrange(year,employ_n)

# Want to show the most recent (2020) top-five occupations for black people. Need to calculate that.#

#Seeing what the top five are. Can do this based on the arrangements in the last call===
Top_five <- tail(Black_employment$minor_occupation,5)

#Filtering the data to only include the "top five" occupation categories and transposing into "wide" format for the plot===
Top_five_employment <- Black_employment %>%
  filter(minor_occupation %in% Top_five) %>%
  mutate(minor_occupation = str_to_title(str_wrap(minor_occupation, width = 35)))

# Adding custom color palette====
custom_color <- c("#010221", "#092f76", "#5ed2fb", "#ffffff", "#f04ac8") 
names(custom_color) = unique(Top_five_employment$minor_occupation)

# Loading in fonts====
loadfonts(quiet = TRUE)

# Creating a parallel coordinate map possibly. Let's see====
ggplot(data = Top_five_employment,aes(x = year, y = employ_n), color = minor_occupation)+
  scale_x_continuous(position = "top", limits = c(2013,2022), breaks=c(2015:2020))+
  theme_minimal()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#8a307e", color = NA),
        panel.background = element_rect(fill = "#8a307e", color = NA),
        axis.title = element_blank(),
        axis.text.x.top=element_text(family = "Secret Winter", vjust=0.5, size = 20, color = "#ffffff"),
        axis.text.y.left = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Secret Winter", color = "#ffffff", size = 25, hjust = .5, face = "bold"),
        plot.subtitle = element_textbox_simple(
          family = "Caviar Dreams",
          size = 14,
          color = "#ffffff",
          padding = margin(0,5,5,5),
          margin = margin(30,0,0,0),
          halign = 0),
        plot.caption = element_textbox_simple(
          family = "Caviar Dreams",
          size = 12,
          color = "#ffffff",
          padding = margin(5,5,5,5),
          margin = margin(20,0,0,0),
          halign = 1) )+
  scale_linetype_manual(values=c("dotted","dotted","solid","dotted","dotted"))+
  scale_color_manual(values = custom_color)+
  geom_line(aes(color = minor_occupation, linetype = minor_occupation),size = 1.5)+
  geom_vline(xintercept = c(2015:2020), size = 2.5, color = "#333333") +
  geom_label_repel(data = Top_five_employment %>% 
                     filter(year == 2015), 
                   aes(label = paste0(minor_occupation,"\n\n(",scales::comma(employ_n),")")) , 
                   hjust = "center", 
                   fontface = "bold", 
                   size = 3, 
                   nudge_x = -2, 
                   direction = "y",
                   color = "#ffffff",
                   fill = "#333333",
                   family = "Caviar Dreams")+
  geom_label_repel(data = Top_five_employment %>% 
                     filter(year == 2020), 
                   aes(label = paste0(minor_occupation,"\n\n(",scales::comma(employ_n),")")) , 
                   hjust = "center", 
                   fontface = "bold", 
                   size = 3, 
                   nudge_x = 2, 
                   direction = "y",
                   color = "#ffffff",
                   fill = "#333333",
                   family = "Caviar Dreams") +  
  labs(title = "Work x 6",
       subtitle = "<br>Each line represents one of the top-five occupational industries held by Black people in the United States from 2015 to 2020. The most populated work industry in 2020 was <b>Professional and Related Occupations</b>. The<b><span style = 'color:#010221'> Transportation and Material Moving</span></b> industry saw the largest net increase over the six years <b><span style = 'color:#010221'>(+765,000)</span></b>, while the<b><span style = 'color:#5ed2fb'> Office and Administrative Support</span></b> industry saw the largest net decrease <b><span style = 'color:#5ed2fb'>(-516,000)</span></b> <br><br><br>",
       caption = "Data Source: U.S Bureau of Labor Statistics <br> Created By: <img src='https://image.flaticon.com/icons/png/512/23/23931.png', width ='12', height = '12'> @meghansharris")

# Saving the image out====
ggsave("work_employment.png", height = 9.3, width = 12.5, units = "in", dpi = 100, bg = "transparent")
  
  




