#===== Tidy Tuesday - Makeup Shades - 3-29-21 =====#
#Sources: Fitzpatrick Scale Chart: https://dermahealthinstitute.com/blog/the-fitzpatrick-scale/
        # Lightness calculation: https://www.rapidtables.com/convert/color/rgb-to-hsl.html
        # Takota custom font: https://www.dafont.com/takota.font
        # Geo Sans Light custom font: https://www.dafont.com/geo-sans-light.font


# Loading necessary packages====
pacman::p_load("tidytuesdayR",
               "tidyverse",
               "ggtext",
               "extrafont",
               "glue",
               "forcats")

# Data Load-in====
TTdata <- tt_load(2021, week = 14)
All_Categories <- TTdata$allCategories

#Want to compare categories of names across shades. Need to do some wrangling.
#Dropping columns that aren't needed====
All_Categories_mod <- All_Categories %>%
  select(name,categories,lightness,hex)

#Need to get all unique category names to throw into a tally.
All_Categories_mod <- All_Categories_mod %>%
  group_by(lightness,hex) %>%
  separate_rows(categories, sep = ", ") %>%
  count(categories, sort = TRUE, name = "Tally") %>%
  arrange(lightness) #needs to be sorted to pull quantile hex colors for later.

#Want to create groups for shades. Will use the fitzpatrick scale====
FS_scale <- data.frame("FS_Type" = c("Type 1","Type 2","Type 3","Type 4", "Type 5", "Type 6"),
                       "Red" = c(240,234,213,187,125,46),
                       "Green" = c(211,202,172,116,78,29 ),
                       "Blue" = c(176,146,115,82,55,19 ))

#Using the rgbs from the scale, can create hex colors then can calculate lightness for type classification.
FS_scale_mod <- FS_scale %>%
  rowwise() %>%
  mutate(Hex = rgb(Red,Green,Blue, maxColorValue = 255)) %>%
  mutate(across(c(Red,Green,Blue), function(x) x/255)) %>%
  mutate(cmax = max(Red,Green,Blue)) %>%
  mutate(cmin = min(Red,Green,Blue)) %>%
  mutate(Lightness = (cmax + cmin)/2)

#Getting a named vector of skin type categories.
Skin_Types <- unlist(FS_scale_mod$Lightness)
names(Skin_Types) <- FS_scale$FS_Type

#Using calculated Lightness to determine fitzpatrick scale type for each shade
All_Categories_mod <- All_Categories_mod %>%
  mutate(shade_category = case_when(
    lightness >= Skin_Types["Type 1"] ~ "Type 1",
    lightness < Skin_Types["Type 1"] & lightness >= Skin_Types["Type 2"] ~ "Type 2",
    lightness < Skin_Types["Type 2"] & lightness >= Skin_Types["Type 3"] ~ "Type 3",
    lightness < Skin_Types["Type 3"] & lightness >= Skin_Types["Type 4"] ~ "Type 4",
    lightness < Skin_Types["Type 4"] & lightness >= Skin_Types["Type 5"] ~ "Type 5",
    lightness < Skin_Types["Type 5"] ~ "Type 6"))

#Summarizing categories by skin type
Summary_Categories <- All_Categories_mod %>%
  group_by(shade_category,categories) %>%
  summarise(total = sum(Tally)) %>%
  arrange(shade_category,desc(total)) %>%
  ungroup(categories) %>%
  mutate(percentage = total/sum(total)) %>%
  slice_head(n=4)

#Pulling the"descriptor" values
descriptor_values <- `names<-`(Summary_Categories$percentage[which(Summary_Categories$categories == "descriptor")],unique(Summary_Categories$shade_category))

#Removing the descriptor values for a cleaner graph and adding the Type hex colors
Summary_Categories_Final <- Summary_Categories %>%
  filter(categories != "descriptor") %>%
  mutate(hex = shade_category)

#Lookup function for the hex colors
hexlookup <- setNames(as.character(FS_scale_mod$Hex),FS_scale_mod$FS_Type)
Summary_Categories_Final$hex <- as.character(lapply(Summary_Categories_Final$hex, function(i) hexlookup[i]))

# Special Font Load-in====
fontpath <- readRDS('fontpath.RDS')
font_import(path = fontpath)
loadfonts(device = "win")

# Creating a basic plot with words in place of data====
final_plot <- Summary_Categories_Final %>%
  ggplot(aes(x = percentage, y = fct_rev(shade_category))) +
  scale_color_manual(values = c("#e9e9e9","#f7d3e4","#e7a0ae","#c76982", "#a6787a", "#81625f")) +
  geom_segment(aes(x = -Inf, xend=Inf, yend = fct_rev(shade_category), color = shade_category),
               size = 30)+
  coord_cartesian(clip = "off")+
  scale_x_reverse(breaks = seq(.235,.068, by =-.02), 
                  limits = c(.235,.068),
                  labels = scales::percent) +
  geom_text(data = Summary_Categories_Final, 
               label = Summary_Categories_Final$categories,
               fontface = "bold",
               family = "Takota",
               size = (Summary_Categories_Final$percentage * 100),
               position = position_dodge(width = .5),
               color = "#2f1412"
               ) +
  theme_minimal()+
  theme(plot.title = element_textbox_simple(family = "GeosansLight", 
                                            color = "#2f1412", 
                                            size = 50, 
                                            halign = .5, 
                                            face = "bold"),
        plot.subtitle = element_textbox_simple(
          family = "GeosansLight",
          size = 20,
          color = "#2f1412",
          padding = margin(0,5,5,5),
          margin = margin(5,0,20,0),
          halign = 0),
        plot.caption = element_textbox_simple(
          family = "GeosansLight",
          size = 18,
          color = "#2f1412",
          padding = margin(5,5,5,5),
          margin = margin(20,0,0,0),
          halign = 1),
        axis.text.x.bottom =element_text(family = "GeosansLight",
                                 face = "bold",
                                 size = 15,
                                 color = "#2f1412"),
        axis.text.y.left =element_text(family = "GeosansLight",
                                         face = "bold",
                                         size = 30,
                                       color = "#2f1412",
                                       margin = margin(t = 0, r = 50, b = 0, l = 0)),
        axis.title.y.left =  element_text(family = "GeosansLight",
                                            face = "bold.italic",
                                            size = 15,
                                          color = "#2f1412",
                                          margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x.bottom =  element_text(family = "GeosansLight",
                                            face = "bold.italic",
                                            size = 15,
                                            color = "#2f1412",
                                            margin = margin(t = 20, r = 0, b = 0, l = 0)),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = "#dec5c1", color = NA)) +
  geom_point(aes(x = Inf, y = fct_rev(shade_category)), color = Summary_Categories_Final$hex, size = 50)+
  xlab("Percentage of Top Four Makeup Shade Categories*")+
  ylab("Fitzpatrick Scale Skin Types") +
  labs(title =  paste0("What's in a <b><span style = font-family:'Takota'>Name?</span></b>"),
       subtitle = paste0("This data from <b>The Pudding</b> shows the categories of makeup shades that have been reported by skin type. The most reported category of shade name for every skin type was 'descriptor' which makes up at least ", scales::percent(min(descriptor_values))," of each skin type category. 'Descriptor' names are those that describe characteristics of the shade like 'deep' or 'light'. The three most reported categories <i>after</i> 'descriptor' are shown.<br><br> The most popular categories for lighter skin types are <b><span style = font-family:'Takota'>Color</span></b>, <b><span style = font-family:'Takota'>Gem</span></b> and <b><span style = font-family:'Takota'>'Miscellaneous'.</span></b> As the skin type gets darker, the introduction of categories like <b><span style = font-family:'Takota'>food</span></b>, <b><span style = font-family:'Takota'>drink</span></b>, and even <b><span style = font-family:'Takota'>wood</span></b> are introduced."),
       caption = "Data Source- The Pudding <br> Created By- <img src='https://image.flaticon.com/icons/png/512/23/23931.png', width ='12', height = '12'> @meghansharris<br><span style = 'font-size: 12px'><i> *The 'descriptor' category has been omitted from the visual for clarity</i></span>")
  
#Saving the final plot out====
ggsave("makeup_final.png", height = 11.46, width = 19.75, units = "in", dpi = 90)
  
  
