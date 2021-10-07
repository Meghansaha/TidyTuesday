#===== Tidy Tuesday - Plastic Pollution - 1-26-21 =====#

# Library Load-in====
`Required Packages` <- c("tidytuesdayR","knitr","ggplot2","tidyverse","extrafont","glue","ggtext","countrycode","textclean","scales") 
`New Packages` <- `Required Packages`[!(`Required Packages` %in% installed.packages()[,"Package"])]
if(length(`New Packages`)) install.packages(`New Packages`)
invisible(suppressPackageStartupMessages(suppressWarnings(lapply(`Required Packages`, require, character.only=T))))

#Font load-in====
windowsFont("AVGARDN")


# Data Load-in====
TTdata <- tidytuesdayR::tt_load('2021-01-26')
Plastics<- TTdata$plastics

# Data Carpentry====
#Thinking of just doing a viz of overall plastic collected by continent. Have an idea, not sure if it'll work.#

#Checking for NAs in the "grand_total", "Country", and volunteer columns===
sum(is.na(Plastics$grand_total))
sum(is.na(Plastics$country))
sum(is.na(Plastics$volunteers))

#Grand total has 14 NAs,  will coerce to zero, but checking the type first.#
class(Plastics$grand_total)
class(Plastics$volunteers)

#Numeric, so will mutate NAs in grand_total & volunteers to zero.#

#Filtering out what I need and condensing it down===
Continents <- Plastics %>%
  select(country,grand_total,volunteers) %>%
  group_by(country,volunteers) %>%
  mutate(grand_total = ifelse(is.na(grand_total),0,grand_total)) %>%
  mutate(volunteers = ifelse(is.na(volunteers),0,volunteers)) %>%
  summarise(Total = sum(grand_total)) %>%
  ungroup()

#Will see what countries don't have a match in the "codelist" dataset in the "countrycode" package===
mismatches <- Continents$country[!(Continents$country %in% codelist$country.name.en)]
mismatches <- mismatches[-3]
mismatchindex <- grep(paste(mismatches, collapse = "|"), Continents$country)
matchesref <- c("Côte d’Ivoire", "Ecuador", "Hong Kong SAR China", "North Korea", "Nigeria", "Taiwan", "United Kingdom", "United States","United States") 

#We have eight observations that don't match. Will clean it by replacing the country names with one that match in the "codelist" dataset===
Continents_cleaned <- Continents %>%
  filter(country !="EMPTY")

Continents_cleaned <- Continents_cleaned %>%
  mutate(country = mgsub(Continents_cleaned$country,mismatches,matchesref))%>%
  group_by(country) %>%
  ungroup()

#Making a lookup function for the continent designations and placing them into the set===
Continents_cleaned$Continent <- Continents_cleaned$country
continents <- setNames(as.character(codelist$continent),codelist$country.name.en)
Continents_cleaned$Continent <- as.character(lapply(Continents_cleaned$Continent , function(i) continents[i]))

#Condensing down to just continents===
Continents_final <- Continents_cleaned %>%
  group_by(Continent) 

#Want to see the frequency of continents. Those that are outliers will be removed===
table(Continents_final$Continent)

#Oceania only has two entries. Will remove it.#

#Saving the total with oceania before removal
Continents_woceania <- Continents_final %>%
  group_by(Continent) %>%
  mutate(`Overall Total` = cumsum(Total))%>%
  select(Continent,Total,volunteers) %>%
  group_by(Continent) %>%
  summarise_at(c("Total","volunteers"),sum)%>%
  arrange(Total)

#Filtering out Oceania, adding a cumsum column===
Continents_final <- Continents_final %>%
  filter(Continent != "Oceania") %>%
  group_by(Continent) %>%
  mutate(`Overall Total` = cumsum(Total)) 

#Getting the continent totals to determine factoring order===
Totalscheck <- Continents_final %>%
  select(Continent,Total,volunteers) %>%
  group_by(Continent) %>%
  summarise_at(c("Total","volunteers"),sum)%>%
  arrange(Total)

#Refactoring continents===
Continents_final$Continent <- factor(Continents_final$Continent, levels = Totalscheck$Continent)

# Making the Plot====
#Making a custom palette===
deepblues<- c("#c0edf0","#82d5e8","#54aad1","#10597a")

#Making coordinates for the labels and adding text===
contlabels <- data.frame(x=c(59900,143000,320000,480000),
                         y=c(.000013,.000005,.000004,.0000027),
                         text= c(paste0(Totalscheck$Continent[1],":",
                                        "\n",Totalscheck$volunteers[1]," Volunteers",
                                        "\n",Totalscheck$Total[1]," Plastics"),
                                 paste0(Totalscheck$Continent[2],":",
                                          "\n",Totalscheck$volunteers[2]," Volunteers",
                                          "\n",Totalscheck$Total[2]," Plastics"),
                                 paste0(Totalscheck$Continent[3],":",
                                        "\n",Totalscheck$volunteers[3]," Volunteers",
                                        "\n",Totalscheck$Total[3]," Plastics"),
                                 paste0(Totalscheck$Continent[4],":",
                                        "\n",Totalscheck$volunteers[4]," Volunteers",
                                        "\n",Totalscheck$Total[4]," Plastics")))

#Final Plot====
Plasticpollution <- ggplot(Continents_final, aes(x = `Overall Total`))+
  coord_cartesian(xlim = c(0,max(Continents_final$`Overall Total`)), ylim = c(-.0, .000015), expand = FALSE)+
  geom_density(aes(fill = Continent), alpha = .8, color = NA)+
  scale_fill_manual(values = deepblues)+
  theme_void()+
  theme(plot.background = element_rect(fill ="#323333"),
        text = element_text(family = 'AvantGarde Bk BT'),
        axis.title=element_blank(),
        legend.position = "none",
        plot.title = element_textbox_simple(
          size = 25,
          color = "#ffffff",
          padding = margin(5,5,5,5),
          margin = margin(0,0,10,0)),
        plot.subtitle = element_textbox_simple(
          size = 13,
          color = "#ffffff",
          padding = margin(5,5,5,5),
          margin = margin(0,0,0,0)))+
  labs(title = str_to_title("<b>Making waves</b> towards a future free from plastic pollution"),
       subtitle = paste0("In 2019 and 2020, ","<span style = 'color:#54aad1'><b>",length(unique(Plastics$country)),"</b></span>"," countries cleaned up an overall total of ","<span style = 'color:#54aad1'><b>",format(sum(Continents_woceania$Total),big.mark = ","),"</b></span>"," pieces of plastic. The","<span style = 'color:#54aad1'><b> Break Free From Plastic</span></b>"," movement is a global effort with the mission of envisioning a future free from plastic pollution [breakfreefromplastic.org]. Below are the 'waves' that shows the distribution of each <i>applicable<b> *</b></i> continent's efforts."))+
  annotate(geom = "text", 
           label = "* Entries from the Oceania region (Australia) were suppressed due to low counts.",
           color = "#ffffff",
           fontface = "italic",
           size = 3.5,
           family = 'AvantGarde Bk BT',
           x = 399900, 
           y = .0000148)+
  annotate(geom = "text", 
           label = "Data Source: Break Free From Plastic | Created By: @meghansharris",
           color = "#ffffff",
           fontface = "bold",
           size = 3.5,
           family = 'AvantGarde Bk BT',
           x = 415000, 
           y = .0000140)+
  layer(data = contlabels,
        geom = "label",
        stat = "identity",
        position = "identity",
        mapping = aes(x = x, 
                      y = y, 
                      label = text,
                      family =  'AvantGarde Bk BT'),
        params = list(color = "#ffffff",
                      label.r = unit(4, "mm"),
                      fill = "#282929"))

#Saving the ggplot====
#Saving the ggplot to an image for loading onto the visual
ggsave("plasticpollution.png", Plasticpollution, dpi = 100, width = 12 , height = 5.80, units = "in", bg = "transparent")

  

