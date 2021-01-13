#===== Tidy Tuesday - Art Collections - 1-12-21 =====#

#Library load in====
`Required Packages` <- c("tidytuesdayR","knitr","ggplot2","tidyverse","extrafont") 
`New Packages` <- `Required Packages`[!(`Required Packages` %in% installed.packages()[,"Package"])]
if(length(`New Packages`)) install.packages(`New Packages`)
invisible(suppressPackageStartupMessages(suppressWarnings(lapply(`Required Packages`, require, character.only=T))))

#Font load-in====
loadfonts(device = "win", quiet = TRUE)
windowsFonts("TREBUC" = windowsFont("TREBUC"))

#Data load in====
ACdata <- tt_load('2021-01-12')
Artwork <- ACdata$artwork
Artists <- ACdata$artists

#After scanning data, will attempt to examine different categories of medium used throughout the years.#

#Data Carpentry====
#Isolating the data we need for artists from the United States.#
Mediumdata <- Artwork %>%
  select(medium, year, artist) %>%
  mutate(artistorigin = artist)

#Creating a lookup function to pull birthplace data into mediumdata by artist name.#
origin <- setNames(as.character(Artists$placeOfBirth),Artists$name)
Mediumdata$artistorigin <- as.character(lapply(Mediumdata$artistorigin, function(i) origin[i]))

#Filtering out missing data.#
Mediumdata <- na.omit(Mediumdata)

#Selecting observations only from the united states.#
USmediumdata <- Mediumdata %>%
  filter(grepl("United states",artistorigin, ignore.case = TRUE))

#Seeing the oldest and newest year in the dataset.#
range(USmediumdata$year)

# a histogram to look at distribution of dates.#
hist(USmediumdata$year)

#Choosing to filter out datesto have a "through the decades" perspective starting from the 1900's.#
USmediumdata <- USmediumdata %>%
  filter(year >= 1900)

# a histogram to look at distribution of dates.#
hist(USmediumdata$year)

#Creating a frequency table to see the most frequent medium types.#
MediumFreq <- as.data.frame(table(USmediumdata$medium))

#Only keeping mediums that are used more than the average times in this set. Using this to get a better picture of most frequently reported medium types#
MediumFreq <- MediumFreq %>%
  filter(Freq >= mean(Freq))

#Using info above, will create a new category variable that attempts to collapse the medium variable.#

#Setting up a reference.#
Popularmediums <- c("Lithograph","Screenprint","Etching","Photograph","Aquatint","Oil Paint","Acrylic Paint","Wood","Intaglio", "Vinyl","Glass","Metal","Relief", "Steel","Bronze","Graphite","Mezzotint","Gouache","Pastel","Watercolour","Charcoal","Linocut","Ink","Drypoint","Plastic","Video","Film","Performance","Granite","Marble","Digital")

USpopularmediums <- as.data.frame(matrix(ncol = length(Popularmediums), nrow = nrow(USmediumdata)))
names(USpopularmediums) <- Popularmediums

#Getting tallies of all reported "Popular" Mediums
for (i in seq_along(Popularmediums)){
  USpopularmediums[Popularmediums[i]] = ifelse(grepl(Popularmediums[i],USmediumdata$medium, ignore.case = TRUE),1,0)
}

#Creating an "Other category for those that don't fall into the ones I've set.#
USpopularmediums <- USpopularmediums %>%
  mutate(Other =rowSums(USpopularmediums)) %>%
  mutate(Other = ifelse(Other == 0,1,0))

#Binding the counts back on to the original dataset.#
USmediumdata <- cbind(USmediumdata,USpopularmediums)

#Adding a "Decade" column.#
USmediumdata <- USmediumdata %>%
  mutate("Decade" = ifelse(year <= 1909,"1900's",
         ifelse(year > 1909 & year < 1920, "1910's",
         ifelse(year > 1919 & year < 1930, "1920's",
         ifelse(year > 1929 & year < 1940, "1930's",
         ifelse(year > 1939 & year < 1950, "1940's",
         ifelse(year > 1949 & year < 1960, "1950's",
         ifelse(year > 1959 & year < 1970, "1960's",
         ifelse(year > 1969 & year < 1980, "1970's",
         ifelse(year > 1979 & year < 1990, "1980's",
         ifelse(year > 1989 & year < 2000, "1990's",
         ifelse(year > 1999 & year < 2010, "2000's",
         ifelse(year > 2000 & year < 2020, "2010's", "ERROR")))))))))))))

#Removing columns we dont need.#
USmediumdata <- USmediumdata %>%
  select(-c(medium:artistorigin))

# Grouping the data set by "decade", Transposing the data into a "long format",  then collapsing the sum of these tallies by color with the summarise function. Lastly, getting the max medium/method to see the decade's most popular type====
USmediumdata <- USmediumdata %>% 
  pivot_longer(-Decade, names_to = "Medium/Method", values_to = "Total") %>%
  group_by(Decade,`Medium/Method`) %>%
  summarise(Total = sum(Total)) %>%
  group_by(Decade) %>%
  filter(Total == max(Total))

#Removing the 2010's as it's numbers are minimal, and combining some decades with more than one max.#
USmediumdata <- USmediumdata %>%
  filter(Decade != "2010's") %>%
  mutate(`Medium/Method` = ifelse(Decade == "1920's", "Bronze and Photographs", 
                                  ifelse(Decade == "1930's", "Gouache, Oil Paint, Watercolour", `Medium/Method`))) %>%
  group_by(Decade,`Medium/Method`) %>%
  summarise(Total = sum(Total))
  
# Adding y axis lengths for geom_segments.#
USmediumdata$ysegmentpos <- c(1,-1,1.5,-1.5,1,-1,1.5,-1.5,1,-1,1.5)

#Setting x axis positions for each decade.#
Decadestart <- seq(from = 1905, to = 2005, by = 10)

#Applying years to the set.#
USmediumdata$Year <- Decadestart

#Setting color values for the plot.#
Colors <- rep(c("#1c1c1c","#ffffff"),7)

# Building the plot====
ggplot(USmediumdata,aes(x=Year,y=0, col=Decade, label=Decade))+
  geom_hline(yintercept=0,color = "#333333", size=5) +
  geom_segment(USmediumdata, mapping = aes(y=ysegmentpos,yend=0,xend=Year, size = 3)) +
  scale_color_manual(values = Colors)+
  geom_label(aes(label = Decade, color = ifelse(Decade %in% c("1900's","1920's","1940's","1960's","1980's","2000's"), "#ffffff","#1c1c1c")),
             family = "TREBUC",
             fontface = "bold",
             stat = "identity",
             position = "identity",
             label.padding = unit(.2, "lines"),
             label.r = unit(0, "lines"),
             label.size = 1,
             inherit.aes = TRUE,
             fill = ifelse(USmediumdata$Decade %in% c("1900's","1920's","1940's","1960's","1980's","2000's"),"#1c1c1c", "#ffffff")) +
  theme_void()+
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "none") 