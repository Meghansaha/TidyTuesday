#Tidy Tuesday Work - January 5th :Transit Costs Project#

#Library load in====
`Required Packages` <- c("tidytuesdayR","knitr","ggplot2","tidyverse","countrycode","extrafont","plotly") 
`New Packages` <- `Required Packages`[!(`Required Packages` %in% installed.packages()[,"Package"])]
if(length(`New Packages`)) install.packages(`New Packages`)
invisible(suppressPackageStartupMessages(suppressWarnings(lapply(`Required Packages`, require, character.only=T))))

#Data load in====
TCdata <- (tidytuesdayR::tt_load('2021-01-05'))
transit_cost <- TCdata$transit_cost

# Font load-in====
loadfonts(device = "win", quiet = TRUE)

#After reviewing the data, interested in visualizing average overall cost per year of construction by country. Need to start some basic cleaning and carpentry to make this happen.#

#Data Cleaning and Carpentry====
#Isolating the metrics we want.#
transit_cost2 <- transit_cost %>%
  select(country,start_year,end_year,real_cost)

#Purposely coercing NAs to catch potential messy data inputs.#
suppressWarnings(transit_cost2$start_year <- as.numeric(transit_cost2$start_year))
suppressWarnings(transit_cost2$end_year <- as.numeric(transit_cost2$end_year))
suppressWarnings(transit_cost2$real_cost <- as.numeric(transit_cost2$real_cost))

#Removing observations that do not have a reported start or end year.#
#Creating a variable that gives the calculation of time spent during entire construction.#
#Creating a variable that gives the average calculation of money spent during entire construction.#
transit_cost2 <- transit_cost2 %>%
  filter(!is.na(start_year)) %>% 
  filter(!is.na(end_year)) %>% 
  mutate(constr_duration = end_year - start_year) %>%
  mutate(constr_costavg = real_cost/constr_duration)

#Calculating how many observations were removed from the original set.#
TCdiff <- nrow(transit_cost) - nrow(transit_cost2)

#Pulling country names with the "countrycode" package/dataset.#
#Merging found country names into the data set#
countryshortcodes <- transit_cost2$country
country <- setNames(as.character(codelist$country.name.en),codelist$ecb)
transit_cost2$country <- as.character(lapply(transit_cost2$country, function(i) country[i]))

#Creating a catch to find country codes that may not match.#
transit_cost2 <- transit_cost2 %>%
  mutate(country = ifelse(is.na(transit_cost2$country),countryshortcodes,transit_cost2$country))

#Looking at unique values lets us know "UK" hasn't caught a country Match, we can change this ourselves.#
unique(transit_cost2$country[(nchar(transit_cost2$country) == 2)])

#Mutating the country column for the UK codes.#
transit_cost2 <- transit_cost2 %>%
  mutate(country = ifelse(country == "UK","United Kingdom",country))

#Had the idea to stratify data by continent (fact wrapping).# - Will revisit at a later date and add that graph!
#Pulling continent names with the "countrycode" package/dataset.#
#Merging found continent names into the data set#
transit_cost2$continent <- transit_cost2$country
continent <- setNames(as.character(codelist$continent),codelist$country.name.en)
transit_cost2$continent <- as.character(lapply(transit_cost2$continent , function(i) continent[i]))

#Grouping and summarizing to get one tabulated row per country. Had the idea to stratify by continent, but abandoned it due to time.#
transit_cost2 <- transit_cost2 %>% 
  group_by(continent,country) %>% 
  summarise_at(vars(constr_duration,constr_costavg),list(mean = mean)) 

#Resetting names of the data set.#
names(transit_cost2) <- c("Continent","Country","Average Duration","Average Cost (in Millions)")

#Calculating mean for the averaged duration.#
avgduration <- mean(transit_cost2$`Average Duration`)

#Creating a category for above average and below average construction duration.#
transit_cost2 <- transit_cost2 %>%
  mutate(`Construction Duration` = ifelse(`Average Duration` >= avgduration,"Above Average Construction Duration","Below Average Construction Duration"))

#Reordering the countries around for the ggplot.#
transit_cost2$Country <- fct_rev(transit_cost2$Country)

#Creating the plot====
transitavgchart <- transit_cost2 %>%
  ggplot(aes(y = Country, x = `Average Cost (in Millions)`, color = `Construction Duration`)) +
  theme_dark()+
  geom_segment(aes(x = `Average Cost (in Millions)`, xend = 0, y = Country, yend =Country),color = "#333333") +
  geom_point()+
  scale_color_manual(values = c("#a8325c","#34ccd1")) +
  labs(title= "Average Cost of Transit Lines Per Millions (USD) in Different Countries", subtitle = paste0("The calculated average construction duration for this particular data set was ",format(avgduration, digits = 1)," years."))+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(family = "Tahoma", size=16, face = "bold", color = "#2e2e2e"),
        plot.subtitle = element_text(family = "Tahoma", size=12, color = "#1f1e1e"),
        legend.position = "right",
        legend.title =  element_text(family = "Tahoma", size=12, color = "#1f1e1e", face = "bold"),
        axis.text.x.bottom = element_text(family = "Tahoma", size=13, color = "#000000", hjust = 0.5),
        plot.margin = unit(c(0, 4, 0, 0), "lines"))

transitavgplotly <- transit_cost2 %>%
  ggplot(aes(y = Country, x = `Average Cost (in Millions)`, color = `Construction Duration`)) +
  theme_dark()+
  geom_segment(aes(x = `Average Cost (in Millions)`, xend = 0, y = Country, yend =Country),color = "#333333") +
  geom_point()+
  scale_color_manual(values = c("#a8325c","#34ccd1")) +
  labs(title= "Average Cost of Transit Lines Per Millions (USD) in Different Countries", subtitle = paste0("The calculated average construction duration for this particular data set was ",format(avgduration, digits = 1)," years."))+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(family = "Tahoma", size=16, face = "bold", color = "#2e2e2e"),
        plot.subtitle = element_text(family = "Tahoma", size=12, color = "#1f1e1e"),
        legend.position = "right",
        legend.title =  element_blank(),
        axis.text.x.bottom = element_text(family = "Tahoma", size=13, color = "#000000", hjust = 0.5),
        plot.margin = unit(c(0, 4, 0, 0), "lines"))

#Building plotly plot.#
transitavgplotly <- ggplotly(transitavgplotly, height = 830, width = 740) %>%
  layout(title= list(x = 0, y=.97, text = paste0("<b>","Average Cost of Transit Lines Per Millions (USD) in \nDifferent Countries","</b><br>","<sup>", "The calculated average construction duration for this particular data set was ",format(avgduration, digits = 1)," years.","</sup><br>", " <br><br>")), barmode = "group", yaxis = list(title = ""), xaxis = list(title = "Average Cost (in Millions)"), margin = list(t = 80), legend = list(x = 0.44, y = 0.98))