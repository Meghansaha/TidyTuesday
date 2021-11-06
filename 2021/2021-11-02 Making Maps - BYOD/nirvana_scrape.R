#Nirvana Concert Web Scraping====

# Library Load-in====
library(rvest) # For scraping the wikipedia tables with concert info
library(tidyverse) # For everything data
library(tidygeocoder) # For getting geocodes of the cities
library(xml2) # For pulling out table headers from the site's HTML code
library(janitor) # For cleaning up some names

# Web Scraping and Initial Data collection/wrangling====
# Pulling the wikipedia site that will be scraped for the data====
concert_url <- "https://en.wikipedia.org/wiki/List_of_Nirvana_concerts"

# Converting to HTML data====
concert_site <- read_html(concert_url)

xml_find_all(concert_site,".//s") %>%
  xml_add_sibling("p","cancelled") 


# Pulling all the tables on the pages====
page_tables <- html_elements(concert_site, "table")


# Grabbing the section headers on the page (most are the table "headers")====
table_headers <- html_elements(concert_site, "span[class= 'mw-headline']") %>% 
  html_text() 

# Externally scanned which tables should be discarded, placing their indexes here====
non_table_titles <- c(1,9:10)

# Removing table headers that aren't associated with concert data. Also cleaning the names generally====
cleaned_table_headers <- make_clean_names(table_headers[-non_table_titles])

# Converting the "held" concerts into a list of dataframes====
concert_tables <- lapply(page_tables, function(x) html_table(x)) 

# Removing externally scanned tables that are not filled with concert data. Last two tables on the page====
concert_tables <- head(concert_tables,7)

# One final strucutral clean and adding a category variable===
for(i in seq_along(concert_tables)){
  concert_tables[[i]] <- concert_tables[[i]] %>%
    filter(! Date == City) %>%
    select(Date:Country) %>%
    mutate(Category = cleaned_table_headers[i])
}

# Finally, condensing all tables down into one table====
all_concerts <- bind_rows(concert_tables)

# Removing cancelled concerts and footnote notations====
all_concerts_clean <- all_concerts %>%
  filter(!grepl("cancelled",Date)) %>%
  mutate(across(everything(), function(x) str_replace_all(x,regex("\\[.\\]"),"")))

# I've been having a terrible week. regex is not about to make it worse...
all_concerts_clean$Date[nrow(all_concerts_clean)] <- "March 1, 1994"

# Geocoding cities and countries====
all_concerts_geo <- all_concerts_clean %>%
  mutate(City = if_else(grepl("(?i)(San|Los|Fort|Salt|Las|Daly|Gold|Buenos|São|Saint|New|Iowa|Ann|Long|West|Dún|St.)",City),City, word(City,1)),
         City = str_replace_all(City,",",""),
         Country = if_else(str_count(Country, '\\w+') > 1 & !Country %in% c("United States","New Zealand", "Republic of Ireland"),word(Country,2),Country),
         Country = if_else(Country %in% c("England","Scotland","Ireland"),"United Kingdom",Country),
         Country = if_else(Country == "Republic of Ireland","Ireland",Country)) 

# Writing it out for use in the process scripts. Will manually add states in Excel
write_csv(all_concerts_geo,"Nirvana Concerts Scraped- No States.csv")