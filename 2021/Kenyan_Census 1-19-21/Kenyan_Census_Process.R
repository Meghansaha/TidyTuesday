#===== Tidy Tuesday - Kenyan Census - 1-19-21 =====#

#Library load in====
`Required Packages` <- c("tidytuesdayR","knitr","ggplot2","tidyverse","extrafont","rKenyaCensus") 
`New Packages` <- `Required Packages`[!(`Required Packages` %in% installed.packages()[,"Package"])]
if(length(`New Packages`)) install.packages(`New Packages`)
invisible(suppressPackageStartupMessages(suppressWarnings(lapply(`Required Packages`, require, character.only=T))))

#Data load in====
ttdata <- tidytuesdayR::tt_load('2021-01-19')
gender <- ttdata$gender
crops <- ttdata$crops
agriculture <- V4_T2.20
othercrops <- V4_T2.22

#Data Cleaning====

#Want to combine the four datasets by matching on "County" Will need to do some wrangling for this.
#Fixing the capitalization of counties in crops and agriculture datasets.

#crops set
crops <- crops %>%
  mutate(County = str_to_title(SubCounty)) %>%
  select(-SubCounty) %>%
  group_by(County) %>%
  arrange(County)

#agriculture and othercrops sets
agriculture <- agriculture %>%
  mutate(County = str_to_title(County)) %>%
  mutate(County = ifelse(County == "Xxx", "Kenya",County)) %>%
  select(-SubCounty)

othercrops <- othercrops %>%
  mutate(County = str_to_title(County)) %>%
  mutate(County = ifelse(County == "Xxx", "Kenya",County)) %>%
  select(-SubCounty)

gender <- gender %>%
  mutate(County = str_to_title(County)) %>%
  mutate(County = ifelse(County == "Total", "Kenya", County)) %>%
  group_by(County) %>%
  arrange(County)

#The agriculture set needs to be summarized down to overall county
agriculture <- agriculture %>%
  select(-AdminArea) %>%
  group_by(County) %>%
  arrange(County) %>%
  summarise(across(everything(), list(sum)))

#Cleaning the names
names(agriculture) <- str_replace(names(agriculture),"_1","")

#The "other crops" set nee to be summarized down to overall county
othercrops <- othercrops %>%
  select(-AdminArea) %>%
  group_by(County) %>%
  arrange(County) %>%
  summarise(across(everything(), list(sum)))

#Cleaning the names
names(othercrops) <- str_replace(names(othercrops),"_1","")

# Checking to see if all counties are the same across the datasets.
#agriculture and crops county check
countycheck <- cbind.data.frame(agriculture$County,crops$County,gender$County,othercrops$County)
names(countycheck) <- c("agriculture","crops","gender","othercrops")
countycheck[which(countycheck[,1] != countycheck[,2:4]),]

#We see that "Nairobi City" is different in the "crops" dataset, we can change that and check the counties again just to be sure.
crops$County[crops$County == "Nairobi"] <- "Nairobi City"

#agriculture and crops county re-check
countycheck <- cbind.data.frame(agriculture$County,crops$County,gender$County,othercrops$County)
names(countycheck) <- c("agriculture","crops","gender","othercrops")
countycheck[which(countycheck[,1] != countycheck[,2:4]),]

#It all checks out! Now we can merge the datasets.

#Data Merge====
masterset1 <- left_join(agriculture,crops,by = "County")
masterset2 <- left_join(gender,othercrops, by = "County")
masterset <- left_join(masterset1,masterset2, by = "County")
rm(masterset1,masterset2)


#Labeling each county as "predominately Male or Female"
mastersetMF <- masterset %>%
  mutate(Gendermajority = ifelse(Female > Male, "Predominately Female", "Predominately Male")) %>%
  select(-County)

#Setting all NAs to 0
mastersetMF[is.na(mastersetMF)] <- 0

#Collapsing into gender groups
mastersetMF <- mastersetMF %>%
  group_by(Gendermajority) %>%
  summarise(across(everything(), list(sum)))
  
#Cleaning the names
names(mastersetMF) <- str_replace(names(mastersetMF),"_1","")

#Just had the idea to do a food pyramid! Will take out the columns not needed.
