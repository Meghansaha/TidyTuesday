#===== Tidy Tuesday - Kenyan Census - 1-19-21 =====#

#Library load in====
`Required Packages` <- c("tidytuesdayR","knitr","ggplot2","tidyverse","extrafont","rKenyaCensus","sp") 
`New Packages` <- `Required Packages`[!(`Required Packages` %in% installed.packages()[,"Package"])]
if(length(`New Packages`)) install.packages(`New Packages`)
invisible(suppressPackageStartupMessages(suppressWarnings(lapply(`Required Packages`, require, character.only=T))))

#Data load in====
ttdata <- tidytuesdayR::tt_load('2021-01-19')
gender <- ttdata$gender
crops <- ttdata$crops
agriculture <- V4_T2.20
othercrops <- V4_T2.22
livestock <- V4_T2.23

#Data Cleaning====

#Want to combine the four datasets by matching on "County" Will need to do some wrangling for this.
#Fixing the capitalization of counties in crops and agriculture datasets.

#Crops set
crops <- crops %>%
  mutate(County = str_to_title(SubCounty)) %>%
  select(-SubCounty) %>%
  group_by(County) %>%
  arrange(County)

#Agriculture, othercrops, livestock sets (Need to be adjusted for the sub county breakdowns and text capitializations)
agriculture <- agriculture %>%
  mutate(County = str_to_title(County)) %>%
  mutate(County = ifelse(County == "Xxx", "Kenya",County)) %>%
  select(-SubCounty)

othercrops <- othercrops %>%
  mutate(County = str_to_title(County)) %>%
  mutate(County = ifelse(County == "Xxx", "Kenya",County)) %>%
  select(-SubCounty)

livestock <- livestock %>%
  mutate(County = str_to_title(County)) %>%
  mutate(County = ifelse(County == "Xxx", "Kenya",County)) %>%
  select(-SubCounty)

#Gender set
gender <- gender %>%
  mutate(County = str_to_title(County)) %>%
  mutate(County = ifelse(County == "Total", "Kenya", County)) %>%
  group_by(County) %>%
  arrange(County)

#The "agriculture" set needs to be summarized down to overall county
agriculture <- agriculture %>%
  select(-AdminArea) %>%
  group_by(County) %>%
  arrange(County) %>%
  summarise(across(everything(), list(sum)))

#Cleaning the names
names(agriculture) <- str_replace(names(agriculture),"_1","")

#The "other crops" set needs to be summarized down to overall county
othercrops <- othercrops %>%
  select(-AdminArea) %>%
  group_by(County) %>%
  arrange(County) %>%
  summarise(across(everything(), list(sum)))

#Cleaning the names
names(othercrops) <- str_replace(names(othercrops),"_1","")

#The "livestock" set needs to be summarized down to overall county
livestock <- livestock %>%
  select(-AdminArea) %>%
  group_by(County) %>%
  arrange(County) %>%
  summarise(across(everything(), list(sum)))

#Cleaning the names
names(livestock) <- str_replace(names(livestock),"_1","")


# Checking to see if all counties are the same across the datasets.
#agriculture and crops county check
countycheck <- cbind.data.frame(agriculture$County,crops$County,gender$County,othercrops$County,livestock$County)
names(countycheck) <- c("agriculture","crops","gender","othercrops","livestock")
countycheck[which(countycheck[,1] != countycheck[,2:5]),]

#We see that "Nairobi City" is different in the "crops" dataset, we can change that and check the counties again just to be sure.
crops$County[crops$County == "Nairobi"] <- "Nairobi City"

#agriculture and crops county re-check
countycheck <- cbind.data.frame(agriculture$County,crops$County,gender$County,othercrops$County,livestock$County)
names(countycheck) <- c("agriculture","crops","gender","othercrops","livestock")
countycheck[which(countycheck[,1] != countycheck[,2:5]),]

#It all checks out! Now we can merge the datasets.

#Data Merge====
masterset1 <- left_join(agriculture,crops,by = "County")
masterset2 <- left_join(gender,othercrops, by = "County")
masterset3 <- left_join(masterset1,masterset2, by = "County")
masterset <- left_join(masterset3,livestock, by = "County")
rm(masterset1,masterset2,masterset3)

#Taking the "Kenya" observation out as it's acting as an aggregated total for all the counties. Will store it just in case I need it
kenyatotals <- masterset %>%
  filter(County == "Kenya")

#Labeling each county as "predominately Male or Female and taking "Kenya" out of the main set"
mastersetMF <- masterset %>%
  mutate(Gendermajority = ifelse(Female > Male, "Predominately Female", "Predominately Male")) %>%
  filter(County != "Kenya")

#Pulling vectors of predominately male/female county names.
Femcounties <- as.vector(mastersetMF$County[which(mastersetMF$Gendermajority == "Predominately Female")])
Malecounties <- as.vector(mastersetMF$County[which(mastersetMF$Gendermajority == "Predominately Male")])

#Removing the counties as they arent needed right now
mastersetMF <- mastersetMF %>%
  select(-County)

#Setting all NAs to 0
mastersetMF[is.na(mastersetMF)] <- 0

#Collapsing into gender groups
mastersetMF <- mastersetMF %>%
  group_by(Gendermajority) %>%
  summarise(across(everything(), list(sum)))
  
#Cleaning the names
names(mastersetMF) <- str_replace(names(mastersetMF),"_1","")

#Just had the idea to do a food pyramid! Will take out the columns not needed then create "summary" columns for each section of the food pyramid. Will then remove the leftover columns not needed again.

#Final wrangle====
mastersetMF <- mastersetMF %>%
  select(-c(contains(c("Farming","Total","Crop","Male","Female","Intersex","Khat","Irrigation","Livestock","Cotton")))) %>%
  mutate(`Fish/Poultry` = (Aquaculture + Fishing + FishPonds + FishCages + IndigenousChicken + ExoticChicken_Layers + ExoticChicken_Broilers + Rabbits)) %>%
  select(-c(Aquaculture, Fishing, FishPonds, FishCages, IndigenousChicken, ExoticChicken_Layers, ExoticChicken_Broilers, Rabbits)) %>%
  mutate(`Nuts/Seeds/Beans` = (Macadamia + `Cashew Nut` + Beans + GreenGrams + GroundNuts )) %>%
  select(-c(Macadamia, `Cashew Nut`, Beans, GreenGrams,GroundNuts)) %>%
  mutate(`Grains/Wheat/Flour` = (Sorghum + Wheat + Millet + Rice)) %>%
  select(-c(Sorghum, Wheat, Millet, Rice)) %>%
  mutate(Vegetables = (Maize + Potatoes + Cassava + SweetPotatoes + Cabbages + Tomatoes + Onions + Kales)) %>%
  select(-c(Maize, Potatoes, Cassava, SweetPotatoes, Cabbages, Tomatoes, Onions, Kales)) %>%
  mutate(Meat = (ExoticCattle_Beef + IndigenousCattle + Sheep + Goats + Camels + Donkeys + Pigs)) %>%
  select(-c(ExoticCattle_Beef, IndigenousCattle, Sheep, Goats, Camels, Donkeys, Pigs)) %>%
  mutate(Fruits = (Avocado + Citrus + Mango + Coconut + Bananas + Watermelons)) %>%
  select(-c(Avocado, Citrus, Mango, Coconut, Bananas, Watermelons)) %>%
  mutate(`Sugar/\nDrinks` = (Tea + Coffee + Sugarcane + Beehives)) %>%
  select(-c(Tea, Coffee, Sugarcane, Beehives)) %>%
  rename(Dairy = ExoticCattle_Dairy) 
  
#Converting counts to percentages====
mastersetMFpercents <- as.data.frame(prop.table(as.matrix(mastersetMF[,-1]), 2))
mastersetMFpercents <- mastersetMFpercents %>%
  mutate(across(everything(), list(function(x) paste0(format((x * 100), digits = 2, nsmall = 1),"%"))), .keep = "none")

#Final merge-back====
mastersetMF[,2:9] <- mastersetMFpercents


#Now...let's look at the counties on a map====
#Converting shapefiles to sf type
shapefiles <- sf::st_as_sf(KenyaCounties_SHP)

#Cleaning the capitalization in the shapefiles set.
shapefiles$County <- str_to_title(shapefiles$County)

#Adding male/female gender designations
shapefiles <- shapefiles %>%
  mutate(Gender = ifelse(County %in% Femcounties,"Predominately Female",
                         ifelse(County %in% Malecounties, "Predominately Male","ERROR")))

#Mapping basic county map with gender dominance classifications
kenyagendermap <- ggplot(shapefiles) +
  theme_void() +
  theme(plot.background = element_rect(fill = NA, linetype = "blank"),
        panel.background = element_rect(fill = NA, linetype = "blank"),
        legend.position = "none")+
  geom_sf(data = shapefiles, mapping = aes(fill = Gender),color = "#424031", size = 1) +
  scale_fill_manual(values = c("#ba0000","#006600")) 

#Saving the ggplot to an image for loading onto the visual
ggsave("kenyagendermap.png", kenyagendermap, dpi = 100, width = 7.8 , height = 4.5, units = "in", bg = "transparent")

#Now...time to plot the food pyramid....how the heck....ok. think shapes, trapezoids...let's see====
#Need to define the points of the pyramid. Will make separate data frames and then rbind them into one dataset.

#Vegetables===
FemaleVeg <- data.frame(x = c(0,9.63,9.63,2.25),
                      y = c(0,0,2.5,2.5),
                      gender = "Predominately Female")

MaleVeg <- data.frame(x = c(9.63,10,10,9.63),
                        y = c(0,0,2.5,2.5),
                        gender = "Predominately Male")

Veg <- rbind(FemaleVeg,MaleVeg)

#Grains===
FemaleGrain <- data.frame(x = c(10.555,20.1,17.85,10.555),
                          y = c(0,0,2.5,2.5),
                          gender = "Predominately Female")

MaleGrain <- data.frame(x = c(10.1,10.555,10.555,10.1),
                        y = c(0,0,2.5,2.5),
                        gender = "Predominately Male")

Grain <- rbind(MaleGrain,FemaleGrain)

#Fruits===
FemaleFruit <- data.frame(x = c(2.25,9.42,9.42,4.65),
                          y = c(2.6,2.6,5.1,5.1),
                          gender = "Predominately Female")

MaleFruit <- data.frame(x = c(9.42,10,10,9.42),
                        y = c(2.6,2.6,5.1,5.1),
                        gender = "Predominately Male")

Fruit <- rbind(FemaleFruit,MaleFruit)

#Nuts/Seeds/Beans===
FemaleNuts <- data.frame(x = c(10.6115,17.85,15.45,10.6115),
                          y = c(2.6,2.6,5.1,5.1),
                          gender = "Predominately Female")

MaleNuts <- data.frame(x = c(10.1,10.6115,10.6115,10.1),
                        y = c(2.6,2.6,5.1,5.1),
                        gender = "Predominately Male")

Nuts <- rbind(MaleNuts, FemaleNuts)

#Dairy===
FemaleDairy <- data.frame(x = c(4.65,9.6041,9.6041,7.0),
                         y = c(5.2,5.2,7.7,7.7),
                         gender = "Predominately Female")

MaleDairy <- data.frame(x = c(9.6041,10,10,9.6041),
                        y = c(5.2,5.2,7.7,7.7),
                       gender = "Predominately Male")

Dairy <- rbind(FemaleDairy, MaleDairy)

#Fish===
FemaleFish <- data.frame(x = c(10.96,15.45,13.25,10.96),
                          y = c(5.2,5.2,7.7,7.7),
                          gender = "Predominately Female")

MaleFish <- data.frame(x = c(10.1,10.96,10.96,10.1),
                        y = c(5.2,5.2,7.7,7.7),
                        gender = "Predominately Male")

Fish <- rbind(MaleFish, FemaleFish)

#Meat===
FemaleMeat <- data.frame(x = c(7.0,9.142,9.142),
                         y = c(7.8,7.8,10.3),
                         gender = "Predominately Female")

MaleMeat <- data.frame(x = c(9.142,10,10,9.142),
                       y = c(7.8,7.8,11.25,10.3),
                       gender = "Predominately Male")

Meat <- rbind(FemaleMeat, MaleMeat)

#Sugar===
FemaleSugar <- data.frame(x = c(10.29,13.25,10.29),
                         y = c(7.8,7.8,11),
                         gender = "Predominately Female")

MaleSugar <- data.frame(x = c(10.1,10.29,10.29,10.1),
                       y = c(7.8,7.8,11,11.25),
                       gender = "Predominately Male")

Sugar <- rbind(MaleSugar, FemaleSugar)

#Loading in the kenya map that was created earlier====
map <- png::readPNG("kenyagendermap.png")
map <- grid::rasterGrob(map, interpolate = TRUE)


#Storing the food industry names for plotting & adding in percentages from master set===
Industry <- data.frame(Type = names(mastersetMF)[-1],
                       x = c(7.325,12.775,13.975,15.1,5,9.2,6.125,10.8),
                       y = c(6.45,6.45,3.85,1.25,1.25,9.3,3.85,9.3),
                       Female = as_vector(mastersetMF[1,-1]),
                       Males = as_vector(mastersetMF[2,-1]))

# Making extra layers to clean up the visuals===
backpy <- data.frame(x = c(0,20.1,10),
                     y = c(0,0,11.25))
frontpy <- data.frame(x = c(0,20.1,10.05),
                      y = c(0,0,11.3))


#Plotting the polygons===
foodpyramid <- ggplot(backpy, aes(x = x, y = y)) +
  geom_polygon() + 
  scale_fill_manual(values = c("#ba0000","#006600","#424031")) +
  layer(data = Veg,
        geom = "polygon",
        stat = "identity",
        position = "identity",
        mapping = aes(fill = gender))+
  layer(data = Grain,
        geom = "polygon",
        stat = "identity",
        position = "identity",
        mapping = aes(fill = gender))+
  layer(data = Fruit,
        geom = "polygon",
        stat = "identity",
        position = "identity",
        mapping = aes(fill = gender))+  
  layer(data = Nuts,
        geom = "polygon",
        stat = "identity",
        position = "identity",
        mapping = aes(fill = gender))+ 
  layer(data = Dairy,
        geom = "polygon",
        stat = "identity",
        position = "identity",
        mapping = aes(fill = gender))+ 
  layer(data = Fish,
        geom = "polygon",
        stat = "identity",
        position = "identity",
        mapping = aes(fill = gender))+
  layer(data = Meat,
        geom = "polygon",
        stat = "identity",
        position = "identity",
        mapping = aes(fill = gender))+
  layer(data = Sugar,
        geom = "polygon",
        stat = "identity",
        position = "identity",
        mapping = aes(fill = gender))+
  ylim(0,20) +
  xlim(-10,20.2) +
  annotation_custom(map, xmin = -9, xmax = 10, ymin = 2, ymax = Inf) 

foodpyramid + 
  geom_polygon(data = frontpy, 
               aes(x = x, y = y), 
               color = "#424031", 
               fill = NA, 
               size = 4 ) + 
  geom_label(data = Industry,
             aes(label=Type, 
                 alpha = .9,
                 family = "Century Gothic",
                 fontface = "bold"), fill = "#000000" ,color = "#FFFFFF") +
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#c4a84b"))




  
