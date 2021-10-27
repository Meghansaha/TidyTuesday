# Tidy Tuesday Contribution - Ultra Trail Running #

# Library Load-in====
library(tidyverse) # For Everything Data
library(showtext) # To bring in text from Google
library(patchwork) # To wrangle ggplot layouts
library(cowplot) # To make patchwork plots play nice

# Data Load-in====
Rankings_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')

# Data Transformation====
## Filtering out for USA, MEX, and CAN====
Rankings_subset <- Rankings_data %>%
  filter(nationality %in% c("USA","MEX","CAN")) %>%
  select(nationality,age) %>%
  filter(age >= 15) # Keeping only the data we want and removing those aged 18 or older #

# Want to make a raincloud of ages of runners in North America #

# Subsetting the three countries into their own data sets====

## Reference vector for the countries====
countries <- unique(Rankings_subset$nationality)
names(countries) <- c("United States","Canada","Mexico")

filtered_data <- list()

## Subsetting in a loop to create different data frames===

for(i in seq_along(countries)){
filtered_data[[i]] <- Rankings_subset %>%
  filter(nationality == countries[i]) 

names(filtered_data)[i] <- countries[i]
}

# Creating a color palette====
background <- "#decca6" # Background color

fills <- c("#ba3834","#6a6a2a","#634d41") # Fill color

borders <- c()

for(i in seq_along(fills)){
borders[i] <- colorRampPalette(c(fills[i],"black"))(10)[5] # Border color calculated with grDevices pkg
}

names(borders) <- countries

# Creating limits for plot consistency====
plot_limits <- range(Rankings_subset$age)


# Making the distribution plots====
dist_curves <- list()

## Iterating through the three datasets====
for(i in seq_along(filtered_data)){
  
dist_curves[[i]] <- filtered_data[[i]] %>% 
  ggplot(aes(x = age))+
  geom_density(fill = fills[i], color = borders[i], size = 2) +
  theme_void()+
  geom_segment(x= mean(filtered_data[[i]]$age), xend = mean(filtered_data[[i]]$age), y = 0, 
               yend = .07, color = borders[i], size = 1.5, inherit.aes = FALSE)+
  geom_segment(x= min(filtered_data[[i]]$age), xend = max(filtered_data[[i]]$age), y = .0001, 
               yend = .0001, color = borders[i], size = 2, inherit.aes = FALSE)+
  coord_cartesian(xlim = plot_limits, ylim = c(0,.1))+
  theme(legend.position = "none")+
  annotate(geom = "rect",
           xmin = -Inf,
           xmax = 40,
           ymin = .9,
           ymax = Inf,
           color = borders[i],
           fill = fills[i],
           size = 2)+
  annotate(geom = "text",
           x = 26,
           y = .057,
           label = names(countries)[i],
           family ="Shadows Into Light Two",
           size = 10,
           color = borders[i])

names(dist_curves)[i] <- paste0(countries[i],"_Dist")
}


# Making Boxplots====
box_plots <- list()

## Iterating through the three datasets====
for(i in seq_along(filtered_data)){
  
box_plots[[i]] <- filtered_data[[i]] %>% 
  ggplot(aes(x=age))+
  geom_boxplot(fill = fills[i], color = borders[i], size = 1.5)+
  theme_void()+
  coord_cartesian(xlim = plot_limits)
  
names(box_plots)[i] <- paste0(countries[i],"_Boxplots")
}

## Adding in Google Fonts====
font_add_google("Shadows Into Light Two")
showtext_auto()

# Making Scatter Plots====
scatter_plots <- list()

## Iterating through the three datasets====
for(i in seq_along(filtered_data)){
  
  scatter_plots[[i]] <- filtered_data[[i]] %>%
  ggplot(aes(x = age, y = 1))+
  geom_jitter(color = borders[i], shape = 21, fill = fills[i])+
  theme_void()+
  theme(axis.title.x.bottom = element_text(family = "Shadows Into Light Two", color = borders[i], size = 20),
        axis.text.x = element_text(family = "Shadows Into Light Two", color = borders[i], size = 13))+
    scale_x_continuous(limits = plot_limits, breaks = scales::pretty_breaks(10))
  
names(scatter_plots)[i] <- paste0(countries[i],"_Scatter")
}




# Merging plots together to make the rain cloud====
## Pulling out dist_curves====
list2env(dist_curves, envir = .GlobalEnv)

## Pulling out dist_curves====
list2env(box_plots, envir = .GlobalEnv)

## Pulling out dist_curves====
list2env(scatter_plots, envir = .GlobalEnv)

# Using Patchwork to assemble each Country====
Canada_Raincloud <- CAN_Dist / CAN_Boxplots / CAN_Scatter +
  plot_layout(ncol = 1, nrow = 3, heights = c(3,.5,1)) +
  plot_annotation(theme = theme(plot.background = element_rect(fill =background, color = borders["CAN"], size = 5)))

Mexico_Raincloud <- MEX_Dist / MEX_Boxplots / MEX_Scatter +
  plot_layout(ncol = 1, nrow = 3, heights = c(3,.5,1)) +
  plot_annotation(theme = theme(plot.background = element_rect(fill =background, color = borders["MEX"], size = 5)))

US_Raincloud <- USA_Dist / USA_Boxplots / USA_Scatter +
  plot_layout(ncol = 1, nrow = 3, heights = c(3,.5,1)) +
  plot_annotation(theme = theme(plot.background = element_rect(fill =background, color = borders["USA"], size = 5)))

# Using Cowplot to wrangle countries onto one grid====

plots <- plot_grid(Canada_Raincloud, Mexico_Raincloud, US_Raincloud,
                   ncol = 3, 
                   scale = 0.9)

 ggdraw(plots) + 
  theme(plot.background = element_rect(fill=background, color = NA))





#to do - add ggrepel with average age and arrow from average line, place all plots together in one
