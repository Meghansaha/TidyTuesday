#======Tidy Tuesday 12-14-2021 Spice Girls======#

# Library Load-In====
library(tidyverse) # For everything
library(showtext) # For OTF fonts
library(extrafont) # For TTF fonts
library(glue) # For custom titles and captions
library(ggtext) # For custom titles and captions

#Fonts: 
#MustangSans: https://drive.google.com/file/d/1xCaPDz761aWLZqbZD_lDM25XQiUm6r9_/view?usp=drivesdk
#Disko_OT: https://www.dafont.com/disko.font
#ImpactLabel-lVYZ: https://www.dafont.com/impact-label.font



# Data Load In====
# The lyrics data set has first words spoken...I wanted to plot first voice (including harmonizing) Data was manually pulled
#Because I never need an excuse to rewatch Spice Girls videos to confirm who's singing/speaking/etc.
SG_Voices <- read_csv("SG_Voices.csv")

# Does it matter that I leave these font paths here? I'm so tired and I have so much work to do. Maybe it'll help someone figure out font loading with showtext
font_add("MustangSans", "local font path here")
font_add("Disko_OT", "local font path here")
font_add("ImpactLabel-lVYZ", "local font path here")

loadfonts(device = "win", quiet = TRUE) # extrafont is used for ttf instead of otf here 
showtext_auto()

#Need to combine these totals between the albums#
#2 albums, 5 spices...5 90's inspired lollipops per album?
#xlim = 0-120 with 10 inch "buffers" so max 110 in data


# Data Wrangling/Transformation=====
SG_voices_sum <- tibble(x = c(seq(20,100, by = 20),
                              seq(140,220, by = 20)),
                        y =c(sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Ginger|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Sporty|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Scary|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Baby|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spice")],"Posh|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Ginger|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Sporty|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Scary|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Baby|All")),
                             sum(str_count(SG_Voices$First_Voice[which(SG_Voices$Album == "Spiceworld")],"Posh|All"))),
                        spice = rep(c("Ginger","Sporty","Scary","Baby","Posh"),2),
                        album = c(rep("Spice",5),rep("Spiceworld",5)))

# We need Lollipops and they need a "bumpy" part====
# The foundation for our base, a theta for a circle.
theta <- seq(0,2*pi, length.out = 100)

#Setting an empty list to iterate and make each spice a lollipop for each album we recognize (Forever doesnt exist here sry)====
spice_pops <- list()

#Getting the bases of the lollipops in a list====
for(i in seq_along(1:nrow(SG_voices_sum))){
  spice_pops[[i]] <- tibble(x = 7*cos(theta)+SG_voices_sum$x[i],
                            y = 7*sin(theta)+(SG_voices_sum$y[i])*10,
                            group = SG_voices_sum$spice[i])
}

# Setting names for each spice's lollipop====
names(spice_pops) <- paste0(unique(SG_voices_sum$spice),c(rep("_Spice",5),rep("_SW",5)))

#Smooshing into a dataframe====
spice_pops_base <- bind_rows(spice_pops)

# Doing the same process for the bumpy part of the lollipop. We can use geom_segment to make the illusion of a "bump" in the lollipop====
bumpy_part <- list()

for(i in seq_along(1:length(spice_pops))){
  bumpy_part[[i]] <- tibble(x = min(spice_pops[[names(spice_pops)[i]]][["x"]]),
                            xend = max(spice_pops[[names(spice_pops)[i]]][["x"]]),
                            y = median(spice_pops[[names(spice_pops)[i]]][["y"]]),
                            yend = median(spice_pops[[names(spice_pops)[i]]][["y"]]),
                            group = SG_voices_sum$spice[i])
}

spice_pops_bump <- bind_rows(bumpy_part)

# Setting up the color palettes====
color_pal <- rep(c("#C04000","#90ee90","#DAA520","#f4c2c2","#2c003f"),2)
border_pal <- sapply(color_pal, function(x) colorRampPalette(c(x,"#000000"))(10)[3], USE.NAMES = FALSE)


# Creating the block backgrounds====
background_left <- tibble(x = c(-Inf,120,120,-Inf),
                          y = c(-Inf,-Inf,Inf,Inf))

background_right <- tibble(x = c(120,Inf,Inf,120),
                          y = c(-Inf,-Inf,Inf,Inf))

# Making the final plot====
spice_pops_base %>%
  ggplot(aes(x,y, group = group))+
  theme_void()+
  geom_polygon(data = background_left, aes(x,y), fill = "#A1C724", inherit.aes = FALSE)+
  geom_polygon(data = background_right, aes(x,y), fill = "#E52B97", inherit.aes = FALSE)+
  geom_segment(data = SG_voices_sum, aes(x =x, xend = x, y = -9, yend = y*10), color = "black", size = 3, inherit.aes = FALSE)+
  geom_segment(data = SG_voices_sum, aes(x =x, xend = x, y = -9, yend = y*10), color = "white", size = 2, inherit.aes = FALSE)+
  geom_polygon(fill = rep(color_pal, each = 100))+
  geom_segment(data = spice_pops_bump, aes(x = x, xend = xend, y=y, yend=yend, group = group), inherit.aes = FALSE, color = border_pal, size = 3.5, lineend = "round")+
  geom_segment(data = spice_pops_bump, aes(x = x, xend = xend, y=y, yend=yend, group = group), inherit.aes = FALSE, color = color_pal, size = 2.5, lineend = "round")+
  xlim(0,240)+
  theme(panel.background = element_rect(fill = "#000000"),
        plot.background = element_rect(fill = "#000000"),
        plot.title =element_textbox_simple(
    padding = margin(10,0,10,0),
    halign = .5,
    valign = 1,
    size = 40,
    family = "MustangSans",
    color = "#ffffff",
    fill = "#000000"),
    plot.subtitle = element_textbox_simple(
      family = "MustangSans",
      padding = margin(.5,.95,.5,.5, unit = "in"),
      color = "#ffffff",
      fill = "#000000",
      lineheight = 2,
      size = 12),
    plot.caption = element_textbox_simple(size = 12,
                                          family = "MustangSans",
                                          color = "#ffffff",
                                          fill = "#000000",
                                          padding = margin(.2,.1,.1,.1, unit = "in"),
                                          hjust = 1))+
  geom_text(data = SG_voices_sum, aes(x,y*10 , label = paste0(y,"\n",spice)), inherit.aes = FALSE, family = "MustangSans", color = "White", size = 7)+
  geom_text(aes(x=0,y=-9, label = "SPICE"),family = "ImpactLabel-lVYZ", inherit.aes = FALSE, size = 10)+
  geom_text(aes(x=230,y=-9, label = "SPICEWORLD"),family = "ImpactLabel-lVYZ", inherit.aes = FALSE, size = 10)+
  labs(title = glue("Which Spice <span style='font-family: Disko_OT; color:#A1C724;'>P O</span><span style='font-family: Disko_OT; color:#E52B97;'> P S </span> On The Track First?"),
       subtitle = "This visual summarizes the total amount of times each Spice Girl in the eponymous girl group has spoken, sang, or harmonized first on the Spice and Spiceworld albums. Individual Spices got credit for popping onto the track first if they sang, spoke, or harmonized in unison with other spices. 
       The trends change a bit between the albums, but the Spices maintain their positions with Ginger leading in first, Sporty in second, Scary and Baby tying for third, and Posh with the fewest times.",
       caption = glue("Data Source: Spice Girls by Jacquie Tran | Created by: @Meghansharris ", '<img style="display: inline-block;"  src="twitter.png" alt="twitter" width="12" height="12" />'))+
  coord_cartesian(clip = "off")
