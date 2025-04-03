install.packages(c("dplyr", "terra", "sf", "ggplot2", "tmap", "readr"))
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(tmap)
library(readr)
setwd("C:/Users/tatec/Documents/AGR333")  

# Read inventory data
u2_data <- read.csv("U2_2017data.csv")

# Read species codes
SppCode <- read.csv("Species_Codes.csv")

# Read biomass equations
Bm_equa <- read.csv("Biomass Equation.csv")
#filter out overstory tree and remove missing values
u2_data <- subset(u2_data, DBH != '' & Code != '' & Class == "O")  # Keep only overstory trees
#merge inventory data with species code
trees_merge <- merge(u2_data, SppCode, by.x = "Code", by.y = "SppCode", all.x = TRUE)

#calculate EF based on plot size
plot_radius <- 58.5  # Radius in feet
plot_area_acres <- (pi * plot_radius^2) / 43560  # Convert square feet to acres
EF <- round(1 / plot_area_acres)  # Expansion Factor (rounded)

#compute diameter in feet and basal area
trees_merge$dia_ft <- as.numeric(trees_merge$DBH) / 12
trees_merge$BA <- pi * (trees_merge$dia_ft / 2)^2
trees_merge$BA_pa <- trees_merge$BA * EF
trees_merge$TPA <- EF

#Summarize BA and TPA by plot
sum_u2_TPA <- aggregate(trees_merge$TPA, by = list(trees_merge$Plot), FUN = sum)
names(sum_u2_TPA) <- c("Plot", "TPA")

sum_u2_BA <- aggregate(trees_merge$BA_pa, by = list(trees_merge$Plot), FUN = sum)
names(sum_u2_BA) <- c("Plot", "BA")

sum_u2 <- merge(sum_u2_TPA, sum_u2_BA)

#merge biomass equations
trees_merge <- merge(trees_merge, Bm_equa, by = "Chojnacky_Code", all.x = TRUE)

#compute biomass per tree
trees_merge$biomass <- exp(trees_merge$b0 + trees_merge$b1 * log(trees_merge$DBH * 2.54))

#aggregate biomass by plot
sum_u2_bm <- aggregate(biomass ~ Plot, data = trees_merge, sum)
sum_u2_bm$bm_pa <- sum_u2_bm$biomass * EF
sum_u2 <- merge(sum_u2, sum_u2_bm, by = "Plot")
#species in each plot
library(dplyr)
tree_cnt <- trees_merge %>% group_by(Plot, Code) %>% tally()

#identify most dominant 
dom_cnt <- tree_cnt %>% group_by(Plot) %>% filter(n == max(n))

