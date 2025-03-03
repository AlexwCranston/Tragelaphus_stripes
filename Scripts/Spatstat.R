# Load necessary packages

library(spatstat)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)

#Load data


data <- read.csv("Data_Backup/Working Copy/Processed Data/Combined Dataset_2025_02_21.csv")

data.iNat <- data %>% filter(source == "iNat")
data.collections <- data %>% filter(source == "collection")


poly

#Read in shapefile for Africa
africa <- st_read("Geometries/Africa/Africa_cropped.shp")
poly <- africa$geometry[[1]]
window <- as.owin(st_geometry(poly)) 

ppp.iNat <- ppp(data.iNat$longitude,data.iNat$latitude, window = window,
               check = TRUE, checkdup=TRUE, drop=TRUE)

quadrat.test(ppp.iNat, nx=5, ny=5,
            alternative=c("clustered"),
            method=c("Chisq"))
