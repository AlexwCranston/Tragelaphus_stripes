library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)


setwd("C:/Users/alexw/OneDrive/Documents/PHD/Tragelaphus Project")

data<-read.csv("iNat_Data/Stripe-athon Datasheet_2024_12_19.csv")

data <- data[-c(525:538),]

glimpse(data)


# Breakdown by species

nrow(data %>% filter(species=="oryx")) # 110 Common Eland
nrow(data %>% filter(species=="derbianus")) # 8 Lord Derby's Eland
nrow(data %>% filter(species=="imberbis")) # 12 Lesser Kudu
nrow(data %>% filter(species=="strepsiceros")) # 136 Greater Kudu
nrow(data %>% filter(species=="scriptus")) # 144 Northern Bushbuck
nrow(data %>% filter(species=="sylvaticus")) # 45 Southern Bushbuck
nrow(data %>% filter(species=="angasii")) # 17 Nyala
nrow(data %>% filter(species=="buxtoni")) # 0 Mountain Nyala
nrow(data %>% filter(species=="eurycerus")) # 15 Bongo
nrow(data %>% filter(species=="spekii")) # 37 Sitatunga


# Breakdown by sex

nrow(data %>% filter(sex=="male")) # 225 Males
nrow(data %>% filter(sex=="female")) # 219 Females
# 80 are unknown

# Breakdown by both

nrow(data %>% filter(species=="oryx") %>% filter(sex=="male")) # 37 confirmed Male Common Eland
nrow(data %>% filter(species=="oryx") %>% filter(sex=="female")) # 13 confirmed Female Common Eland

nrow(data %>% filter(species=="derbianus") %>% filter(sex=="male")) # 5 confirmed Male Giant Eland
nrow(data %>% filter(species=="derbianus") %>% filter(sex=="female")) # 2 confirmed Female Common Eland

nrow(data %>% filter(species=="imberbis") %>% filter(sex=="male")) # 9 confirmed male Lesser Kudu
nrow(data %>% filter(species=="imberbis") %>% filter(sex=="female")) # 3 confirmed female Lesser Kudu

nrow(data %>% filter(species=="strepsiceros") %>% filter(sex=="male")) # 65 confirmed male Greater Kudu
nrow(data %>% filter(species=="strepsiceros") %>% filter(sex=="female")) # 70 confirmed male Greater Kudu

nrow(data %>% filter(species=="eurycerus") %>% filter(sex=="male")) # 0 confirmed male Bongo
nrow(data %>% filter(species=="eurycerus") %>% filter(sex=="female")) # 2 confirmed female Bongo

nrow(data %>% filter(species=="scriptus") %>% filter(sex=="male")) # 70 confirmed male Northern Bushbuck
nrow(data %>% filter(species=="scriptus") %>% filter(sex=="female")) # 72 confirmed female Northern Bushbuck

nrow(data %>% filter(species=="sylvaticus") %>% filter(sex=="male")) # 17 confirmed male Northern Bushbuck
nrow(data %>% filter(species=="sylvaticus") %>% filter(sex=="female")) # 28 confirmed female Northern Bushbuck

nrow(data %>% filter(species=="spekii") %>% filter(sex=="male")) # 15 confirmed male Northern Bushbuck
nrow(data %>% filter(species=="spekii") %>% filter(sex=="female")) # 19 confirmed female Northern Bushbuck

nrow(data %>% filter(species=="angasii")  %>% filter(sex=="male")) # 7 confirmed male Nyala
nrow(data %>% filter(species=="angasii")  %>% filter(sex=="female")) # 10 confirmed female Nyala

# Turn the data frame into an sf object with the CRS WGS84 

points_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)


plot(points_sf["number_vertstripes"])
add(africa)

africa <- st_read("Geometries/Africa_cropped.shp")


ggplot() + 
  geom_sf(data=africa) +
  geom_sf(data = points_sf, aes(col=number_vertstripes)) +
  scale_color_viridis_c()

  
