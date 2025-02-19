library(ggplot2) 
library(patchwork)
library(tidyverse)
library(sf)
library(stars)
library(gstat)

### First steps, reading all necessary data ####

# Read in Stripe Data

data <- read.csv("Data_backup/Working Copy/Processed Data/Combined Dataset_2025_02_14.csv")

View(data %>% group_by(species) %>% summarise(n = n())) # Look at the numbers of samples by species and sex
View(data %>% group_by(source, sex) %>% summarise(n = n())) # Look at the numbers of samples by source and sex. We can see far more NAs for sex in collection data than iNat


data.spatial <- st_as_sf(data, crs = 4326, coords = 
           c("longitude", "latitude"))
data.spatial  <- st_transform(data.spatial, crs = 3857)


# Read in spatial polygons of species ranges

species_ranges <- st_read("Geometries/Species Ranges_IUCN/data_0.shp", crs = 4326) # upload shapefile
species_ranges <- st_transform(species_ranges, crs = 3857)



#Read in shapefile for Africa
africa <- st_read("Geometries/Africa/Africa_cropped.shp")
africa  <- st_transform(africa, crs = 3857)

## Check spatial points against IUCN ranges

ggplot() + geom_sf(data = data.spatial %>% filter(species=="derbianus")) + 
  xlab(NULL) + ylab(NULL) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # add species range
  geom_sf(data = species_ranges %>% filter(SCI_NAME == "Tragelaphus derbianus"), fill = "blue", color = "black", alpha = 0.1) 


##

split_plot_Tsetse <- ggplot(aes(TsetsePresencePROB, n_distinct_stripes), data = data) + 
  geom_point() + 
  facet_wrap(~ species) + # create a facet for each mountain range
  xlab("TsetsePresenceProb") + 
  ylab("N of stripes")


split_plot_Tabanid <- ggplot(aes(TabanidActivity, n_distinct_stripes), data = data) + 
  geom_point() + 
  facet_wrap(~ species) + # create a facet for each mountain range
  xlab("Tabanid Activity") + 
  ylab("N of stripes")


split_plot_PNV <- ggplot(aes(PNV, n_distinct_stripes), data = data) + 
  geom_point() + 
  facet_wrap(~ species) + # create a facet for each mountain range
  xlab("PNV") + 
  ylab("N of stripes")


# Plot graphs showing variation within species ####

# T. angasii - Lowland nyala ####

# Plot raw variation

ggplot(data %>%  filter(species == "angasii"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. angasii") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>%  filter(species == "angasii") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. angasii") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.angasii <- ggplot() +
  geom_sf(
    data=data.spatial %>%  filter(species == "angasii"), 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(1000000, 4700000),  # Longitude range 
    ylim = c(-4300000, -560000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. angasii (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_angasii <- species_ranges %>% filter(SCI_NAME == "Tragelaphus angasii") # upload shapefile
data_angasii <- data.spatial %>% filter(species == "angasii") 

grid.angasii <- st_bbox(species_ranges_angasii) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_angasii)

i.angasii <- idw(n_distinct_stripes~1, data_angasii, grid.angasii)

interpolated.angasii <- ggplot() + geom_stars(data = i.angasii, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(1000000, 4700000),  # Longitude range 
    ylim = c(-4300000, -560000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. angasii (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )
  

# T. buxtoni - Mountain nyala ####

# Plot raw variation

ggplot(data %>%  filter(species == "buxtoni"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. buxtoni") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>%  filter(species == "buxtoni") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. buxtoni") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.buxtoni<-ggplot() +
  geom_sf(
    data=data.spatial %>%  filter(species == "buxtoni"), 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(3500000, 6000000),  # Longitude range 
    ylim = c(340000, 1700000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. buxtoni (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_buxtoni <- species_ranges %>% filter(SCI_NAME == "Tragelaphus buxtoni") # upload shapefile
data_buxtoni <- data.spatial %>% filter(species == "buxtoni") 

grid.buxtoni <- st_bbox(species_ranges_buxtoni) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_buxtoni)

i.buxtoni <- idw(n_distinct_stripes~1, data_buxtoni, grid.buxtoni)

interpolated.buxtoni <- ggplot() + geom_stars(data = i.buxtoni, 
                                              aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(3500000, 6000000),  # Longitude range 
    ylim = c(340000, 1700000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. buxtoni (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )


# T. derbianus - Giant eland ####

# Plot raw variation

ggplot(data %>%  filter(species == "derbianus"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. derbianus") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>%  filter(species == "derbianus") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. derbianus") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.derbianus<-ggplot() +
  geom_sf(
    data=data.spatial %>%  filter(species == "derbianus"), 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 5000000),  # Longitude range 
    ylim = c(340000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. derbianus (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_derbianus <- species_ranges %>% filter(SCI_NAME == "Tragelaphus derbianus") # upload shapefile
data_derbianus <- data.spatial %>% filter(species == "derbianus") 

grid.derbianus <- st_bbox(species_ranges_derbianus) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_derbianus)

i.derbianus<- idw(n_distinct_stripes~1, data_derbianus, grid.derbianus)

interpolated.derbianus <- ggplot() + geom_stars(data = i.derbianus, 
                                              aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 5000000),  # Longitude range 
    ylim = c(340000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. derbianus (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )


# T. eurycerus - Bongo ####

# Plot raw variation

ggplot(data %>%  filter(species == "eurycerus"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. eurycerus") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>%  filter(species == "eurycerus") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. eurycerus") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.eurycerus<-ggplot() +
  geom_sf(
    data=data.spatial %>%  filter(species == "eurycerus"), 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 5000000),  # Longitude range 
    ylim = c(-1000000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. eurycerus (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_eurycerus <- species_ranges %>% filter(SCI_NAME == "Tragelaphus eurycerus") # upload shapefile
data_eurycerus <- data.spatial %>% filter(species == "eurycerus") 

grid.eurycerus <- st_bbox(species_ranges_eurycerus) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_eurycerus)

i.eurycerus<- idw(n_distinct_stripes~1, data_eurycerus, grid.eurycerus)

interpolated.eurycerus <- ggplot() + geom_stars(data = i.eurycerus, 
                                                aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 5000000),  # Longitude range 
    ylim = c(-1000000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. eurycerus (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )


# T. strepsiceros - Greater Kudu ####

# Plot raw variation

ggplot(data %>%  filter(species == "strepsiceros"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. strepsiceros") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>%  filter(species == "strepsiceros") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. strepsiceros") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.strepsiceros<-ggplot() +
  geom_sf(
    data=data.spatial %>%  filter(species == "strepsiceros"), 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. strepsiceros (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_strepsiceros <- species_ranges %>% filter(SCI_NAME == "Tragelaphus strepsiceros") # upload shapefile
data_strepsiceros <- data.spatial %>% filter(species == "strepsiceros") 

grid.strepsiceros <- st_bbox(species_ranges_strepsiceros) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_strepsiceros)

i.strepsiceros<- idw(n_distinct_stripes~1, data_strepsiceros, grid.strepsiceros)

interpolated.strepsiceros <- ggplot() + geom_stars(data = i.strepsiceros, 
                                                aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. strepsiceros (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )


# T. imberbis - Lesser Kudu ####

# Plot raw variation

ggplot(data %>%  filter(species == "imberbis"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. imberbis") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>%  filter(species == "imberbis") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. imberbis") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.imberbis<-ggplot() +
  geom_sf(
    data=data.spatial %>%  filter(species == "imberbis"), 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(2500000, 6000000),  # Longitude range 
    ylim = c(-1500000, 1700000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. imberbis (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_imberbis <- species_ranges %>% filter(SCI_NAME == "Tragelaphus imberbis") # upload shapefile
data_imberbis <- data.spatial %>% filter(species == "imberbis") 

grid.imberbis <- st_bbox(species_ranges_imberbis) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_imberbis)

i.imberbis<- idw(n_distinct_stripes~1, data_imberbis, grid.imberbis)

interpolated.imberbis <- ggplot() + geom_stars(data = i.imberbis, 
                                                   aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(2500000, 6000000),  # Longitude range 
    ylim = c(-1500000, 1700000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. imberbis (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )


# T. oryx - Common eland ####

# Plot raw variation

ggplot(data %>%  filter(species == "oryx"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. oryx") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>%  filter(species == "oryx") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. oryx") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.oryx<-ggplot() +
  geom_sf(
    data=data.spatial %>%  filter(species == "oryx"), 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. oryx (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_oryx <- species_ranges %>% filter(SCI_NAME == "Tragelaphus oryx") # upload shapefile
data_oryx <- data.spatial %>% filter(species == "oryx") 

grid.oryx <- st_bbox(species_ranges_oryx) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_oryx)

i.oryx<- idw(n_distinct_stripes~1, data_oryx, grid.oryx)

interpolated.oryx <- ggplot() + geom_stars(data = i.oryx, 
                                               aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. oryx (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )



# T. spekii - Sitatunga ####

# Plot raw variation

ggplot(data %>%  filter(species == "spekii"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. spekii") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>%  filter(species == "spekii") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. spekii") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.spekii<-ggplot() +
  geom_sf(
    data=data.spatial %>%  filter(species == "spekii"), 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. spekii (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_spekii <- species_ranges %>% filter(SCI_NAME == "Tragelaphus spekii") # upload shapefile
data_spekii <- data.spatial %>% filter(species == "spekii") 

grid.spekii <- st_bbox(species_ranges_spekii) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_spekii)

i.spekii<- idw(n_distinct_stripes~1, data_spekii, grid.spekii)

interpolated.spekii <- ggplot() + geom_stars(data = i.spekii, 
                                           aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. spekii (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )




# T. sylvaticus/scriptus - Bushbuck ####

# Plot raw variation

ggplot(data %>%  filter(species == "sylvaticus"|species == "scriptus"|species == "sylvaticus/scriptus"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. sylvaticus/scriptus") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>%  filter(species == "sylvaticus"|species == "scriptus"|species == "sylvaticus/scriptus") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. sylvaticus/scriptus") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.sylvaticus_scriptus<-ggplot() +
  geom_sf(
    data=data.spatial %>%  filter(species == "sylvaticus"|species == "scriptus"|species == "sylvaticus/scriptus"), 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. sylvaticus/scriptus (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_sylvaticus_scriptus <- species_ranges %>% filter(SCI_NAME == "Tragelaphus scriptus") # upload shapefile
data_sylvaticus_scriptus <- data.spatial %>% filter(species == "sylvaticus"|species == "scriptus"|species == "sylvaticus/scriptus") 

grid.sylvaticus_scriptus <- st_bbox(species_ranges_sylvaticus_scriptus) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_sylvaticus_scriptus)

i.sylvaticus_scriptus<- idw(n_distinct_stripes~1, data_sylvaticus_scriptus, grid.sylvaticus_scriptus)

interpolated.sylvaticus_scriptus <- ggplot() + geom_stars(data = i.sylvaticus_scriptus, 
                                             aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. sylvaticus/scriptus (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )




# All species ####


# Plot raw variation

ggplot(data, aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="All Tragelaphus") +
  theme_bw(base_size = 20)

# Plot variation with sex plotted separately

ggplot(data %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="All Tragelaphus") +
  theme_bw(base_size = 20)


# Plot variation spatially

# First just plot raw points

raw.all_species<-ggplot() +
  geom_sf(
    data=data.spatial, 
    aes(col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Add a color scale for points
  scale_color_viridis_c( na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - all Tragelaphus (raw data)",
    x = "Longitude",
    y = "Latitude"
  )

# Now do interpolation to see inferred values over entire species range

species_ranges_all_species <- species_ranges  # upload shapefile
data_all_species <- data.spatial

grid.all_species <- st_bbox(species_ranges_all_species) %>% st_as_stars(dx = 10000)  %>%
  st_crop(species_ranges_all_species)

i.all_species<- idw(n_distinct_stripes~1, data_all_species, grid.all_species)

interpolated.all_species <- ggplot() + geom_stars(data = i.all_species, 
                                                          aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  # Remove NA values
  scale_fill_viridis_c(na.value = NA, breaks = c(0, 5, 10, 15), limits = c(0, 15), name = "Number of Distinct Stripes") +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  coord_sf(
    xlim = c(-2500000, 6000000),  # Longitude range 
    ylim = c(-4500000, 2200000),  # Latitude range
    expand = FALSE
  ) +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - all Tragelaphus (IDW interpolation)",
    x = "Longitude",
    y = "Latitude"
  )

