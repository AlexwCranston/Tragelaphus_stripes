library(ggplot2) 

# Read in Data

data <- read.csv("Data_backup/Working Copy/Processed Data/Combined Dataset_2025_01_30.csv")

# Read in spatial polygons


#Read in shapefile for Africa
africa <- st_read("Geometries/Africa_cropped.shp")
plot(africa)

# Plot graphs showing variation within species

# T. angasii - Lowland nyala

ggplot(data %>%  filter(species == "angasii"), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. angasii") +
  theme_bw(base_size = 20)

ggplot(data %>%  filter(species == "angasii") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. angasii") +
  theme_bw(base_size = 20)

ggplot() +
  geom_point(
    data=data %>%  filter(species == "angasii") %>% filter(sex == "male"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. angasii MALES ONLY",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )

# T. buxtoni - Mountain nyala

ggplot(data %>%  filter(species == "buxtoni") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. buxtoni") +
  theme_bw(base_size = 20)

ggplot(data %>%  filter(species == "buxtoni") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. buxtoni") +
  theme_bw(base_size = 20)


ggplot() +
  geom_point(
    data=data %>%  filter(species == "buxtoni"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. buxtoni",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )


# T. derbianus - Giant eland

ggplot(data %>%  filter(species == "derbianus") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. derbianus") +
  theme_bw(base_size = 20)

ggplot(data %>%  filter(species == "derbianus") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. derbianus") +
  theme_bw(base_size = 20)


ggplot() +
  geom_point(
    data=data %>%  filter(species == "derbianus"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. derbianus",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )

# T. eurycerus - Bongo

ggplot(data %>%  filter(species == "eurycerus") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. eurycerus") +
  theme_bw(base_size = 20)

ggplot(data %>%  filter(species == "eurycerus") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. eurycerus") +
  theme_bw(base_size = 20)


ggplot() +
  geom_point(
    data=data %>%  filter(species == "eurycerus"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. eurycerus",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )


# T. strepsiceros - Greater Kudu

ggplot(data %>%  filter(species == "strepsiceros") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. strepsiceros") +
  theme_bw(base_size = 20)

ggplot(data %>%  filter(species == "strepsiceros") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. strepsiceros") +
  theme_bw(base_size = 20)


ggplot() +
  geom_point(
    data=data %>%  filter(species == "strepsiceros"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. strepsiceros",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )


# T. imberbis - Lesser Kudu

ggplot(data %>%  filter(species == "imberbis") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. imberbis") +
  theme_bw(base_size = 20)

ggplot(data %>%  filter(species == "imberbis") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. imberbis") +
  theme_bw(base_size = 20)


ggplot() +
  geom_point(
    data=data %>%  filter(species == "imberbis"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. imberbis",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )


# T. oryx - Common eland

ggplot(data %>%  filter(species == "oryx") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. oryx") +
  theme_bw(base_size = 20)

ggplot(data %>%  filter(species == "oryx") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. oryx") +
  theme_bw(base_size = 20)


ggplot() +
  geom_point(
    data=data %>%  filter(species == "oryx"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. oryx",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )


# T. spekii - Sitatunga

ggplot(data %>%  filter(species == "spekii") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. spekii") +
  theme_bw(base_size = 20)

ggplot(data %>%  filter(species == "spekii") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. spekii") +
  theme_bw(base_size = 20)


ggplot() +
  geom_point(
    data=data %>%  filter(species == "spekii"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. spekii",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )


# T. sylvaticus/scriptus - Bushbuck
ggplot(data %>%  filter(species == "sylvaticus"|species == "scriptus"|species =="sylvaticus/scriptus") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes)) +
  geom_density(fill="#20447a") +
  labs(x="Number of Distinct Stripe", title="T. sylvaticus/scriptus") +
  theme_bw(base_size = 20)

ggplot(data %>%  filter(species == "sylvaticus"|species == "scriptus"|species =="sylvaticus/scriptus") %>% filter(!is.na(sex)), aes(x=n_distinct_stripes, fill=sex, color=sex)) +
  geom_density(alpha = 0.4) +
  labs(x="Number of Distinct Stripe", title="T. spekii") +
  theme_bw(base_size = 20)


ggplot() +
  geom_point(
    data=data %>%  filter(species == "sylvaticus"|species == "scriptus"|species =="sylvaticus/scriptus"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. sylvaticus/scriptus",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )


ggplot(final.data_withPredictors  %>%  filter(species == c("scriptus","sylvaticus","sylvaticus/scriptus")), aes(x=TsetsePresencePROB,y=n_distinct_stripes))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Probability of Tsetse Presence", y ="Number of Vertical Stripes") +
  theme_bw(base_size = 20)

ggplot(final.data_withPredictors  %>%  filter(species == c("oryx")), aes(x=TsetsePresencePROB,y=n_distinct_stripes))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Probability of Tsetse Presence", y ="Number of Vertical Stripes") +
  theme_bw(base_size = 20)

ggplot(final.data_withPredictors  %>%  filter(species == "spekii"), aes(x=TsetsePresencePROB,y=n_distinct_stripes))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Probability of Tsetse Presence", y ="Number of Vertical Stripes") +
  theme_bw(base_size = 20)


ggplot(final.data_withPredictors, aes(x=TabanidActivity,y=n_distinct_stripes))+ geom_point(size = 4, col = "cornflowerblue", alpha = 0.5) +
  geom_smooth(method = lm, se=FALSE) +
  labs(x="Probability of Tsetse Presence", y ="Number of Vertical Stripes") +
  theme_bw(base_size = 20)

ggplot() +
  geom_point(
    data=final.data_withPredictors %>% filter(species=="spekii"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2, alpha = 1) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. spekii",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )

ggplot() +
  geom_point(
    data=final.data_withPredictors %>% filter(species=="oryx"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2, alpha = 1) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. oryx",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )

ggplot() +
  geom_point(
    data=final.data_withPredictors %>% filter(species=="angasii"), 
    aes(x=longitude,y=latitude, col=n_distinct_stripes), 
    size = 2, alpha = 1) +
  # Add polygon of Africa
  geom_sf(data = africa, fill = "gray90", color = "black", alpha = 0.5) +
  # Add a color scale for points
  scale_color_continuous(type = "viridis") +
  # Customize the plot appearance
  theme_minimal() +
  labs(
    title = "Variation in Stripe Number - T. angasii",
    x = "Longitude",
    y = "Latitude",
    color = "Number of Distinct Stripes"
  )

ggplot(data = africa) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Map of Africa", x = "Longitude", y = "Latitude")


## 