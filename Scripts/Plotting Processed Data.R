hist(final.data_withPredictors$n_distinct_stripes)

final.data_withPredictors<-read.csv("Data_Backup/Working Copy/Processed DataCombined Dataset_specimen_info_2025_01_24_withPredictorVariables.csv")


iNat.data <- read.csv("Data_backup/Working Copy/iNat/Stripe-athon Datasheet_2025_01_24_specimen_measurements.csv")


#Read in shapefile for Africa
africa <- st_read("Geometries/Africa_cropped.shp")
plot(africa)


ggplot(final.data_withPredictors %>%  filter(sex == c("male","female")), aes(x=sex,y=n_distinct_stripes))+ geom_jitter() +
  labs(x="Sex", y ="Number of Distinct Stripes") +
  theme_bw(base_size = 20)

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