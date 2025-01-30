
library(raster)
library(sf)
library(tidyr)
library(dplyr)

TsetsePresence<-raster("Rasters/Outputs/AllGroupsPROBwNULL.tif") # Read in the raster for Tsetse Fly Distribution


TabanidActivity<-raster("Rasters/Outputs/MonthsTabanidActivitywNUL.tif") # Read in the raster for the Tabanid Fly Activity



### Here I've made some random data around Africa with radiuses indicating uncertainty
### Substitute this with a line reading in your georeferenced points, which will also have a measure of uncertainty (I've assumed this is in metres by the way, so convert if needed) 

points_df <- read.csv("Data_backup/Stripes Datasheet_specimen_info_24_05_15.csv")

# Drop any points without a latitude/longitude

points_df<-points_df %>% drop_na(longitude) %>% drop_na(latitude)

# Lets see how many come from each country

country.n<-points_df %>% 
  group_by(country) %>%
  summarise(no_rows = length(country))

#convert extent from km to metres 


points_df$extent_m <- points_df$extent_km*1000 


#Remove all points where uncertainty is greater than 200 km

points_df <- points_df %>% dplyr::filter(extent_km < 200)

# Turn the data frame into an sf object with the CRS WGS84 

points_sf <- st_as_sf(points_df, coords = c("longitude", "latitude"), crs = 4326)

# Create circular buffers (circles) around points based on radius in meters to represent uncertainty over exact location

circles_sf <- st_buffer(points_sf, dist = points_df$extent_m)
circles_sf_plot <- st_buffer(points_sf, dist = 50000)

# Plot the result to check it all looks good

plot(TsetsePresence)
plot(circles_sf_plot, pch = 16, col = "red", add=TRUE)


plot(TabanidActivity, xlim =c(-20,55), ylim=c(-40,40))
plot(circles_sf_plot, pch = 16, col = "red", add=TRUE)


# Use the extract function from the package raster to take values from the raster that are covered by the circle. 
# Using fun=mean will return the mean value covered by each cirlce. df = TRUE tells the output to be a dataframe

TsetsePresence_points <- raster::extract(TsetsePresence, circles_sf, fun=mean, df=TRUE, na.rm = TRUE)
TabanidActivity_points <- raster::extract(TabanidActivity, circles_sf, fun=mean, df=TRUE, na.rm = TRUE)


# Bind these with the orignal data

final_df <- cbind(TsetsePresence_points, TabanidActivity_points, points_df)

## Now save your extracted data as a dataframe

hist(final_df$AllGroupsPROBwNULL)
hist(final_df$MonthsTabanidActivitywNUL)


# Drop defunct columns for ID

final_df <- final_df %>% select(-ID)

# Rename some columns 


final_df<-final_df %>% 
  rename(
    TsetsePROB = AllGroupsPROBwNULL ,
    MonthsTabanidActivity = MonthsTabanidActivitywNUL 
  )
 
write.csv(final_df, file="Data_backup/Stripes Datasheet_specimen_info_24_05_15_withPredictorVariables.csv")

