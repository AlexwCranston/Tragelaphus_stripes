
library(raster)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)

#### First, read and edit in iNaturalist data ####

iNat.data <- read.csv("Data_backup/Working Copy/iNat/Stripe-athon Datasheet_2025_02_14_specimen_measurements.csv")
iNat.data <- iNat.data %>% dplyr::filter(flag != "yes") # Remove any rows which have been flagged (i.e. where there is any kind of issue with the record)

names(iNat.data)[names(iNat.data) == "revised_uncertainty_km"] <- "extent_km" # Changing column names to make collection datasets for later merging 

iNat.data$source <- "iNat" # Add a column for data source 

# Read in and attach stripe data from iNat Stripe Data


# First vertical stripes
iNat.vertical.stripe<- read.csv("Data_backup/Working Copy/iNat/Stripe-athon Datasheet_2025_02_14_stripes_vertical.csv") 
iNat.vertical.stripe <- iNat.vertical.stripe %>% dplyr::filter(flag != "yes") # Remove any rows which have been flagged (i.e. where there is any kind of issue with the record)
iNat.vertical.stripe$stripe_distinctness_numerical<-ifelse(iNat.vertical.stripe$stripe_distinctness=="no",0,1) # Assign each stripe a one or a zero depending on distinctness
iNat.vertical.stripe.by<-by(iNat.vertical.stripe$stripe_distinctness_numerical, iNat.vertical.stripe$ï..specimen_id, mean) # Average the ones and zeros for each unique id to give a percent distinctiveness
iNat.vertical.stripe.by<-as.data.frame(iNat.vertical.stripe.by)
iNat.vertical.stripe.by <- cbind(rownames(iNat.vertical.stripe.by), data.frame(iNat.vertical.stripe.by, row.names=NULL))
iNat.vertical.stripe.by<-rename(iNat.vertical.stripe.by, "ï..specimen_id"="rownames(iNat.vertical.stripe.by)","vertical_percent_distinct"="x")

# Now horizontal

iNat.horizontal.stripe<- read.csv("Data_backup/Working Copy/iNat/Stripe-athon Datasheet_2025_02_14_stripes_horizontal.csv") 
iNat.horizontal.stripe <- iNat.horizontal.stripe %>% dplyr::filter(flag != "yes") # Remove any rows which have been flagged (i.e. where there is any kind of issue with the record)
iNat.horizontal.stripe$stripe_distinctness_numerical<-ifelse(iNat.horizontal.stripe$stripe_distinctness=="no",0,1)
iNat.horizontal.stripe.by<-by(iNat.horizontal.stripe$stripe_distinctness_numerical, iNat.horizontal.stripe$ï..specimen_id, mean) # Average the ones and zeros for each unique id to give a percent distinctiveness
iNat.horizontal.stripe.by<-as.data.frame(iNat.horizontal.stripe.by)
iNat.horizontal.stripe.by <- cbind(rownames(iNat.horizontal.stripe.by), data.frame(iNat.horizontal.stripe.by, row.names=NULL))
iNat.horizontal.stripe.by<-rename(iNat.horizontal.stripe.by, "ï..specimen_id"="rownames(iNat.horizontal.stripe.by)","horizontal_percent_distinct"="x")



percent_distinct <- full_join(iNat.horizontal.stripe.by, iNat.vertical.stripe.by,
                       by = "ï..specimen_id")
iNat.data <- full_join(iNat.data, percent_distinct,
                              by = "ï..specimen_id")


iNat.data <- iNat.data%>% drop_na(extent_km)
iNat.data <- iNat.data %>% dplyr::filter(extent_km < 200)
names(iNat.data)[names(iNat.data) == "number_vertstripes"] <- "number_vertstripes_MODEL" # Changing column names to match other dataset
names(iNat.data)[names(iNat.data) == "number_horizontal_stripes"] <- "number_horizontal_stripes_MODEL" # Changing column names to match other dataset


#### Now the same with the collections data ####
                       
collections.data <- read.csv("Data_backup/Working Copy/Collections/Stripes Datasheet_2025_01_31_specimen_info.csv")

collections.data <- collections.data %>% dplyr::filter(flag != "yes") # Remove any rows which have been flagged (i.e. where there is any kind of issue with the record)

collections.data$source <- "collection" 

# Combine the collections specimen info with the specimen measurements

collections.data_2 <- read.csv("Data_backup/Working Copy/Collections/Stripes Datasheet_2025_01_31_specimen_measurements.csv")
collections.data_2 <- collections.data_2 %>% dplyr::filter(flag != "yes") # Same as above, remove any rows which have been flagged (i.e. where there is any kind of issue with the record)


collections.data <- merge(collections.data, collections.data_2, by = "ï..specimen_id") 

## We don't need the averages of stripes and spots on the left and right side of the specimens, we will use right side only as we only have percentage distinctiveness from this side
## collections.data <- collections.data %>%
##  mutate(mean_number_vertstripes = 
##           rowMeans(data.frame(collections.data$number_vertstripes_right,collections.data$number_vertstripes_left), 
##                    na.rm = TRUE)) # Take the average number of stripes on either side

## collections.data <- collections.data %>%
##  mutate(mean_number_horizontal_stripes = 
##           rowMeans(data.frame(collections.data$number_horizontal_stripes_right,collections.data$number_horizontal_stripes_left), 
##                  na.rm = TRUE)) # Take the average number of stripes on either side # Take the average number of stripes on either side


collections.data <- collections.data %>%
 mutate(number_spots = 
           rowMeans(data.frame(collections.data$number_spots_left,collections.data$number_spots_right), 
                   na.rm = TRUE)) %>%
  mutate(number_spots=round(number_spots, 0)) # Take the average number of spots on either side, round to nearest whole number for ease of use in poisson distribution

# Calculate and Add percent distinctiveness


# First vertical stripes
collections.vertical.stripe<- read.csv("Data_backup/Working Copy/Collections/Stripes Datasheet_2025_01_31_stripes_vertical.csv") 
collections.vertical.stripe <- collections.vertical.stripe %>% dplyr::filter(flag != "yes") # Remove any rows which have been flagged (i.e. where there is any kind of issue with the record)
collections.vertical.stripe$stripe_distinctness_numerical<-ifelse(collections.vertical.stripe$stripe_distinctness=="no",0,1) # Assign each stripe a one or a zero depending on distinctness
collections.vertical.stripe.by<-by(collections.vertical.stripe$stripe_distinctness_numerical, collections.vertical.stripe$ï..specimen_id, mean) # Average the ones and zeros for each unique id to give a percent distinctiveness
collections.vertical.stripe.by<-as.data.frame(collections.vertical.stripe.by)
collections.vertical.stripe.by <- cbind(rownames(collections.vertical.stripe.by), data.frame(collections.vertical.stripe.by, row.names=NULL))
collections.vertical.stripe.by<-rename(collections.vertical.stripe.by, "ï..specimen_id"="rownames(collections.vertical.stripe.by)","vertical_percent_distinct"="x")

# Now horizontal

collections.horizontal.stripe<- read.csv("Data_backup/Working Copy/Collections/Stripes Datasheet_2025_01_31_stripes_horizontal.csv") 
collections.horizontal.stripe <- collections.horizontal.stripe %>% dplyr::filter(flag != "yes") # Remove any rows which have been flagged (i.e. where there is any kind of issue with the record)
collections.horizontal.stripe$stripe_distinctness_numerical<-ifelse(collections.horizontal.stripe$stripe_distinctness=="no",0,1)
collections.horizontal.stripe.by<-by(collections.horizontal.stripe$stripe_distinctness_numerical, collections.horizontal.stripe$ï..specimen_id, mean) # Average the ones and zeros for each unique id to give a percent distinctiveness
collections.horizontal.stripe.by<-as.data.frame(collections.horizontal.stripe.by)
collections.horizontal.stripe.by <- cbind(rownames(collections.horizontal.stripe.by), data.frame(collections.horizontal.stripe.by, row.names=NULL))
collections.horizontal.stripe.by<-rename(collections.horizontal.stripe.by, "ï..specimen_id"="rownames(collections.horizontal.stripe.by)","horizontal_percent_distinct"="x")



percent_distinct <- full_join(collections.horizontal.stripe.by, collections.vertical.stripe.by,
                              by = "ï..specimen_id")

collections.data <- full_join(collections.data, percent_distinct,
                       by = "ï..specimen_id")


collections.data <- collections.data %>% dplyr::filter(extent_km < 200)
names(collections.data)[names(collections.data) == "revised_species"] <- "species" # Changing column names to match other dataset
names(collections.data)[names(collections.data) == "number_vertstripes_right"] <- "number_vertstripes_MODEL" # Changing column names to match iNat. We took distinctness measurements on the right side of the skins only, so this is the data we will use in the model
names(collections.data)[names(collections.data) == "number_horizontal_stripes_right"] <- "number_horizontal_stripes_MODEL" # Changing column names as above


#### Combine iNat and Collections Data ####

common_col_names <- intersect(names(collections.data), names(iNat.data))
collections.data_final <- collections.data %>% dplyr::select(all_of(common_col_names)) 

iNat.data_final <- iNat.data %>% dplyr::select(all_of(common_col_names))

final.data <- rbind(iNat.data_final, collections.data_final)

final.data <- final.data %>% dplyr::select(-c("legs_white","midline_white", "face_white","throat_white")) # Drop columns that we don't want
glimpse(final.data) # Check that everything is as we expect


# Change the NAs in percent distinctiveness to zeros to prevent NAs being introduced in calculations

final.data <- final.data %>% mutate(horizontal_percent_distinct = ifelse(is.na(horizontal_percent_distinct), 0, horizontal_percent_distinct),
                                                                  vertical_percent_distinct = ifelse(is.na(vertical_percent_distinct), 0, vertical_percent_distinct))


## Calculate number of distinct stripes 

final.data <- final.data %>% mutate(n_distinct_stripes = ((number_vertstripes_MODEL*vertical_percent_distinct) + (number_horizontal_stripes_MODEL*horizontal_percent_distinct)))
final.data <- final.data %>% mutate(n_distinct_stripes=round(n_distinct_stripes, 0)) # We'll remove the need for this step later when the data is fully clean but for now, round to the nearest number to ensure only integers so that we can use poisson distribution


## Predictor variables

TsetsePresence<-raster("Rasters/Outputs/AllGroupsPROBwNULL.tif") # Read in the raster for Tsetse Fly Distribution


TabanidActivity<-raster("Rasters/Outputs/MonthsTabanidActivitywNUL.tif") # Read in the raster for the Tabanid Fly Activity

PNV <- raster("Rasters/PNV_Hengl/pnv_fapar_proba.v.annual_d_1km_s0..0cm_2014..2017_v0.1.tif") 

#convert extent from km to metres 

final.data$extent_m <- final.data$extent_km*1000 

# Turn the data frame into an sf object with the CRS WGS84 

final.data_sf <- st_as_sf(final.data, coords = c("longitude", "latitude"), crs = 4326)

# Create circular buffers (circles) around points based on radius in meters to represent uncertainty over exact location

circles_sf <- st_buffer(final.data_sf, dist = final.data_sf$extent_m)

# Plot the result to check it all looks good

plot(TsetsePresence)
plot(circles_sf, pch = 16, col = "red", add=TRUE)

plot(TabanidActivity, xlim =c(-20,55), ylim=c(-40,40))
plot(circles_sf, pch = 16, col = "red", add=TRUE)

plot(PNV, xlim =c(-20,55), ylim=c(-40,40))
plot(circles_sf, pch = 16, col = "red", add=TRUE)

# Use the extract function from the package raster to take values from the raster that are covered by the circle. 
# Using fun=mean will return the mean value covered by each cirlce. df = TRUE tells the output to be a dataframe

TsetsePresence_points <- raster::extract(TsetsePresence, circles_sf, fun=mean, df=TRUE, na.rm = TRUE)
TabanidActivity_points <- raster::extract(TabanidActivity, circles_sf, fun=mean, df=TRUE, na.rm = TRUE)
PNV_points <- raster::extract(PNV, circles_sf, fun=mean, df=TRUE, na.rm = TRUE)



# Bind these with the original data

final.data_withPredictors <- cbind(TsetsePresence_points, TabanidActivity_points, PNV_points, final.data)

final.data_withPredictors <- final.data_withPredictors %>%
  dplyr::select(-matches("^ID$"))

# Rename some columns 


final.data_withPredictors<-final.data_withPredictors %>% 
  rename(
    TsetsePresencePROB = fuscgroup ,
    TabanidActivity = RHpm01,
    PNV = pnv_fapar_proba.v.annual_d_1km_s0..0cm_2014..2017_v0.1,
    specimen_ID = ï..specimen_id
  )

### Some Points are missing, we can visualise them below

na_points <- final.data_withPredictors %>% 
  dplyr::filter(is.nan(final.data_withPredictors$TsetsePresencePROB))
na_sf <- st_as_sf(na_points, coords = c("longitude", "latitude"), crs = 4326)

na_points <- final.data_withPredictors %>% 
  dplyr::filter(is.nan(final.data_withPredictors$TabanidActivity))
na_sf <- st_as_sf(na_points, coords = c("longitude", "latitude"), crs = 4326)

na_points <- final.data_withPredictors %>% 
  dplyr::filter(is.nan(final.data_withPredictors$PNV))
na_sf <- st_as_sf(na_points, coords = c("longitude", "latitude"), crs = 4326)



plot(TsetsePresence)
plot(na_sf, pch = 16, col = "red", add=TRUE)

## Most points missing are values very close to the coast or on islands in lakes, lets replace the missing values with IDW 


final.data_withPredictors_sf <- st_as_sf(final.data_withPredictors, coords = c("longitude", "latitude"), crs = 4326) # First convert dataframe to spatial object

## Start with TsetsePresencePROB ####

# Split into known and missing points
known_points <- final.data_withPredictors_sf %>%
  filter(!is.nan(TsetsePresencePROB))

missing_points <- final.data_withPredictors_sf %>%
  filter(is.nan(TsetsePresencePROB))

# Convert to Spatial format for gstat
known_points_sp <- as(known_points, "Spatial")
missing_points_sp <- as(missing_points, "Spatial")


idw_model <- gstat::idw(TsetsePresencePROB ~ 1, known_points_sp, newdata = missing_points_sp, idp = 2)

missing_points$TsetsePresencePROB <- idw_model$var1.pred

final.data_withPredictors_sf <- bind_rows(known_points, missing_points)

## Next TabanidActivity ####

# Known values (non-NaN)
known_points <- final.data_withPredictors_sf %>%
  filter(!is.nan(TabanidActivity))

# Missing values (NaN)
missing_points <- final.data_withPredictors_sf %>%
  filter(is.nan(TabanidActivity))

# Convert to Spatial format for gstat
known_points_sp <- as(known_points, "Spatial")
missing_points_sp <- as(missing_points, "Spatial")


idw_model <- gstat::idw(TabanidActivity ~ 1, known_points_sp, newdata = missing_points_sp, idp = 2)

missing_points$TabanidActivity <- idw_model$var1.pred

final.data_withPredictors_sf <- bind_rows(known_points, missing_points)

# Next PNV ####

# Known values (non-NaN)
known_points <- final.data_withPredictors_sf %>%
  filter(!is.nan(PNV))

# Missing values (NaN)
missing_points <- final.data_withPredictors_sf %>%
  filter(is.nan(PNV))

# Convert to Spatial format for gstat
known_points_sp <- as(known_points, "Spatial")
missing_points_sp <- as(missing_points, "Spatial")


idw_model <- gstat::idw(PNV ~ 1, known_points_sp, newdata = missing_points_sp, idp = 2)

missing_points$PNV <- idw_model$var1.pred

final.data_withPredictors_sf <- bind_rows(known_points, missing_points)

## Now convert sf back to dataframe and export ####


# Convert sf object back to a dataframe
final.data_withPredictors <- final.data_withPredictors_sf %>%
  st_drop_geometry() %>%  # Removes the spatial component
  mutate(longitude = st_coordinates(final.data_withPredictors_sf)[, 1],  # Extract X (Longitude)
         latitude = st_coordinates(final.data_withPredictors_sf)[, 2])   # Extract Y (Latitude)


write.csv(final.data_withPredictors, file="Data_backup/Working Copy/Processed Data/Combined Dataset_2025_02_14.csv")

