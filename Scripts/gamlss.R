
library(gamlss)
library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)


data_raw <- read.csv("Data_Backup/Working Copy/Processed Data/Combined Dataset_2025_02_21.csv")

data_raw$species <- as.factor(data_raw$species) 
data_raw$sex <- as.factor(data_raw$sex) 
data_raw$species <- fct_recode(data_raw$species, "scriptus" = "sylvaticus") # Make all bushbuck same species

## data <- data %>% mutate(revised.sex = ifelse(is.na(sex), "Undetermined", sex)) # Define NAs in sex as "undetermined"

data <- data_raw %>% drop_na(TsetsePresencePROB) %>% 
  drop_na(species) %>% 
  drop_na(PNV) %>%
  drop_na(sex)  #Drop NAs for fixed effects

data <- data %>% filter(age == "adult"| is.na(age))

data$relative_n_stripes <- data$n_distinct_stripes/data$skin_length_m # Add column for number of stripes relative to bodysize

data <- data %>% select(c("relative_n_stripes","TsetsePresencePROB","PNV","species","sex"))

model <- gamlss(formula = relative_n_stripes ~ TsetsePresencePROB + 
                  PNV +  random (species) + random (species:sex),
                nu.formula = relative_n_stripes ~ TsetsePresencePROB + 
                  PNV +  random (species) + random (species:sex),
                family = ZAGA, data = data)

summary(model)
