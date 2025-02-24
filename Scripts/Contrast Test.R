# Load necessary packages

library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)

#Load data


data_raw <- read.csv("Data_Backup/Working Copy/Processed Data/Combined Dataset_2025_02_21.csv")

data_raw$species <- as.factor(data_raw$species) 
data_raw$species <- fct_recode(data_raw$species, "scriptus" = "sylvaticus") # Make all bushbuck same species

## data <- data %>% mutate(revised.sex = ifelse(is.na(sex), "Undetermined", sex)) # Define NAs in sex as "undetermined"

data <- data_raw %>% drop_na(TsetsePresencePROB) %>% 
  drop_na(species)   #Drop NAs for fixed effects

mean_values_Tsetse_species <- data %>%
  group_by(species) %>%
  summarise(
    Tsetse_mean_value = mean(TsetsePresencePROB))

mean_values_stripe_species <- data %>%
  group_by(species) %>%
  summarise(
    stripe_mean_value = mean(n_distinct_stripes))

both_means <-  full_join(mean_values_Tsetse_species, mean_values_stripe_species,
                            by = "species")

data_withmean <- full_join(data, both_means,
                           by = "species")


data_withmean$contrastStripes <- (data_withmean$n_distinct_stripes - data_withmean$stripe_mean_value) / data_withmean$stripe_mean_value  # Relative contrast
data_withmean$contrastTsetse <- (data_withmean$TsetsePresencePROB - data_withmean$Tsetse_mean_value) / data_withmean$Tsetse_mean_value  # Relative contrast

pearson_test <- cor.test(data_withmean$contrastStripes, data_withmean$contrastTsetse, method = "pearson")

pearson_test

# Pearson's product-moment correlation

#  data:  data_withmean$contrastStripes and data_withmean$contrastTsetse
#   t = 12.519, df = 1322, p-value < 2.2e-16
#  alternative hypothesis: true correlation is not equal to 0
#  95 percent confidence interval:
#  0.2765440 0.3729003
#  sample estimates:
#      cor 
#  0.3255672 