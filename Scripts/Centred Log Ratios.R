# Load necessary packages

library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)

#Load data


data_raw <- read.csv("Data_Backup/Working Copy/Processed Data/Combined Dataset_2025_03_17.csv")

data_raw$species <- as.factor(data_raw$species) 
data_raw$sex <- as.factor(data_raw$sex) 

data_raw$species <- fct_recode(data_raw$species, "scriptus" = "sylvaticus") # Make all bushbuck same species

## data <- data %>% mutate(revised.sex = ifelse(is.na(sex), "Undetermined", sex)) # Define NAs in sex as "undetermined"

data <- data_raw %>% drop_na(TsetsePresencePROB) %>% 
  drop_na(species) %>% 
  drop_na(TabanidActivity) %>%
  drop_na(sex)  #Drop NAs for fixed effects

data <- data %>% filter(age == "adult"| is.na(age))

data$relative_n_stripes <- data$n_distinct_stripes/data$skin_length_m # Add column for number of stripes relative to bodysize
data$relative_n_stripes <- round(data$relative_n_stripes, 0) # Round so these are whole integers


k <- 0.001

data <-data %>%
  group_by(species) %>%
  summarise(Tsetse_mean_value = mean(TsetsePresencePROB), stripe_mean_value = median(relative_n_stripes, na.rm =TRUE),
            PNV_mean_value = mean(PNV), Tabanid_mean_value = mean(TabanidActivity)) %>%
  left_join(data, by="species")

data <- data %>% mutate(ContrastStripe = log((relative_n_stripes+k)/(stripe_mean_value+k)), 
                        ContrastTsetse = log((TsetsePresencePROB+k)/(Tsetse_mean_value+k)),
                        ContrastPNV = log((PNV+k)/(PNV_mean_value+k)),
                        ContrastTabanid = log((TabanidActivity+k)/(Tabanid_mean_value+k))) # Log ratios
                                              
                                     
ggplot(data, aes(x = ContrastTabanid, y = ContrastStripe, color=species)) +
  # Add shaded quadrants
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "green", alpha = 0.1) +  
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "green", alpha = 0.1) +  
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf, fill = "red", alpha = 0.1) +  
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0, fill = "red", alpha = 0.1) +
  geom_point(aes(color = species), alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") +  
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  labs(x="Centred Log Ratio - Probability of Tsetse Presence", y="Centred Log Ratio - Relative Stripe Number") +
  theme_bw()



spearman_test.Tsetse <- cor.test(data$ContrastStripe, data$ContrastTsetse, method = "spearman")
spearman_test.PNV <- cor.test(data$ContrastStripe, data$ContrastPNV, method = "spearman")
spearman_test.Tabanid <- cor.test(data$ContrastStripe, data$ContrastTabanid, method = "spearman")


spearman_test.Tsetse
spearman_test.PNV
spearman_test.Tabanid

## Tsetse and Tabanid are 
# Spearman's rank correlation rho

# data:  data$ContrastStripe and data$ContrastTsetse
# S = 213230116, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.4140105  

# Count the number of supporting points
supporting_points <- sum((data$ContrastStripe * data$ContrastTsetse) > 0)

# Compute proportion
proportion_supporting <- supporting_points / nrow(data)
