# Load necessary packages

library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)

#Load data


data_raw <- read.csv("Data_Backup/Working Copy/Processed Data/Combined Dataset_2025_02_21.csv")

data_raw$species <- as.factor(data_raw$species) 
data_raw$species <- fct_recode(data_raw$species, "scriptus" = "sylvaticus") # Make all bushbuck same species

data_raw <- data_raw %>% dplyr::filter(age == "adult"| is.na(age))


## data <- data %>% mutate(revised.sex = ifelse(is.na(sex), "Undetermined", sex)) # Define NAs in sex as "undetermined"

data <- data_raw %>% drop_na(TsetsePresencePROB) %>% 
  drop_na(species)   #Drop NAs for fixed effects

# data$relative_n_stripes <- data$n_distinct_stripes/data$skin_length_m # Add column for number of stripes relative to bodysize


data$TsetsePresencePROB <- scale(data$TsetsePresencePROB, center = TRUE, scale = FALSE)
data$n_distinct_stripes <- scale(data$n_distinct_stripes, center = TRUE, scale = FALSE)


data <-data %>%
  group_by(species) %>%
  summarise(Tsetse_mean_value = mean(TsetsePresencePROB), stripe_mean_value = median(n_distinct_stripes, na.rm =TRUE)) %>%
  left_join(data, by="species")

data <- data %>% mutate(ContrastStripe = (n_distinct_stripes-stripe_mean_value)/abs(stripe_mean_value), 
                        ContrastTsetse = (TsetsePresencePROB-Tsetse_mean_value)/abs(Tsetse_mean_value)) 


ggplot(data, aes(x = ContrastTsetse, y = ContrastStripe, color=species)) +
  # Add shaded quadrants
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "green", alpha = 0.1) +  # Upper right (supports)
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "green", alpha = 0.1) +  # Lower left (supports)
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf, fill = "red", alpha = 0.1) +  # Upper left (contradicts)
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0, fill = "red", alpha = 0.1) +  # Lower right (contradicts)
  geom_point(aes(color = species), alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # Horizontal reference line
  geom_vline(xintercept = 0, linetype = "dashed") +  # Vertical reference line
  geom_smooth(method = "lm",color ="blue", se = FALSE) +
  scale_shape_manual(values = c(17, 19), labels = c("Contradicts", "Supports")) +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  labs(x="Relative Deviation from Mean - Tsetse Presence", y="Relative Deviation from Mean - Number of Distinct Stripes") +
theme_bw()



pearson_test <- cor.test(data$ContrastStripe, data$ContrastTsetse, method = "pearson")
pearson_test



spearman_test <- cor.test(data$ContrastStripe, data$ContrastTsetse, method = "spearman")
spearman_test


# Count the number of supporting points
supporting_points <- sum((data$ContrastStripe * data$ContrastTsetse) > 0)

# Compute proportion
proportion_supporting <- supporting_points / nrow(data)

