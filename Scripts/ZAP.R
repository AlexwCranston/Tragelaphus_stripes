library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)
library(glmmTMB)
library(ggeffects)
library(parameters)


# Read in stripe data

data_raw <- read.csv("Data_Backup/Working Copy/Processed Data/Combined Dataset_2025_02_21.csv")

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
ggplot(data, aes(x=relative_n_stripes)) + geom_histogram(binwidth=1, colour="black", fill="darkgrey") +
  labs(title="Stripe Data is Heavily Zero-Inflated",x="Relative Number Of Stripes (Rounded)", y = "Count") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5))

# Scale and centre (where required fixed effects)

data$TsetsePresencePROB <- scale(data$TsetsePresencePROB, center = FALSE)
data$TabanidActivity <- scale(data$TabanidActivity, center = FALSE)
data$PNV <- scale(data$PNV, center = TRUE)



# Run the model with random effects only, i.e. only species and sex (nested within species) ####


glmm.randomONLY.ZAP <- glmmTMB(relative_n_stripes ~ 1 + 
                                 (1 | species) +               # Random intercept for species
                                 (1 | species:sex),            # Random intercept for sex within species
                               ziformula = ~ 1 + 
                                 (1 | species) +               # Random intercept for species
                                 (1 | species:sex),
                               family = truncated_poisson,
                               data = data)

performance::check_overdispersion(glmm.randomONLY.ZAP) # No overdispersion detected

# Add fixed effects and select final model ####

glmm.fixed.ZAP <- glmmTMB(relative_n_stripes ~ TsetsePresencePROB + 
                             TabanidActivity +
                             PNV +  
                             (1 | species) + (1 | species:sex),
                           ziformula = ~ TsetsePresencePROB + 
                             TabanidActivity +
                             PNV  + (1 | species) + (1|species:sex),  # Model excess zeros
                           family = truncated_poisson,
                           data = data)

performance::check_overdispersion(glmm.fixed.ZAP) # No overdispersion detected

AIC(glmm.randomONLY.ZAP,glmm.fixed.ZAP)

glmm.dropTabanid.ZAP <- glmmTMB(relative_n_stripes ~ TsetsePresencePROB + 
                                   PNV +  
                                  (1 | species) + (1 | species:sex),
                                ziformula = ~ TsetsePresencePROB + 
                                  PNV  + (1 | species) + (1|species:sex),  # Model excess zeros
                                family = truncated_poisson,
                                data = data)

glmm.dropPNV.ZAP <- glmmTMB(relative_n_stripes ~ TsetsePresencePROB + 
                                  TabanidActivity +  
                                  (1 | species) + (1 | species:sex),
                                ziformula = ~ TsetsePresencePROB + 
                                  TabanidActivity +
                                  (1 | species) + (1|species:sex),  # Model excess zeros
                                family = truncated_poisson,
                                data = data)

glmm.dropTsetse.ZAP <- glmmTMB(relative_n_stripes ~ TabanidActivity + 
                                 PNV +  
                              (1 | species) + (1 | species:sex),
                            ziformula = ~ TabanidActivity +
                              PNV  + (1 | species) + (1|species:sex),  # Model excess zeros
                            family = truncated_poisson,
                            data = data)

AIC(glmm.randomONLY.ZAP,glmm.fixed.ZAP, glmm.dropTabanid.ZAP, glmm.dropPNV.ZAP, glmm.dropTsetse.ZAP)
BIC(glmm.randomONLY.ZAP,glmm.fixed.ZAP, glmm.dropTabanid.ZAP, glmm.dropPNV.ZAP, glmm.dropTsetse.ZAP)

### Overall, glmm.dropTabanid.ZAP seems like the best model

summary(glmm.dropTabanid.ZAP)


# Plot random intercepts for species effect, firt for the conditional part of the model ####


species.cond.intercept <- ranef(glmm.dropTabanid.ZAP)$cond$species %>%
  as_tibble(rownames = "species")


ggplot(species.cond.intercept, aes(x = species, y = `(Intercept)`, color=species)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Intercepts by Species",
       y = "Deviation from Overall Intercept") +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title=element_text(hjust=0.5))


mean_values_Tsetse_species <- data %>%
  group_by(species) %>%
  summarise(
    Tsetse_mean_value = mean(TsetsePresencePROB))


cor.test.data.species.cond <-  full_join(species.cond.intercept, mean_values_Tsetse_species,
                            by = "species")

ggplot(cor.test.data.species.cond, aes(x = Tsetse_mean_value, y = `(Intercept)`, color=species)) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw()

  
cor.test(cor.test.data.species.cond$Tsetse_mean_value, cor.test.data$`(Intercept)`, test = "pearson")

### Now the binomial ####

species.binom.intercept <- ranef(glmm.dropTabanid.ZAP)$zi$species %>%
  as_tibble(rownames = "species")


ggplot(species.binom.intercept, aes(x = species, y = `(Intercept)`, color = species)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Intercepts by Species",
       y = "Deviation from Overall Intercept") +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus scriptus" = "#A6761D", "scriptus sylvaticus" = "#666666",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


cor.test.data.species.binom<-  full_join(species.binom.intercept, mean_values_Tsetse_species,
                                            by = "species")


ggplot(cor.test.data.species.binom, aes(x = Tsetse_mean_value, y = `(Intercept)`, color=species)) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus scriptus" = "#A6761D", "scriptus sylvaticus" = "#666666",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw()


cor.test(cor.test.data.species.binom$Tsetse_mean_value, cor.test.data.species.binom$`(Intercept)`, test = "pearson")



summary(glmm.dropTabanid.ZAP)


# Get predictions for the count model ####

species_list <- unique(data$species)
sex_list <- unique(data$sex)
new_data_all <- expand.grid(
  TsetsePresencePROB = seq(min(data$TsetsePresencePROB, na.rm = TRUE), 
                           max(data$TsetsePresencePROB, na.rm = TRUE), 
                           length.out = 100),
  PNV = 0, 
  species = species_list, 
  sex = sex_list)

predict_Tsetse <- predict(glmm.dropTabanid.ZAP,
                          newdata = new_data_all,
                          type="conditional", se.fit = TRUE)

new_data_all$fit <- predict_Tsetse$fit
new_data_all$upper <- predict_Tsetse$fit + 1.96 * predict_Tsetse$se.fit
new_data_all$lower <- predict_Tsetse$fit - 1.96 * predict_Tsetse$se.fit


ggplot(new_data_all, aes(x = TsetsePresencePROB, y = fit, color = species, linetype = sex)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = species), alpha = 0.1, show.legend = FALSE) +
  facet_wrap(~species, scales = "fixed") +  
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  scale_fill_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  labs(title = "Predicted Effect of Tsetse Presence on Stripe Count by Species",
       x = "Tsetse Presence Probability",
       y = "Predicted Stripe Count") +
  theme_bw()


# Get predictions for the zero-inflation model
predict_Tsetse_zero <- predict(glmm.dropTabanid.ZAP,
                               newdata = new_data_all,
                               type="zprob", se.fit = TRUE)

new_data_all$fit.zero <- predict_Tsetse_zero$fit
new_data_all$upper.zero <- predict_Tsetse_zero$fit + 1.96 * predict_Tsetse_zero$se.fit
new_data_all$lower.zero <- predict_Tsetse_zero$fit - 1.96 * predict_Tsetse_zero$se.fit

# Plot
ggplot(new_data_all, aes(x = TsetsePresencePROB, y = fit.zero, color = species, linetype = sex)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower.zero, ymax = upper.zero, fill = species), alpha = 0.2, show.legend = FALSE) +
  facet_wrap(~species, scales = "fixed") +
  coord_cartesian(ylim = c(0,1)) +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  scale_fill_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092"))+
  labs(x = "Tsetse Presence Probability", 
       y = "Probability of Having Zero Stripes",
       title = "Effect of Tsetse Flies on Zero Stripes (Hurdle Model)") +
  theme_bw()




# Subspecies Model ####

glmm.dropTabanid.ZAP_subspecies <- glmmTMB(relative_n_stripes ~ TsetsePresencePROB + 
                                  PNV +  
                                  (1 | revised_subspecies) + (1 | revised_subspecies:sex),
                                ziformula = ~ TsetsePresencePROB + 
                                  PNV  + (1 | revised_subspecies) + (1|revised_subspecies:sex),  # Model excess zeros
                                family = truncated_poisson,
                                data = data)


# Plot random intercepts for subspecies effect
# First for conditional part of the model ####


subspecies.cond.intercept <- ranef(glmm.dropTabanid.ZAP_subspecies)$cond$revised_subspecies %>%
  as_tibble(rownames = "revised_subspecies")

subspecies.cond.intercept$revised_subspecies <- factor(
  subspecies.cond.intercept$revised_subspecies, 
  levels = c("angasii", 
             "buxtoni", 
             "derbianus", "gigas",
             "eurycerus", "isaaci",
             "australis","imberbis",
             "livingstonii", "oryx", "pattersonianus",
             "bor", "decula", "phaleratus", "scriptus",
             "dama", "delamerei", "fasciatus", 
             "meneliki", "ornatus", "roualeynei","sylvaticus",
             "gratus","selousi","spekii",
             "chora", "cottoni", "strepsiceros"))

subspecies.cond.intercept <- subspecies.cond.intercept %>%
  mutate(species = case_when(
    revised_subspecies %in% c("angasii") ~ "angasii",
    revised_subspecies %in% c("buxtoni") ~ "buxtoni",
    revised_subspecies %in% c("derbianus", "gigas") ~ "derbianus",
    revised_subspecies %in% c("eurycerus", "isaaci") ~ "eurycerus",
    revised_subspecies %in% c("australis", "imberbis") ~ "imberbis",
    revised_subspecies %in% c("livingstonii", "oryx", "pattersonianus") ~ "oryx",
    revised_subspecies %in% c("bor", "decula", "phaleratus", "scriptus") ~ "scriptus scriptus",
    revised_subspecies %in% c("dama", "delamerei", "fasciatus", "meneliki", "ornatus", "roualeynei","sylvaticus") ~ "scriptus sylvaticus",
    revised_subspecies %in% c("gratus", "selousi", "spekii") ~ "spekii",
    revised_subspecies %in% c("chora", "cottoni", "strepsiceros") ~ "strepsiceros",
    TRUE ~ NA_character_  # Assign NA if no match
  ))


ggplot(subspecies.cond.intercept, aes(x = revised_subspecies, y = `(Intercept)`, color = species)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Intercepts by Subspecies",
       y = "Deviation from Overall Intercept",
       x = "Subspecies") +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus scriptus" = "#A6761D", "scriptus sylvaticus" = "#666666",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))


mean_values_Tsetse_subspecies <- data %>%
  group_by(revised_subspecies) %>%
  summarise(
    Tsetse_mean_value = mean(TsetsePresencePROB))

mean_values_Tsetse_subspecies <- na.omit(mean_values_Tsetse_subspecies)

cor.test.data.subspecies.cond <-  full_join(subspecies.cond.intercept, mean_values_Tsetse_subspecies,
                                            by = "revised_subspecies")

ggplot(cor.test.data.subspecies.cond, aes(x = Tsetse_mean_value, y = `(Intercept)`, color=species)) +
  geom_point(size = 3) +
  labs(y = "Intercept",
       x = "Mean Tsetse Probability for Subspecies") +
  geom_smooth(method = "lm",color ="blue", se = FALSE) +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus scriptus" = "#A6761D", "scriptus sylvaticus" = "#666666",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw()
  

cor.test(cor.test.data.subspecies.cond$Tsetse_mean_value, cor.test.data.subspecies.cond$`(Intercept)`, method  = "pearson")
cor.test(cor.test.data.subspecies.cond$Tsetse_mean_value, cor.test.data.subspecies.cond$`(Intercept)`, method = "spearman")

# Now for the binomial part of the model ####


subspecies.binom.intercept <- ranef(glmm.dropTabanid.ZAP_subspecies)$zi$revised_subspecies %>%
  as_tibble(rownames = "revised_subspecies")


subspecies.binom.intercept$revised_subspecies <- factor(
  subspecies.binom.intercept$revised_subspecies, 
  levels = c("angasii", 
             "buxtoni", 
             "derbianus", "gigas",
             "eurycerus", "isaaci",
             "australis","imberbis",
             "livingstonii", "oryx", "pattersonianus",
             "bor", "decula", "phaleratus", "scriptus",
             "dama", "delamerei", "fasciatus", 
             "meneliki", "ornatus", "roualeynei","sylvaticus",
             "gratus","selousi","spekii",
             "chora", "cottoni", "strepsiceros"))

subspecies.binom.intercept <- subspecies.binom.intercept %>%
  mutate(species = case_when(
    revised_subspecies %in% c("angasii") ~ "angasii",
    revised_subspecies %in% c("buxtoni") ~ "buxtoni",
    revised_subspecies %in% c("derbianus", "gigas") ~ "derbianus",
    revised_subspecies %in% c("eurycerus", "isaaci") ~ "eurycerus",
    revised_subspecies %in% c("australis", "imberbis") ~ "imberbis",
    revised_subspecies %in% c("livingstonii", "oryx", "pattersonianus") ~ "oryx",
    revised_subspecies %in% c("bor", "decula", "phaleratus", "scriptus") ~ "scriptus scriptus",
    revised_subspecies %in% c("dama", "delamerei", "fasciatus", "meneliki", "ornatus", "roualeynei","sylvaticus") ~ "scriptus sylvaticus",
    revised_subspecies %in% c("gratus", "selousi", "spekii") ~ "spekii",
    revised_subspecies %in% c("chora", "cottoni", "strepsiceros") ~ "strepsiceros",
    TRUE ~ NA_character_  # Assign NA if no match
  ))


ggplot(subspecies.binom.intercept, aes(x = revised_subspecies, y = `(Intercept)`, color = species)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Intercepts by Subspecies (Binomial Model)",
       y = "Deviation from Overall Intercept",
       x = "Subspecies") +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus scriptus" = "#A6761D", "scriptus sylvaticus" = "#666666",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))



cor.test.data.subspecies.binom<-  full_join(subspecies.binom.intercept, mean_values_Tsetse_subspecies,
                                            by = "revised_subspecies")


ggplot(cor.test.data.subspecies.binom, aes(x = Tsetse_mean_value, y = `(Intercept)`, color=species)) +
  geom_point(size = 3) +
  labs(y = "Intercept",
       x = "Mean Tsetse Probability for Subspecies") +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus scriptus" = "#A6761D", "scriptus sylvaticus" = "#666666",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw()


cor.test(cor.test.data.subspecies.binom$Tsetse_mean_value, cor.test.data.subspecies.binom$`(Intercept)`, test = "pearson")

