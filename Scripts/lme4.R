library(lme4)
library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)
library(glmmTMB)
library(ggeffects)
library(parameters)


# Read in stripe data

data<- read.csv("Data_Backup/Working Copy/Processed Data/Combined Dataset_2025_02_14.csv")

data$species <- as.factor(data$species) 
data$species <- fct_recode(data$species, "scriptus" = "sylvaticus/scriptus", "scriptus" = "sylvaticus") # Make all bushbuck same species

## data <- data %>% mutate(revised.sex = ifelse(is.na(sex), "Undetermined", sex)) # Define NAs in sex as "undetermined"

data <- data %>% drop_na(TsetsePresencePROB) %>% 
  drop_na(species) %>% 
  drop_na(TabanidActivity) %>%
  drop_na(sex)  #Drop NAs for fixed effects

# Scale and centre (where required fixed effects)

data$TsetsePresencePROB <- scale(data$TsetsePresencePROB, center = FALSE)
data$TabanidActivity <- scale(data$TabanidActivity, center = FALSE)
data$PNV <- scale(data$PNV, center = TRUE)



# Run the model with random effects only, i.e. only species and sex (nested within species) ####

glmm.randomONLY <- glmer(n_distinct_stripes ~ 1 + 
                           (1 | species) +               # Random intercept for species
                           (1 | species:sex),            # Random intercept for sex within species
                         data = data, 
                         family = poisson())

summary(glmm.randomONLY)

species.specific.intercept <- ranef(glmm.randomONLY)$species %>%
  as_tibble(rownames = "species")

sex.by.species.specific.intercept <- ranef(glmm.randomONLY)$`species:sex` %>%
  as_tibble(rownames = "species_sex")

# Plot random intercepts

ggplot(species.specific.intercept, aes(x = species, y = `(Intercept)`)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Intercepts by Species",
       y = "Deviation from Overall Intercept") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Extract "male" or "female" from species_sex
sex.by.species.specific.intercept <- sex.by.species.specific.intercept %>%
  mutate(sex = ifelse(grepl(":male$", species_sex), "male", "female"))


ggplot(sex.by.species.specific.intercept, aes(x = species_sex, y = `(Intercept)`, color = sex)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Intercepts by Sex within Species",
       y = "Deviation from Overall Intercept") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("male" = "blue", "female" = "red"))


# Run the model with fixed effects added ####

glmm.fixed.poisson <- glmer(n_distinct_stripes ~ TsetsePresencePROB + 
                              TabanidActivity +
                              PNV +
                           (1 | species) +               # Random intercept for species
                           (1 | species:sex),            # Random intercept for sex within species
                         data = data, 
                         family = poisson())

performance::check_overdispersion(glmm.fixed.poisson) # Overdispersed, either because of zero-inflation or high variance in the count data

## Try again but with a zero-inflated poisson model (ZIP) ####
glmm.fixed.ZIP <- glmmTMB(n_distinct_stripes ~ TsetsePresencePROB + 
                                 TabanidActivity +
                                 PNV +
                                 (1 | species) +               # Random intercept for species
                                 (1 | species:sex),            # Randome intercept for sex within species
                                ziformula = ~1,  # Model excess zeros
                                family = poisson,
                                data = data)

performance::check_overdispersion(glmm.fixed.ZIP) # No overdispersion, using a zero-inflated model appears to have solved the problem

performance::check_model(glmm.fixed.ZIP) 

glmm.fixed.ZIP2 <- glmmTMB(n_distinct_stripes ~ TsetsePresencePROB + 
                                  TabanidActivity +
                                  PNV +  
                                 (1 | species) + (1 | species:sex),
                               ziformula = ~ TsetsePresencePROB + 
                                 TabanidActivity +
                                 PNV  + (1 | species) + (1|sex),  # Model excess zeros
                               family = poisson,
                               data = data)

performance::check_overdispersion(glmm.fixed.ZIP2) # No overdispersion, using a zero-inflated model appears to have solved the problem
performance::check_model(glmm.fixed.ZIP2)


glmm.fixed.ZIP3 <- glmmTMB(n_distinct_stripes ~ TsetsePresencePROB +  
                                  (1 | species) + (1 | species:sex),
                                ziformula = ~ TsetsePresencePROB + 
                                  TabanidActivity +
                                  PNV + sex  + (1 | species),  # Model excess zeros
                                family = poisson,
                                data = data)

performance::check_model(glmm.fixed.ZIP3)

AIC(glmm.fixed.ZIP, glmm.fixed.ZIP2, glmm.fixed.ZIP3)
BIC(glmm.fixed.ZIP, glmm.fixed.ZIP2, glmm.fixed.ZIP3)


## ZIP2 seems like the best model

glmm.dropTabanid.ZIP <- glmmTMB(n_distinct_stripes ~ TsetsePresencePROB + 
                             PNV +  
                             (1 | species) + (1 | species:sex),
                           ziformula = ~ TsetsePresencePROB + 
                             PNV +  
                             (1 | species) + (1 | sex),  # Model excess zeros
                           family = poisson,
                           data = data)

glmm.dropPNV.ZIP <- glmmTMB(n_distinct_stripes ~ TsetsePresencePROB + 
                             TabanidActivity +  
                             (1 | species) + (1 | species:sex),
                           ziformula = ~ TsetsePresencePROB + 
                             TabanidActivity +  
                             (1 | species) + (1 | sex),  # Model excess zeros
                           family = poisson,
                           data = data)

glmm.dropTsetse.ZIP <- glmmTMB(n_distinct_stripes ~ TabanidActivity +
                             PNV +  
                             (1 | species) + (1 | species:sex),
                           ziformula = ~ TabanidActivity +
                             PNV +  
                             (1 | species) + (1 | sex),  # Model excess zeros
                           family = poisson,
                           data = data)


AIC(glmm.fixed.ZIP2, glmm.dropTabanid.ZIP, glmm.dropPNV.ZIP, glmm.dropTsetse.ZIP)
BIC(glmm.fixed.ZIP2, glmm.dropTabanid.ZIP, glmm.dropPNV.ZIP, glmm.dropTsetse.ZIP)



summary(glmm.dropTabanid.ZIP)


# Plot random intercepts for species effect


species.specific.intercept_slope <- ranef(glmm.dropTabanid.ZIP)$cond$species %>%
  as_tibble(rownames = "species")

ggplot(species.specific.intercept_slope, aes(x = species, y = `(Intercept)`)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Intercepts by Species",
       y = "Deviation from Overall Intercept") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Species/sex interaction effect

sex.by.species.specific.intercept <- ranef(glmm.dropTabanid.ZIP)$cond$`species:sex` %>%
  as_tibble(rownames = "species_sex")

# Extract "male" or "female" from species_sex
sex.by.species.specific.intercept <- sex.by.species.specific.intercept %>%
  mutate(sex = ifelse(grepl(":male$", species_sex), "male", "female"))

# Plot

ggplot(sex.by.species.specific.intercept, aes(x = species_sex, y = `(Intercept)`, color = sex)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Intercepts by Sex within Species",
       y = "Deviation from Overall Intercept") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("male" = "blue", "female" = "red"))


##
fixef(glmm.dropTabanid.ZIP)


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

predict_Tsetse <- predict(glmm.dropTabanid.ZIP,
                          newdata = new_data_all,
                          type="conditional", se.fit = TRUE)

new_data_all$fit <- predict_Tsetse$fit
new_data_all$upper <- predict_Tsetse$fit + 1.96 * predict_Tsetse$se.fit
new_data_all$lower <- predict_Tsetse$fit - 1.96 * predict_Tsetse$se.fit


ggplot(new_data_all, aes(x = TsetsePresencePROB, y = fit, color = sex, linetype = sex)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex), alpha = 0.2, show.legend = FALSE) +
  facet_wrap(~species, scales = "fixed") +  
  labs(title = "Predicted Effect of Tsetse Presence on Stripe Count by Species",
       x = "Tsetse Presence Probability",
       y = "Predicted Stripe Count") +
  theme_bw()


# Get predictions for the zero-inflation model
predict_Tsetse_zero <- predict(glmm.dropTabanid.ZIP,
                              newdata = new_data_all,
                              type="zprob", se.fit = TRUE)

new_data_all$fit.zero <- predict_Tsetse_zero$fit
new_data_all$upper.zero <- predict_Tsetse_zero$fit + 1.96 * predict_Tsetse_zero$se.fit
new_data_all$lower.zero <- predict_Tsetse_zero$fit - 1.96 * predict_Tsetse_zero$se.fit

# Plot
ggplot(new_data_all, aes(x = TsetsePresencePROB, y = fit.zero, color = sex, linetype = sex)) +
  geom_line() +
  facet_wrap(~species, scales = "fixed") +
  labs(x = "Tsetse Presence Probability", 
       y = "Probability of Having Zero Stripes",
       title = "Effect of Tsetse Flies on Zero Stripes (Zero-Inflation Model)") +
  theme_bw()


model_params <- model_parameters(glmm.dropTabanid.ZIP, ci = 0.95)
print(model_params)
