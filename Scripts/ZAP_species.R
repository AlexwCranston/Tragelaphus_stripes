library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)
library(glmmTMB)
library(ggeffects)
library(parameters)


# Read in stripe data

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


summary(glmm.fixed.ZAP)


# Plot random intercepts with forest plot, first for the conditional part of the model ####

random.effects.species <- as.data.frame(glmmTMB::ranef(glmm.fixed.ZAP))
random.effects.species <- random.effects.species %>%  mutate(condval = ifelse(component == "zi", -condval, condval)) # Take inverse of binomial intercepts so it shows chance of having stripes, not zero stripes
random.effects.species <- random.effects.species %>%
  mutate(
    lower = condval - 1.96 * condsd,
    upper = condval + 1.96 * condsd
  )

species.cond.intercept <- random.effects.species %>% filter (grpvar == "species" & component == "cond")
species.cond.intercept<- species.cond.intercept %>% mutate(grp = fct_relevel(grp, 
                           "angasii","buxtoni","derbianus", 
                           "eurycerus", "imberbis","oryx",
                           "scriptus","spekii","strepsiceros")) # Relevel these so they plot in alphabetical order

sex.by.species.cond.intercept <- random.effects.species %>% filter (grpvar == "species:sex" & component == "cond")
sex.by.species.cond.intercept<- sex.by.species.cond.intercept %>% mutate(grp = fct_relevel(grp, 
                                                                             "angasii:female","angasii:male",
                                                                             "buxtoni:female","buxtoni:male",
                                                                             "derbianus:female", "derbianus:male",
                                                                             "eurycerus:female", "eurycerus:male",
                                                                             "imberbis:female", "imberbis:male",
                                                                             "oryx:female","oryx:male",
                                                                             "scriptus:female", "scriptus:male",
                                                                             "spekii:female", "spekii:male",
                                                                             "strepsiceros:female", "strepsiceros:male")) # Relevel these so they plot in alphabetical order

sex.by.species.cond.intercept$species <- c("angasii","angasii", "buxtoni","buxtoni", "derbianus","derbianus", 
                                          "eurycerus","eurycerus", "imberbis","imberbis", "oryx" ,"oryx",
                                          "scriptus","scriptus",
                                          "spekii","spekii", "strepsiceros","strepsiceros") # Add species label to colour by species


ggplot(species.cond.intercept, aes(x = grp, y = condval, color=grp)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
  labs(title = "Random Intercepts by Species - Count",
       y = "Deviation from Overall Intercept",
       x = "Species",
       color = "Species",
       ) +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092"),
    labels = c("Nyala", "Mountain Nyala", "Giant Eland", "Bongo", "Lesser Kudu", "Common Eland", "Bushbuck", "Sitatunga", "Greater Kudu")) +
  scale_x_discrete(
    labels = c("Nyala", "Mountain Nyala", "Giant Eland", "Bongo", "Lesser Kudu", "Common Eland", "Bushbuck", "Sitatunga", "Greater Kudu")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title=element_text(hjust=0.5))


ggplot(sex.by.species.cond.intercept, aes(x = grp, y = condval, color=species)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
  labs(title = "Random Intercepts by Sex by Species - Count",
       y = "Deviation from Overall Intercept",
       x = "Sex/Species",
       color = "Species") +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092"),
    labels = c("Nyala", "Mountain Nyala", "Giant Eland", "Bongo", "Lesser Kudu", "Common Eland", "Bushbuck", "Sitatunga", "Greater Kudu")) +
  scale_x_discrete(labels = c("Nyala - Female", "Nyala - Male","Mountain Nyala - Female", "Mountain Nyala - Male",
               "Giant Eland - Female", "Giant Eland - Male","Bongo - Female", "Bongo - Male",
               "Lesser Kudu - Female", "Lesser Kudu - Male", "Common Eland - Female", "Common Eland - Male",
               "Bushbuck - Female", "Bushbuck - Male","Sitatunga - Female", "Sitatunga - Male",
               "Greater Kudu - Female", "Greater Kudu - Male")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.4, hjust=1),plot.title=element_text(hjust=0.5))



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

  
cor.test(cor.test.data.species.cond$Tsetse_mean_value, cor.test.data.species.cond$`(Intercept)`, test = "pearson")

### Now the binomial ####

species.binom.intercept <- random.effects.species %>% filter (grpvar == "species" & component == "zi")
species.binom.intercept<- species.binom.intercept %>% mutate(grp = fct_relevel(grp, 
                                                                             "angasii","buxtoni","derbianus", 
                                                                             "eurycerus", "imberbis","oryx",
                                                                             "scriptus","spekii","strepsiceros"))



sex.by.species.binom.intercept <- random.effects.species %>% filter (grpvar == "species:sex" & component == "zi")
sex.by.species.binom.intercept<- sex.by.species.binom.intercept %>% mutate(grp = fct_relevel(grp, 
                                                                                           "angasii:female","angasii:male",
                                                                                           "buxtoni:female","buxtoni:male",
                                                                                           "derbianus:female", "derbianus:male",
                                                                                           "eurycerus:female", "eurycerus:male",
                                                                                           "imberbis:female", "imberbis:male",
                                                                                           "oryx:female","oryx:male",
                                                                                           "scriptus:female", "scriptus:male",
                                                                                           "spekii:female", "spekii:male",
                                                                                           "strepsiceros:female", "strepsiceros:male",))

sex.by.species.binom.intercept$species <- c("angasii","angasii", "buxtoni","buxtoni", "derbianus","derbianus", 
                                           "eurycerus","eurycerus", "imberbis","imberbis", "oryx" ,"oryx",
                                           "scriptus","scriptus",
                                           "spekii","spekii", "strepsiceros","strepsiceros")


ggplot(species.binom.intercept, aes(x = grp, y = condval, color=grp)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
  labs(title = "Random Intercepts by Species - Binomial",
       y = "Deviation from Overall Intercept",
       x = "Species",
       color = "Species",
  ) +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092"),
    labels = c("Nyala", "Mountain Nyala", "Giant Eland", "Bongo", "Lesser Kudu", "Common Eland", "Bushbuck", "Sitatunga", "Greater Kudu")) +
  scale_x_discrete(
    labels = c("Nyala", "Mountain Nyala", "Giant Eland", "Bongo", "Lesser Kudu", "Common Eland", "Bushbuck", "Sitatunga", "Greater Kudu")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title=element_text(hjust=0.5))



ggplot(sex.by.species.binom.intercept, aes(x = grp, y = condval, color=species)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
  labs(title = "Random Intercepts by Sex by Species - Binomial",
       y = "Deviation from Overall Intercept",
       x = "Sex/Species",
       color = "Species") +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus" = "#A6761D",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092"),
    labels = c("Nyala", "Mountain Nyala", "Giant Eland", "Bongo", "Lesser Kudu", "Common Eland", "Bushbuck", "Sitatunga", "Greater Kudu")) +
  scale_x_discrete(labels = c("Nyala - Female", "Nyala - Male","Mountain Nyala - Female", "Mountain Nyala - Male",
                              "Giant Eland - Female", "Giant Eland - Male","Bongo - Female", "Bongo - Male",
                              "Lesser Kudu - Female", "Lesser Kudu - Male", "Common Eland - Female", "Common Eland - Male",
                              "Bushbuck - Female", "Bushbuck - Male","Sitatunga - Female", "Sitatunga - Male",
                              "Greater Kudu - Female", "Greater Kudu - Male")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.4, hjust=1),plot.title=element_text(hjust=0.5))





ggplot(cor.test.data.species.binom, aes(x = Tsetse_mean_value, y = `(Intercept)`, color=species)) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus scriptus" = "#A6761D", "scriptus sylvaticus" = "#666666",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw()


cor.test(cor.test.data.species.binom$Tsetse_mean_value, cor.test.data.species.binom$`(Intercept)`, test = "pearson")



# Plot estimated effect sizes with forest  plot for fixed effects of model ####

fixed.effects <- fixef(glmm.fixed.ZAP)
fixed.effects <-as.data.frame(unlist(fixed.effects))


# Compute Profile Likelihood Confidence Intervals
ci_profile <- as.data.frame(confint(glmm.fixed.ZAP, method = "profile", parm = "beta_"))

row.names(fixed.effects) <- row.names(ci_profile)
fixed.effects <- merge(fixed.effects, ci_profile, by = "row.names", all.x = TRUE)
fixed.effects<-fixed.effects %>% 
  rename(
    estimate = `unlist(fixed.effects)`,
    parameter = Row.names,
    conf.low = `2.5 %`,
    conf.high = `97.5 %`
  )


fixed.effects <- fixed.effects %>% filter(parameter %in% c("PNV", "TsetsePresencePROB", "TabanidActivity","zi~PNV","zi~TsetsePresencePROB", "zi~TabanidActivity"))

fixed.effects$section <- c("Count","Count","Count","Binomial","Binomial","Binomial")
fixed.effects$parameter <- as.factor(fixed.effects$parameter)


fixed.effects <- fixed.effects %>% mutate(estimate = ifelse(section == "Binomial", -estimate, estimate),
                                          conf.low = ifelse(section == "Binomial", -conf.low, conf.low),
                                          conf.high = ifelse(section == "Binomial", -conf.high, conf.high))



ggplot(fixed.effects, aes(x = estimate, y = parameter, color = section)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(name = "Component",
                     values = c(
      "Count" = "#1B9E77", "Binomial" = "#D95F02")) +
  scale_y_discrete(labels = c(
    "PNV" = "Predicted Natural Vegetation (fAPAR)",
    "TsetsePresencePROB" = "Probability of Tsetse Fly Presence",
    "zi~PNV" = "Predicted Natural Vegetation (fAPAR)",
    "zi~TsetsePresencePROB" = "Probability of Tsetse Fly Presence",
    "TabanidActivity" = "Months of Optimal Tabanid Conditions",
    "zi~TabanidActivity" = "Months of Optimal Tabanid Conditions")) +
  labs(x = "Effect Size", y = "Predictor") +
  theme_bw()


# Get predictions for the count model ####

species_list <- unique(data$species)
sex_list <- unique(data$sex)

new_data_all <- expand.grid(
  TsetsePresencePROB = seq(min(data$TsetsePresencePROB, na.rm = TRUE), 
                           max(data$TsetsePresencePROB, na.rm = TRUE), 
                           length.out = 100),
  PNV = 0,
  TabanidActivity = mean(data$TabanidActivity), 
  species = species_list, 
  sex = sex_list)

predict_Tsetse <- predict(glmm.fixed.ZAP,
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
       x = "Probability of Tsetse Presence",
       y = "Predicted Relative Stripe Count") +
  theme_bw()


# Get predictions for the zero-inflation model
predict_Tsetse_zero <- predict(glmm.fixed.ZAP,
                               newdata = new_data_all,
                               type="zprob", se.fit = TRUE)

new_data_all$fit.zero <- 1-predict_Tsetse_zero$fit
new_data_all$upper.zero <- 1-(predict_Tsetse_zero$fit + 1.96 * predict_Tsetse_zero$se.fit)
new_data_all$lower.zero <- 1-(predict_Tsetse_zero$fit - 1.96 * predict_Tsetse_zero$se.fit)

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
  labs(x = "Probability of Tsetse Presence", 
       y = "Probability of Having Stripes",
       title = "Effect of Tsetse Flies on Presence of Stripes (Hurdle Model)") +
  theme_bw()




