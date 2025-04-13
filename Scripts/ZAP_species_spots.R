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
## Spots model

data <- data  %>% drop_na(number_spots) # Drop NAs
data$relative_n_spots <- data$number_spots/data$skin_length_m # Add column for number of spots relative to bodysize
data$relative_n_spots <- round(data$relative_n_spots, 0) # Round so these are whole integers


glmm.spots.ZAP <- glmmTMB(relative_n_spots ~ TsetsePresencePROB + 
                            PNV + TabanidActivity  + (1 | species) +               # Random intercept for species
                            (1 | species:sex),
                          ziformula = ~ TsetsePresencePROB + 
                            PNV  + TabanidActivity  + (1 | species) +               # Random intercept for species
                            (1 | species:sex),  # Model excess zeros
                          family = truncated_poisson,
                          data = data)

performance::check_overdispersion(glmm.spots.ZAP) # No overdispersion detected


summary(glmm.spots.ZAP)


# Create forest plot for fixed effects of model

fixed.effects <- fixef(glmm.spots.ZAP)
fixed.effects <-as.data.frame(unlist(fixed.effects))
fixed.effects <- fixed.effects %>% filter(row.names(fixed.effects) != "disp.(Intercept)")

# Compute Profile Likelihood Confidence Intervals
ci_profile <- as.data.frame(confint(glmm.spots.ZAP, method = "profile", parm = "beta_"))

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



# Plot random intercepts with forest plot, first for the conditional part of the model ####

random.effects.species <- as.data.frame(glmmTMB::ranef(glmm.spots.ZAP))
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

