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


# Scale and centre (where required fixed effects)

data$TsetsePresencePROB <- scale(data$TsetsePresencePROB, center = FALSE)
data$TabanidActivity <- scale(data$TabanidActivity, center = FALSE)
data$PNV <- scale(data$PNV, center = TRUE)




# Subspecies Model ####

glmm.fixed.ZAP_subspecies <- glmmTMB(relative_n_stripes ~ TsetsePresencePROB + 
                                       PNV + TabanidActivity + 
                                       (1 | revised_subspecies) + (1 | revised_subspecies:sex),
                                     ziformula = ~ TsetsePresencePROB + 
                                       PNV  + TabanidActivity + (1 | revised_subspecies) + (1|revised_subspecies:sex),  # Model excess zeros
                                     family = truncated_poisson,
                                     data = data)

summary(glmm.fixed.ZAP_subspecies)
# Plot random intercepts for subspecies effect
# First for conditional part of the model ####
random.effects.subspecies <- as.data.frame(glmmTMB::ranef(glmm.fixed.ZAP_subspecies))
random.effects.subspecies <- random.effects.subspecies %>%  mutate(condval = ifelse(component == "zi", -condval, condval)) # Take inverse of binomial intercepts so it shows chance of having stripes, not zero stripes

subspecies.cond.intercept <- random.effects.subspecies %>% filter (grpvar == "revised_subspecies" & component == "cond")

subspecies.cond.intercept$grp <- factor(
  subspecies.cond.intercept$grp, 
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
    grp %in% c("angasii") ~ "angasii",
    grp %in% c("buxtoni") ~ "buxtoni",
    grp %in% c("derbianus", "gigas") ~ "derbianus",
    grp %in% c("eurycerus", "isaaci") ~ "eurycerus",
    grp %in% c("australis", "imberbis") ~ "imberbis",
    grp %in% c("livingstonii", "oryx", "pattersonianus") ~ "oryx",
    grp %in% c("bor", "decula", "phaleratus", "scriptus") ~ "scriptus scriptus",
    grp %in% c("dama", "delamerei", "fasciatus", "meneliki", "ornatus", "roualeynei","sylvaticus") ~ "scriptus sylvaticus",
    grp %in% c("gratus", "selousi", "spekii") ~ "spekii",
    grp %in% c("chora", "cottoni", "strepsiceros") ~ "strepsiceros",
    TRUE ~ NA_character_  # Assign NA if no match
  ))


ggplot(subspecies.cond.intercept, aes(x = grp, y = condval, color = species)) +
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


subspecies.cond.intercept<-subspecies.cond.intercept %>% 
  rename(
    revised_subspecies = grp,
  )


mean_values_Tsetse_subspecies <- data %>%
  group_by(revised_subspecies) %>%
  summarise(
    Tsetse_mean_value = mean(TsetsePresencePROB))


mean_values_PNV_subspecies <- data %>%
  group_by(revised_subspecies) %>%
  summarise(
    mean_values_PNV = mean(PNV))


mean_values_Tabanid_subspecies <- data %>%
  group_by(revised_subspecies) %>%
  summarise(
    mean_values_Tabanid = mean(TabanidActivity))


mean_values_Tsetse_subspecies <- na.omit(mean_values_Tsetse_subspecies)
mean_values_PNV_subspecies <- na.omit(mean_values_PNV_subspecies)
mean_values_Tabanid_subspecies <- na.omit(mean_values_Tabanid_subspecies)


cor.test.data.subspecies.cond <-  full_join(subspecies.cond.intercept, mean_values_Tsetse_subspecies,
                                            by = "revised_subspecies")

cor.test.data.subspecies.cond <-  full_join(cor.test.data.subspecies.cond, mean_values_PNV_subspecies,
                                            by = "revised_subspecies")

cor.test.data.subspecies.cond <-  full_join(cor.test.data.subspecies.cond, mean_values_Tabanid_subspecies,
                                            by = "revised_subspecies")

ggplot(cor.test.data.subspecies.cond, aes(x = Tsetse_mean_value, y = condval, color=species)) +
  geom_point(size = 3) +
  labs(y = "Intercept",
       x = "Mean Tsetse Probability for Subspecies") +
  geom_smooth(method = "lm",color ="black", se = FALSE) +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus scriptus" = "#A6761D", "scriptus sylvaticus" = "#666666",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw()


subspecies.model.cond.tsetse<-lm(condval~ Tsetse_mean_value, data = cor.test.data.subspecies.cond)
summary(subspecies.model.cond.tsetse)
cor.test(cor.test.data.subspecies.cond$condval, cor.test.data.subspecies.cond$Tsetse_mean_value, data = cor.test.data.subspecies.cond)



subspecies.model.cond.tabanid<-lm(condval~ mean_values_PNV, data = cor.test.data.subspecies.cond)
summary(subspecies.model.cond.tabanid)
cor.test(cor.test.data.subspecies.cond$condval, cor.test.data.subspecies.cond$mean_values_PNV, data = cor.test.data.subspecies.cond)


subspecies.model.cond.pnv<-lm(condval~ mean_values_Tabanid, data = cor.test.data.subspecies.cond)
summary(subspecies.model.cond.pnv)
cor.test(cor.test.data.subspecies.cond$condval, cor.test.data.subspecies.cond$mean_values_Tabanid, data = cor.test.data.subspecies.cond)


# Now for the binomial part of the model ####

subspecies.binom.intercept <- random.effects.subspecies %>% filter (grpvar == "revised_subspecies" & component == "zi")


subspecies.binom.intercept$grp <- factor(
  subspecies.binom.intercept$grp, 
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
    grp %in% c("angasii") ~ "angasii",
    grp %in% c("buxtoni") ~ "buxtoni",
    grp %in% c("derbianus", "gigas") ~ "derbianus",
    grp %in% c("eurycerus", "isaaci") ~ "eurycerus",
    grp %in% c("australis", "imberbis") ~ "imberbis",
    grp %in% c("livingstonii", "oryx", "pattersonianus") ~ "oryx",
    grp %in% c("bor", "decula", "phaleratus", "scriptus") ~ "scriptus scriptus",
    grp %in% c("dama", "delamerei", "fasciatus", "meneliki", "ornatus", "roualeynei","sylvaticus") ~ "scriptus sylvaticus",
    grp %in% c("gratus", "selousi", "spekii") ~ "spekii",
    grp %in% c("chora", "cottoni", "strepsiceros") ~ "strepsiceros",
    TRUE ~ NA_character_  # Assign NA if no match
  ))


ggplot(subspecies.binom.intercept, aes(x = grp, y = condval, color = species)) +
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


subspecies.binom.intercept<-subspecies.binom.intercept %>% 
  rename(
    revised_subspecies = grp,
  )

cor.test.data.subspecies.binom<-  full_join(subspecies.binom.intercept, mean_values_Tsetse_subspecies,
                                            by = "revised_subspecies")

cor.test.data.subspecies.binom <- full_join(cor.test.data.subspecies.binom, mean_values_PNV_subspecies,
                                            by = "revised_subspecies")

cor.test.data.subspecies.binom <- full_join(cor.test.data.subspecies.binom, mean_values_Tabanid_subspecies,
                                            by = "revised_subspecies")


ggplot(cor.test.data.subspecies.binom, aes(x = Tsetse_mean_value, y = condval, color=species)) +
  geom_point(size = 3) +
  labs(y = "Intercept",
       x = "Mean Tsetse Probability for Subspecies") +
  scale_color_manual(values = c(
    "angasii" = "#1B9E77", "buxtoni" = "#D95F02", "derbianus" = "#7570B3", 
    "eurycerus" = "#E7298A", "imberbis" = "#66A61E", "oryx" = "#E6AB02",
    "scriptus scriptus" = "#A6761D", "scriptus sylvaticus" = "#666666",
    "spekii" = "#FF6DB6", "strepsiceros" = "#490092")) +
  theme_bw()


subspecies.model.binom.tsetse<-lm(condval~ Tsetse_mean_value, data = cor.test.data.subspecies.binom)
summary(subspecies.model.binom.tsetse)
cor.test(cor.test.data.subspecies.binom$condval, cor.test.data.subspecies.binom$Tsetse_mean_value, data = cor.test.data.subspecies.binom)


subspecies.model.binom.tabanid<-lm(condval~ mean_values_PNV, data = cor.test.data.subspecies.binom)
summary(subspecies.model.binom.tabanid)
cor.test(cor.test.data.subspecies.binom$condval, cor.test.data.subspecies.binom$mean_values_PNV, data = cor.test.data.subspecies.binom)


subspecies.model.binom.pnv<-lm(condval~ mean_values_Tabanid, data = cor.test.data.subspecies.binom)
summary(subspecies.model.binom.pnv)
cor.test(cor.test.data.subspecies.binom$condval, cor.test.data.subspecies.binom$mean_values_Tabanid, data = cor.test.data.subspecies.binom)


