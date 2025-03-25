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
data <- data %>% filter(species == "scriptus")

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


glmm.bushbuck.ZAP <- glmmTMB(relative_n_stripes ~ TsetsePresencePROB + 
                               PNV + TabanidActivity + 
                               (1 | sex),
                             ziformula = ~ TsetsePresencePROB + 
                               PNV  + TabanidActivity + (1|sex),  # Model excess zeros
                             family = truncated_nbinom2,
                             data = data)


performance::check_overdispersion(glmm.bushbuck.ZAP) # No overdispersion detected


summary(glmm.bushbuck.ZAP)


# Create forest plot for fixed effects of model

fixed.effects <- fixef(glmm.bushbuck.ZAP)
fixed.effects <-as.data.frame(unlist(fixed.effects))
fixed.effects <- fixed.effects %>% filter(row.names(fixed.effects) != "disp.(Intercept)")

# Compute Profile Likelihood Confidence Intervals
ci_profile <- as.data.frame(confint(glmm.bushbuck.ZAP, method = "profile", parm = "beta_"))

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


write.csv(fixed.effects, file ="Data_backup/Working Copy/BushbuckONLY_Model_FixedEffects.csv")


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

