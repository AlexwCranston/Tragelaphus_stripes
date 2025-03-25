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


# Split by source

data.iNat <- data %>% filter(source == "iNat")
data.collections <- data %>% filter(source == "collection")

# Mean error extent 

mean(data.iNat$extent_km) # 18.48509
median(data.iNat$extent_km) # 1.205
hist(data$extent_km)


mean(data.collections$extent_km) # 31.1019
median(data.collections$extent_km) # 12.805
hist(data.collections$extent_km)




# Run the model with random effects only, i.e. only species and sex (nested within species) ####


glmm.iNat.ZAP <- glmmTMB(relative_n_stripes ~ TsetsePresencePROB + 
                                  PNV + TabanidActivity +
                                  (1 | species) + (1 | species:sex),
                                ziformula = ~ TsetsePresencePROB + 
                                  PNV  + TabanidActivity + (1 | species) + (1|species:sex),  # Model excess zeros
                                family = truncated_poisson,
                                data = data.iNat)


glmm.museum.ZAP <- glmmTMB(relative_n_stripes ~ TsetsePresencePROB + 
                                  PNV + TabanidActivity + 
                                  (1 | species) + (1 | species:sex),
                                ziformula = ~ TsetsePresencePROB + 
                                  PNV  + TabanidActivity + (1 | species) + (1|species:sex),  # Model excess zeros
                                family = truncated_poisson,
                                data = data.collections)

summary(glmm.iNat.ZAP)
summary(glmm.museum.ZAP)



fixed.effects.iNat <- fixef(glmm.iNat.ZAP)
fixed.effects.iNat <-as.data.frame(unlist(fixed.effects.iNat))


fixed.effects.museum <- fixef(glmm.museum.ZAP)
fixed.effects.museum <-as.data.frame(unlist(fixed.effects.museum))

# Compute Profile Likelihood Confidence Intervals
ci_profile.iNat <- as.data.frame(confint(glmm.iNat.ZAP, method = "profile", parm = "beta_"))

row.names(fixed.effects.iNat) <- row.names(ci_profile.iNat)
fixed.effects.iNat <- merge(fixed.effects.iNat, ci_profile.iNat, by = "row.names", all.x = TRUE)
fixed.effects.iNat<-fixed.effects.iNat %>% 
  rename(
    estimate.iNat = `unlist(fixed.effects.iNat)`,
    parameter = Row.names,
    conf.low.iNat = `2.5 %`,
    conf.high.iNat = `97.5 %`
  )

# Compute Profile Likelihood Confidence Intervals
ci_profile.museum <- as.data.frame(confint(glmm.museum.ZAP, method = "profile", parm = "beta_"))

row.names(fixed.effects.museum) <- row.names(ci_profile.museum)
fixed.effects.museum <- merge(fixed.effects.museum, ci_profile.museum, by = "row.names", all.x = TRUE)
fixed.effects.museum<-fixed.effects.museum %>% 
  rename(
    estimate.museum = `unlist(fixed.effects.museum)`,
    parameter = Row.names,
    conf.low.museum = `2.5 %`,
    conf.high.museum = `97.5 %`
  )

fixed.effects <- merge(fixed.effects.museum, fixed.effects.iNat, by="parameter")

fixed.effects <- fixed.effects %>%
  filter(parameter %in% c("PNV", "TsetsePresencePROB", "TabanidActivity","zi~PNV","zi~TsetsePresencePROB", "zi~TabanidActivity"))
fixed.effects$section <- c("Count","Count", "Count", "Binomial","Binomial", "Binomial")
fixed.effects$parameter <- as.factor(fixed.effects$parameter)

fixed.effects <- fixed.effects %>% mutate(estimate.museum = ifelse(section == "Binomial", -estimate.museum, estimate.museum),
                                          estimate.iNat = ifelse(section == "Binomial", -estimate.iNat, estimate.iNat),
                                          conf.low.iNat = ifelse(section == "Binomial", -conf.low.iNat, conf.low.iNat),
                                          conf.low.museum = ifelse(section == "Binomial", -conf.low.museum, conf.low.museum),
                                          conf.high.iNat = ifelse(section == "Binomial", -conf.high.iNat, conf.high.iNat),
                                          conf.high.museum = ifelse(section == "Binomial", -conf.high.museum, conf.high.museum))

write.csv(fixed.effects, file ="Data_backup/Working Copy/Museum_vs_iNat_SummaryComparison.csv")

