
# Load the packages
library(ape)
library(geiger)
library(caper)
library(tidyverse)
library(phytools)
library(MCMCglmm)

## Read in phylogeny

tree <- read.tree("Phylogenies/Rakotoarivelo/Rakotoarivelo_tree.tre")

plot(tree) # Check that the tree has loaded correctly

# Check whether the tree is binary
is.binary(tree)

# Check whether the tree is rooted 

is.rooted(tree)

# Check whether the tree is ultrametric

is.ultrametric(tree) #True

# Change names to match species name in dataset

tree$tip.label <- gsub("Tragelaphus_imberbis", "imberbis", tree$tip.label)
tree$tip.label <- gsub("Tragelaphus_angasii", "angasii", tree$tip.label)
tree$tip.label <- gsub("Tragelaphus_strepsiceros", "strepsiceros", tree$tip.label)
tree$tip.label <- gsub("Tragelaphus_scriptus", "scriptus", tree$tip.label)
tree$tip.label <- gsub("Tragelaphus_sylvaticus", "sylvaticus", tree$tip.label)
tree$tip.label <- gsub("Tragelaphus_eurycerus", "eurycerus", tree$tip.label)
tree$tip.label <- gsub("Tragelaphus_oryx", "oryx", tree$tip.label)
tree$tip.label <- gsub("Tragelaphus_derbianus", "derbianus", tree$tip.label)
tree$tip.label <- gsub("Tragelaphus_buxtoni", "buxtoni", tree$tip.label)
tree$tip.label <- gsub("Tragelaphus_spekii", "spekii", tree$tip.label)

# Read in stripe data

data<- read.csv("Data_Backup/Working Copy/Processed DataCombined Dataset_specimen_info_2025_01_24_withPredictorVariables.csv")

data <- data %>%
  mutate(
    TabanidActivity = c(scale (TabanidActivity, center = TRUE))
  )




check <- name.check(phy = final.tree, data = data, 
                    data.names = data$tiplabel) # Some species in our data frame don't match our tree, we rename them below.

data <- data %>% 
  mutate(species = case_when(
    species %in% c("sylvaticus","sylvaticus/scriptus") ~ "scriptus"
    ,TRUE ~ species
  )
  )

data <- data %>% 
  mutate(species = case_when(
    species %in% c("spekei") ~ "spekii"
    ,TRUE ~ species
  )
  )

names(data)[names(data) == "species"] <- "tiplabel" 

data <- data %>% drop_na(TsetsePresencePROB) %>% 
  drop_na(tiplabel) %>% 
  drop_na(sex) %>%
  drop_na(TabanidActivity)

str(final.tree)

# Plot raw data, evidence of phylogenetic effect

# Create logEye containing just log eye size length values
n_distinct_stripes <- pull(data, n_distinct_stripes)

# Give log Eye names = species names at the tips of the phylogeny
names(n_distinct_stripes) <- data$tiplabel
head(n_distinct_stripes)

lambda_n_distinct_stripes <- phylosig(final.tree, n_distinct_stripes, method = "lambda", test = TRUE)

ggplot(data, aes(x = TsetsePresencePROB, 
                   y =  n_distinct_stripes, 
                   colour = tiplabel)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE)
  theme_bw()


inv.phylo <- inverseA(final.tree, nodes = "TIPS", scale = TRUE)$Ainv

# Set up priors for MCMCglmm
# Inverse Wishart with V = 1 and nu = 0.02
# i.e. fairly uninformative priors
prior <- list(G = list(G1 = list(V = 1, nu = 0.02)),
              R = list(V = 1, nu = 0.02))
#Number of Iterations
nitt <- 200000
# Set burnin
burnin <- 1000
# Thinning interval
thin <- 500

hist(data$TsetsePresencePROB)

model_mcmcglmm <- MCMCglmm(n_distinct_stripes ~ TsetsePresencePROB, 
                           data = data,
                           family= "zipoisson",
                           random = ~ tiplabel,
                           ginverse = list(tiplabel = inv.phylo), 
                           prior = prior,
                           nitt = nitt, thin = thin, burnin = burnin,
                           verbose = TRUE)

?MCMCglmm

# Plot model diagnostics for MCMCglmm
# For fixed effects
  

effectiveSize(model_mcmcglmm$Sol[, 1:model_mcmcglmm$Fixed$nfl, 
                                 drop = FALSE])[[1]]

# Look for autocorrelation
autocorr(model_mcmcglmm$VCV)


summary(model_mcmcglmm)$solutions

# Look at G-structure and R-structure posterior modes
posterior.mode(model_mcmcglmm$VCV)
# tiplabel     units 
# 51.498573    6.995506  

lambda <- model_mcmcglmm$VCV[,'tiplabel']/
  (model_mcmcglmm$VCV[,'tiplabel'] + model_mcmcglmm$VCV[,'units'])

# Posterior mean of lambda
mean(lambda)

ggplot(as.data.frame(lambda), aes(x = lambda))+
  geom_density() +
  geom_vline(xintercept = posterior.mode(lambda), colour = "red") +
  geom_vline(xintercept = HPDinterval(lambda)[[1]], 
             linetype = "dotted", colour = "red") +
  geom_vline(xintercept = HPDinterval(lambda)[[2]], 
             linetype = "dotted", colour = "red") +
  theme_bw()


# Set up new priors for MCMCglmm
# Inverse Wishart with V = 1 and nu = 1
prior2 <- list(G = list(G1 = list(V = 1, nu = 1)),
               R = list(V = 1, nu = 1))

# Run the model
model_mcmcglmm2 <-  MCMCglmm(n_distinct_stripes ~ TsetsePresencePROB + TabanidActivity + as.factor(sex), 
                             data = data,
                             family = "gaussian",
                             random = ~ tiplabel,
                             ginverse = list(tiplabel = inv.phylo), 
                             prior = prior2,
                             nitt = nitt, thin = thin, burnin = burnin,
                             verbose = TRUE)

summary(model_mcmcglmm2)$solutions # No massive difference when the priors are changed
posterior.mode(model_mcmcglmm2$VCV)


lambda2 <- model_mcmcglmm2$VCV[,'tiplabel']/
  (model_mcmcglmm2$VCV[,'tiplabel'] + model_mcmcglmm2$VCV[,'units'])


ggplot(as.data.frame(lambda2), aes(x = lambda2))+
  geom_density() +
  geom_vline(xintercept = posterior.mode(lambda2), colour = "red") +
  geom_vline(xintercept = HPDinterval(lambda2)[[1]], 
             linetype = "dotted", colour = "red") +
  geom_vline(xintercept = HPDinterval(lambda)[[2]], 
             linetype = "dotted", colour = "red") +
  theme_bw()


#### We're gonna pick a different tree and see how much it afects the results


tree.10 <- tree[[10]] ## Read in Tree

# Check whether the tree is binary
is.binary(tree.10)

# Check whether the tree is rooted 

is.rooted(tree.10)

# Check whether the tree is ultrametric

is.ultrametric(tree.10) #False

final.tree.2<-force.ultrametric(tree.10, method=c("extend"))


final.tree.2$tip.label <- gsub("Tragelaphus_imberbis", "imberbis", final.tree.2$tip.label)
final.tree.2$tip.label <- gsub("Tragelaphus_angasii", "angasii", final.tree.2$tip.label)
final.tree.2$tip.label <- gsub("Tragelaphus_strepsiceros", "strepsiceros", final.tree.2$tip.label)
final.tree.2$tip.label <- gsub("Tragelaphus_scriptus", "scriptus", final.tree.2$tip.label)
final.tree.2$tip.label <- gsub("Tragelaphus_eurycerus", "eurycerus", final.tree.2$tip.label)
final.tree.2$tip.label <- gsub("Tragelaphus_oryx", "oryx", final.tree.2$tip.label)
final.tree.2$tip.label <- gsub("Tragelaphus_derbianus", "derbianus", final.tree.2$tip.label)
final.tree.2$tip.label <- gsub("Tragelaphus_buxtoni", "buxtoni", final.tree.2$tip.label)
final.tree.2$tip.label <- gsub("Tragelaphus_spekii", "spekii", final.tree.2$tip.label)


inv.phylo <- inverseA(final.tree.2, nodes = "TIPS", scale = TRUE)$Ainv

# Set up priors for MCMCglmm
# Inverse Wishart with V = 1 and nu = 0.02
# i.e. fairly uninformative priors
prior <- list(G = list(G1 = list(V = 1, nu = 0.02)),
              R = list(V = 1, nu = 0.02))
#Number of Iterations
nitt <- 1000000
# Set burnin
burnin <- 1000
# Thinning interval
thin <- 500


model_mcmcglmm.3 <- MCMCglmm(n_distinct_stripes ~ TsetsePresencePROB + TabanidActivity + as.factor(sex), 
                           data = data, 
                           family = "gaussian",
                           random = ~ tiplabel,
                           ginverse = list(tiplabel = inv.phylo), 
                           prior = prior,
                           nitt = nitt, thin = thin, burnin = burnin,
                           verbose = TRUE)

summary(model_mcmcglmm.3)$solutions 


