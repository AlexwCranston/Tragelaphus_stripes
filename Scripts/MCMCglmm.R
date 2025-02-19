
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

data<- read.csv("Data_Backup/Working Copy/Processed Data/Combined Dataset_2025_01_31.csv")

data$species <- as.factor(data$species) 
data$species <- fct_recode(data$species, "scriptus" = "sylvaticus/scriptus", "scriptus" = "sylvaticus") # Make all bushbuck same species

data <- data %>% mutate(revised.sex = ifelse(is.na(sex), "Undetermined", sex)) # Define NAs in sex as "undetermined"

names(data)[names(data) == "species"] <- "tiplabel" # Rename species to tiplabel to match phylogeny

data <- data %>% drop_na(TsetsePresencePROB) %>% 
  drop_na(tiplabel) %>% 
  drop_na(TabanidActivity) #Drop NAs for fixed effects

# Calc inv.phylo 
  
inv.phylo <- inverseA(tree, nodes = "TIPS", scale = TRUE)$Ainv

# Set up priors for MCMCglmm
# Inverse Wishart with V = 1 and nu = 0.02
# i.e. fairly uninformative priors
prior <- list(G = list(G1 = list(V = diag(3), nu = 0.02),
                       G2 = list(V= diag(1), nu = 0.02),
                       G3 = list(V= diag(1), nu = 0.02)))
#Number of Iterations
nitt <- 150000
# Set burnin
burnin <- 1000
# Thinning interval
thin <- 500

hist(data$n_distinct_stripes)

model_mcmcglmm <- MCMCglmm(n_distinct_stripes ~ TsetsePresencePROB, 
                           data = data,
                           family= "poisson",
                           random = ~ us(revised.sex):tiplabel + us(TsetsePresencePROB):tiplabel + tiplabel,
                           ginverse = list(tiplabel = inv.phylo), 
                           prior = prior,
                           nitt = nitt, thin = thin, burnin = burnin,
                           verbose = TRUE)

# Check ESS


effectiveSize(model_mcmcglmm$Sol[, 1:model_mcmcglmm$Fixed$nfl, 
                                 drop = FALSE])[[1]]

# Look for autocorrelation
autocorr(model_mcmcglmm$VCV)

# Overall model summary

summary(model_mcmcglmm)

# Look at fixed effects
summary(model_mcmcglmm)$solutions

# Look at species level random effect

summary(model_mcmcglmm)$Gcovariances

summary(model_mcmcglmm$VCV)



