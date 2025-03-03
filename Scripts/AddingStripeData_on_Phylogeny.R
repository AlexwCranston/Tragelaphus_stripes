library(ape) # for reading and manipulating trees
library(tidyverse) # for data manipulation
library(geiger) # for checking names in phylogeny and data match
library(ggtree) # for plotting trees
library(ggstance) # for geombarh function to make bars
library(ggnewscale) # for different scales in ggplot

# Helper functions for plotting
remove_y <- 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


antelopetree <- read.nexus("Phylogenies/Mammal_SuperTree/antelopetree.nex")
# look at it

str(antelopetree)


stripes <- read_csv("Data_backup/Working Copy/Trait Data for Plots/antelope-data.csv")

# Look at it
stripes

# Note that stripes here = vertical stripes. I have not counted species with single horizontal stripes along their midlines for example.
# Habitat is primary habitat type. Closed = forest or dense scrub. Open = open grasslands, waterways, mountainsides, rocky areas.
# Only living, African antelope species are included.

# Reorder to match tree
stripes <- stripes[match(antelopetree$tip.label, stripes$tips),]

# Create a new dataframe object that
# contains only the variable you want to plot
# with the species/tip names as rownames...
stripe <-
  stripes %>%
  select(stripes) %>%
  as.data.frame()
# Add rownames as species names  
rownames(stripe) <- stripes$Binomial

# Make the tree base
antelopetree$tip.label <- gsub("_", " ", antelopetree$tip.label)

base <- 
  ggtree(antelopetree, layout = "circular") +
  geom_tiplab(geom = "text", offset = 8, size = 3.2, fontface = "italic") +
  xlim(0,45)

# Plot stripes as facet
p1 <- gheatmap(base, stripe, offset = 0.5, width = 0.1,
               colnames = FALSE, color = "black", legend_title = "stripes")+
  # add fill colours using standard scale fill manual
  scale_fill_manual(values = c("white", "red")) +
  theme(legend.title = element_blank())

