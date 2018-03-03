library(tidyverse)
library(igraph)
library(igraphdata)

tolatex <- readRDS("data/tidy_deffs_networks.rds")

#### Descriptive stats

data("yeast")
data("USairports")
data("immuno")
list_real_graphs <- list(yeast, USairports, immuno)
names(list_real_graphs) <- c("yeast", "USairports", "immuno")


describe_real_datasets <- foreach(k=1:length(list_real_graphs), .combine = rbind) %do% {
  
  g <- list_real_graphs[[k]]
  c( names(list_real_graphs)[k], vcount(g),
     ecount(g), mean(degree(g)), mean(transitivity(g, "local"), na.rm=T),
     transitivity(g, "global"), count_components(g), fit_power_law(degree(g))$alpha)
}

## TODO xtable
# print(xtable(deff_tidy),  include.rownames=F)
