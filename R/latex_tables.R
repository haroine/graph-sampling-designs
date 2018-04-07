library(tidyverse)
library(igraph)
library(igraphdata)
library(foreach)

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


#### Comptes
tolatex$categ <- cut(tolatex$deff, 
                     breaks=c(-Inf, 0.9999999, 2, Inf), 
                     labels=c("efficient","medium","inefficient"))

counts_efficiency <- tolatex %>%
    group_by(var, categ) %>%
    summarize(count=n())
  
counts_efficiency <- na.omit(counts_efficiency)

counts_efficiency <- counts_efficiency %>%
  ungroup %>% 
  group_by(var) %>%  mutate(proportion = count / sum(count)) 


