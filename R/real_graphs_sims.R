library(igraph)
library(igraphdata)
library(foreach)
library(sampling)
library(tidyverse)
source("R/snowball.R")
source("R/metrics.R")

#### Model graph
N <- 1000
nSimus_sample <- 500
n <- 50

set.seed(1005192119)

data(yeast)
g <- yeast

list_X <- list(closeness(g), betweenness(g), degree(g), page_rank(g)$vector, 
               eigen_centrality(g)$vector, transitivity(g, "local"), max_path_length(g))
names(list_X) <- c("closeness", "betweenness", "degree", "page_rank", 
                   "eigen", "clustering", "max_path_length")

graph_stat <- list_X
name_stat <- names(list_X)
parameter <- "yeast"
method_first <- "poisson.pps"

yeast_estimators <- foreach(k=1:(length(list_X)), .combine=rbind) %do% {
    currentX <- list_X[[k]]
    current_name <- names(list_X)[k]
    print(paste("----", current_name))
    
    graph_estimators(g, n, nSimus_sample, name, parameter,
                     name_stat,
                     graph_stat,
                     method_first = method_first, X = X)
    
}

saveRDS(yeast_estimators, file="yeast_21022018_pps.rds")

  