library(igraph)
library(foreach)
library(sampling)
library(tidyverse)
source("R/snowball.R")

#### Model graph
N <- 1000
nSimus_sample <- 500
n <- 50

# set.seed(1005192119)

# g <- watts.strogatz.game(1, N, N*0.2, 0.4)
g <- barabasi.game(N, directed = F)
# g <- make_ring(N, circular = T)
# g <- graph_from_adjacency_matrix(matrix(1,nrow=N,ncol=N))
# g <- forest.fire.game(N, 0.35, directed=F)
# g %>% rewire(each_edge(p = .2, loops = FALSE))

# name <- "BA"
# parameter <- 0

# plot(g)

#### Simulations
# param_vec <- seq(0.05, 1, by=0.05)
# param_vec <- seq(1, 10, by=1)
param_vec <- seq(0.01, 0.01, by=0.01)
ambs <- 1
# fwprob <- 0.4

method_first <- "poisson.pps"
X <- betweenness(g)

estimators_stats <- foreach(param=param_vec, .combine=rbind) %do% {
  print(param)
  g <- forest.fire.game(nodes = N, fw.prob = param, 
                        ambs = ambs, directed=F)
  name <- "forest_fire_X_deg"
  parameter <- param
  graph_estimators(g, n, nSimus_sample, name, parameter,
         c("degree", "betweenness", "clustering"),
         list(degree(g), betweenness(g), transitivity(g, "local")),
         method_first = method_first, X = X)
  
  
}

saveRDS(estimators_stats, file="ff_fwprob_100218_X_btwn.rds")