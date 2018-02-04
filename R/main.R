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
# g <- barabasi.game(N, directed = F)
# g <- make_ring(N, circular = T)
# g <- graph_from_adjacency_matrix(matrix(1,nrow=N,ncol=N))
# g <- forest.fire.game(N, 0.35, directed=F)
# g %>% rewire(each_edge(p = .2, loops = FALSE))

# plot(g)

#### Simulations
# g <- forest.fire.game(N, 0.35, directed=F
estimators_stats <- graph_estimators(g, n, nSimus_sample, 
                                      c("degree", "betweenness", "clustering"),
                                      list(degree(g), betweenness(g), transitivity(g, "local")) )

## TODO Graphs Deff <-> model parameter for snowball, for various models
