library(igraph)
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

# g <- watts.strogatz.game(1, N, N*0.2, 0.4)
# g <- barabasi.game(N, directed = F)
# g <- make_ring(N, circular = T)
# g <- graph_from_adjacency_matrix(matrix(1,nrow=N,ncol=N))
g <- forest.fire.game(N, 0.3, directed=F)
# g %>% rewire(each_edge(p = .2, loops = FALSE))

# name <- "BA"
# parameter <- 0

# plot(g)

#### Simulations
# param_vec <- seq(0.05, 1, by=0.05)
# param_vec <- seq(1, 10, by=1)
ambs <- 1
# fwprob <- 0.4

# list_X <- list(betweenness(g), degree(g), transitivity(g, "local"), max_path_length(g),
#                1/(betweenness(g)+0.01), 1/(degree(g)+0.01), 1/(transitivity(g, "local")+0.01), 1/(max_path_length(g)+0.01))
# names(list_X) <- c("betweenness", "degree", "clustering", 
#                    "max_path_length", "inv_betweenness", "inv_degree",
#                    "inv_clustering", "inv_max_path_length")
# name_stat <- c("degree", "betweenness", "clustering", "max_path_length")
# graph_stat <- list(degree(g), betweenness(g), transitivity(g, "local"), max_path_length(g))

# list_X <- list(closeness(g), betweenness(g), degree(g), page_rank(g)$vector, eigen_centrality(g)$vector)
# names(list_X) <- c("closeness", "betweenness", "degree", "page_rank", "eigen")

# graph_stat <- list(closeness(g), betweenness(g), degree(g), page_rank(g)$vector, eigen_centrality(g)$vector)
# name_stat <- c("closeness", "betweenness", "degree", "page_rank", "eigen")

graph_stat <- list(closeness(g), max_path_length(g), page_rank(g)$vector, eigen_centrality(g)$vector)
name_stat <- c("closeness", "max_path_length",  "page_rank", "eigen")

fwprob_vec <- seq(0.01, 0.30, by=0.01)

# estimators_stats <- graph_estimators_fwprob_pps(list_X, fwprob_vec,
#         g, n, nSimus_sample,
#         name_stat, graph_stat)


supplement_ff_simple <- graph_estimators_param_fwprob(fwprob_vec,
                              g, n, nSimus_sample, "forest_fire",
                              name_stat, graph_stat, method_first = "bernoulli")

## TODO: merge with forestfire_simple.rds
ff_simple <- readRDS("data/forestfire_simple.rds")
ff_simple_tot <- rbind(ff_simple, supplement_ff_simple)

## TODO: sort

# saveRDS(ff_simple_tot, file="data/forestfire_simple.rds")

## Conclusions from ff_fwprob_18022018_pps.rds:
## - only efficient to measure betweenneess
## - Efficient sampling designs: betweenness, degree (centralities?)

## TODO characterisation of ff model
## - Distance between top 2 units in degrees, in function of fwprobs
## - Local / Global clustering in function of fwprobs
