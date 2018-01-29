library(igraph)
library(foreach)
library(sampling)
library(tidyverse)
source("R/snowball.R")
#### Model graph
N <- 10000
nSimus_sample <- 100
n <- 50

# set.seed(1005192119)

g <- watts.strogatz.game(1, N, N*0.2, 0.4)
# g <- barabasi.game(N, directed = F)
# g <- make_ring(N, circular = T)
# g <- graph_from_adjacency_matrix(matrix(1,nrow=N,ncol=N))
# g <- forest.fire.game(N, 0.35, directed=F)
# g %>% rewire(each_edge(p = .2, loops = FALSE))
# get.adjacency(g)

# transitivity(g, "local")
# plot(g)


# Y <- rep(1,N)
# Y <- alpha_centrality(g)
# X <- degree(g)
# Y <- X + rnorm(length(X), mean(X), sd(X))

stats_toestimate <- c(ecount(g), vcount(g), mean(degree(g)), 
                      mean(alpha_centrality(g)),
                      mean(transitivity(g, "local"), na.rm=T))
names(stats_toestimate) <- c("order","size","degree",
                             "centrality_mean",
                             "clustering_mean")

#### Sampling
results_sample <- get_snowball_sim(g, n, nSimus_sample, 
                 c("degree", "alpha_centrality", "clustering"), 
                 list(degree(g), alpha_centrality(g), transitivity(g, "local")) )

## Compute estimators stats

estimators_stats <- results_sample %>% 
  group_by(stat_name) %>% 
  summarise_at(vars(stat_value, se_simple, se_snowball, 
                    bias_simple, bias_snowball,
                    size_simple, size_snowball, stat_disp), 
               mean, na.rm = T) 

estimators_stats$var_simple <- estimators_stats$se_simple - (estimators_stats$bias_simple)**2
estimators_stats$cv_simple <- sqrt(estimators_stats$var_simple) / abs(estimators_stats$stat_value)

estimators_stats$var_snowball <- estimators_stats$se_snowball - (estimators_stats$bias_snowball)**2
estimators_stats$cv_snowball <- sqrt(estimators_stats$var_snowball) / abs(estimators_stats$stat_value)

deff_denom <- function(n, N, s2) { return( (1-n/N)*s2/n  ) }
estimators_stats$deff_simple <- estimators_stats$var_simple / 
  deff_denom(estimators_stats$size_simple, N, estimators_stats$stat_disp)

estimators_stats$deff_snowball <- estimators_stats$var_snowball / 
  deff_denom(estimators_stats$size_snowball, N, estimators_stats$stat_disp)


###### Induced subgraphs

# g_test <- induced_subgraph(g, test$sample_first)
# g_test2 <- induced_subgraph(g, test$sample_snowball)
# plot(g)
# plot(g_test)
# plot(g_test2)