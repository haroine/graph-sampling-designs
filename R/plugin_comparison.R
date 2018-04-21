#### Simulations versus plugin sampling
source("R/snowball.R")
source("R/metrics.R")
library(igraph)
library(igraphdata)
library(foreach)

N <- 1e5
nSims <- 500

set.seed(1005192119)
g <- forest.fire.game(N, 0.25)

graph_stat <- list(closeness(g), betweenness(g), degree(g), page_rank(g)$vector, eigen_centrality(g)$vector)
name_stat <- c("closeness", "betweenness", "degree", "page_rank", "eigen")

# n <- 200
sample_sizes_sim <- c(0.0001, 0.0005, 0.001, 0.005, 
                      0.01, 0.025, 0.05, 0.075)* N
result_forplugin <- foreach(n=sample_sizes_sim, .combine=rbind) %do% {
partial_forplugin <- foreach(k=1:length(name_stat), .combine=rbind) %do% {
  
  current_name_stat <- name_stat[k]
  print(current_name_stat)
  print(n)
  
  sims_forplugin <- foreach(j=1:nSims, .combine = rbind) %do% {
    
    snowsample_forplugin <- snowball_sample(g, n)
    
    selected_vertices <- snowsample_forplugin$sample_snowball
    weights <- snowsample_forplugin$weights
    
    ## Hajek
    size_est <- sum(weights, na.rm=T)
    
    graph_stat_current <- graph_stat[[k]]
    
    real_stat_value <- mean(graph_stat_current, na.rm=T)
    plugin_estimate <- mean(graph_stat_current[selected_vertices], na.rm=T)
    weighted_estimate <- sum(graph_stat_current[selected_vertices] * (weights), na.rm=T)  / size_est
    
    c(
      real_stat_value,
      length(selected_vertices),
      plugin_estimate,
      (plugin_estimate - real_stat_value)**2,
      (plugin_estimate - real_stat_value),
      abs(plugin_estimate - real_stat_value),
      weighted_estimate,
      (weighted_estimate - real_stat_value)**2,
      (weighted_estimate - real_stat_value),
      abs(weighted_estimate - real_stat_value)
    )
  }
  
  sims_forplugin_agg <- c(n, current_name_stat, apply(sims_forplugin, 2, mean))
}
}

result_forplugin <- data.frame(result_forplugin, stringsAsFactors = F)

names(result_forplugin) <- c("initial_sample_size",
                             "graph_stat",
                             "stat_value",
                             "snowball_sample_size",
                             "plugin_estimate", "MSE_plugin",
                             "bias_plugin","MAE_plugin",
                             "weighted_estimate", "MSE_weighted",
                             "bias_weighted","MAE_weighted")

saveRDS(result_forplugin, "data/MSE_plugin_weighted_2.rds")
