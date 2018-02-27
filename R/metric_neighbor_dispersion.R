library(tidyverse)
library(igraph)
library(igraphdata)
library(foreach)
set.seed(1005192119)
source("R/snowball.R")
source("R/metrics.R")

# data("yeast")
# g <- yeast
# 
# graph_stat <- betweenness(g)

neigh_sim <- function(v, graph_stat) {
  (mean(graph_stat[adjacent_vertices(g, v, "total")[[1]]], na.rm=T) - 
     mean(graph_stat, na.rm=T))**2*degree(g)[v]
}

deg_homogeneity <- function(v, graph_stat) {
  (degree(g)[v] - mean(degree(g), na.rm=T)) * mean(graph_stat[adjacent_vertices(g, v, "total")[[1]]], na.rm=T)**2
}

simple_deg_homogeneity <- function(v) {
  (degree(g)[v] - mean(degree(g), na.rm=T))
}

simple_neigh_sim <- function(v, graph_stat) {
  (mean(graph_stat[adjacent_vertices(g, v, "total")[[1]]], na.rm=T) - 
     mean(graph_stat, na.rm=T))**2
}

deff_estimator_1 <- function(g, graph_stat) {
  mean(sapply(1:vcount(g), neigh_sim, graph_stat = graph_stat), na.rm = T) /
    mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = graph_stat), na.rm = T)
}

deff_estimator_2 <- function(g, graph_stat) {
  mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = graph_stat), na.rm = T) /
    mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = betweenness(g)), na.rm = T)
}

get_deffs_byvar <- function(g) {
  
  c(deff_estimator_1(g, betweenness(g)),
  deff_estimator_1(g, closeness(g)),
  deff_estimator_1(g, page_rank(g)[[1]]),
  deff_estimator_1(g, transitivity(g, "local")),
  deff_estimator_1(g, max_path_length(g)),
  deff_estimator_1(g, degree(g)),
  deff_estimator_1(g, eigen_centrality(g)$vector),
  deff_estimator_2(g, betweenness(g)),
  deff_estimator_2(g, closeness(g)),
  deff_estimator_2(g, page_rank(g)[[1]]),
  deff_estimator_2(g, transitivity(g, "local")),
  deff_estimator_2(g, max_path_length(g)),
  deff_estimator_2(g, degree(g)),
  deff_estimator_2(g, eigen_centrality(g)$vector)
  )
}

N <- 1000
nSimus <- 25
param_vec <- seq(0.05, 0.30, by=0.05)

intra_cor_ff <- foreach(k=1:length(param_vec), .combine=rbind) %do% {
  
  print(param_vec[k])
  
  current_df <-  foreach(j=1:nSimus, .combine=rbind) %do% {
    g <- forest.fire.game(N, param_vec[k], directed=F)
    c(
  param_vec[k],
  get_deffs_byvar(g)
  )
  }
  
  current_df
  
}

intra_cor_ff <- data.frame(intra_cor_ff)

# betweenness(g)
# closeness(g)
# page_rank(g)[[1]]
# transitivity(g, "local")
# max_path_length(g)
# degree(g)
# eigen_centrality(g)$vector

names_var <- c("betweenness","closeness","page_rank"
                         ,"clustering","max_path_length","eigen","degree")

names_intra_cor_ff <- c( "fwprobs",
                         paste("deff1", names_var,sep="_"),
                         paste("deff2", names_var,sep="_"))

names(intra_cor_ff) <- names_intra_cor_ff

intra_cor_ff_summary <- intra_cor_ff %>%
  group_by(fwprobs) %>%
  summarise_all(mean)

saveRDS(intra_cor_ff_summary, "data/intra_cor_ff_deffs.rds")

## For all graphs in igraphdata

d <- data(package = "igraphdata")
nm <- d$results[, "Item"][c(3,6,11)]
list_datasets <- mget(data(list = nm))
names(list_datasets)


intra_cor_real_graphs <- foreach(k=1:length(list_datasets), .combine=rbind) %do% {
  
  g <- list_datasets[[k]]
  print(names(list_datasets)[k])

    c(
      names(list_datasets)[k],
      get_deffs_byvar(g)
    )
  
}


intra_cor_real_graphs <- data.frame(intra_cor_real_graphs)

names_var <- c("betweenness","closeness","page_rank"
               ,"clustering","max_path_length","eigen","degree")

names_intra_cor_real_graphs <- c( "graph_name",
                         paste("deff1", names_var,sep="_"),
                         paste("deff2", names_var,sep="_"))

names(intra_cor_real_graphs) <- names_intra_cor_real_graphs

saveRDS(intra_cor_real_graphs, "data/intra_cor_real_graphs_deffs.rds")


## Barabasi-Albert
  
intra_cor_BA <-  foreach(j=1:nSimus, .combine=rbind) %do% {
  g <- barabasi.game(N, directed = F)
  c(
    "BA",
    get_deffs_byvar(g)
  )
}
  

names_var <- c("betweenness","closeness","page_rank"
               ,"clustering","max_path_length","eigen","degree")

intra_cor_BA <- data.frame(intra_cor_BA)
intra_cor_BA[,2:ncol(intra_cor_BA)] <- apply(intra_cor_BA[,2:ncol(intra_cor_BA)], c(1,2), as.numeric)

names_intra_cor_BA <- c( "name",
                         paste("deff1", names_var,sep="_"),
                         paste("deff2", names_var,sep="_"))

names(intra_cor_BA) <- names_intra_cor_BA

intra_cor_BA_summary <- intra_cor_BA
# intra_cor_BA_summary$deff1_clustering <- 0
# intra_cor_BA_summary$deff2_clustering <- 0

intra_cor_BA_summary <- intra_cor_BA_summary %>%
  group_by(name) %>%
  summarise_all(mean)

saveRDS(intra_cor_BA_summary, "data/intra_cor_BA_deffs.rds")

#### Bind all deffs estimates


deffs_networks <- rbind(intra_cor_ff_summary %>% rename(name=fwprobs),
                        intra_cor_real_graphs %>% rename(name=graph_name),
                        intra_cor_BA_summary)
row.names(deffs_networks) <- NULL

saveRDS(deffs_networks, "data/deffs_networks.rds")

