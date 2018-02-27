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

N <- 1000
nSimus <- 500
param_vec <- seq(0.05, 0.30, by=0.05)

intra_cor_ff <- foreach(k=1:length(param_vec), .combine=rbind) %do% {
  
  print(param_vec[k])
  
  current_df <-  foreach(j=1:nSimus, .combine=rbind) %do% {
    g <- forest.fire.game(N, param_vec[k], directed=F)
    c(
  param_vec[k],
  mean(sapply(1:vcount(g), neigh_sim, graph_stat = betweenness(g)), na.rm = T),
  mean(sapply(1:vcount(g), neigh_sim, graph_stat = closeness(g)), na.rm = T),
  mean(sapply(1:vcount(g), neigh_sim, graph_stat = page_rank(g)[[1]]), na.rm = T),
  mean(sapply(1:vcount(g), neigh_sim, graph_stat = transitivity(g, "local")), na.rm = T),
  mean(sapply(1:vcount(g), neigh_sim, graph_stat = max_path_length(g)), na.rm = T),
  mean(sapply(1:vcount(g), neigh_sim, graph_stat = degree(g)), na.rm = T),
  mean(sapply(1:vcount(g), neigh_sim, graph_stat = eigen_centrality(g)$vector), na.rm = T),
  mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = betweenness(g)), na.rm = T),
  mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = closeness(g)), na.rm = T),
  mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = page_rank(g)[[1]]), na.rm = T),
  mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = transitivity(g, "local")), na.rm = T),
  mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = max_path_length(g)), na.rm = T),
  mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = degree(g)), na.rm = T),
  mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = eigen_centrality(g)$vector), na.rm = T),
  mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = betweenness(g)), na.rm = T),
  mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = closeness(g)), na.rm = T),
  mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = page_rank(g)[[1]]), na.rm = T),
  mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = transitivity(g, "local")), na.rm = T),
  mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = max_path_length(g)), na.rm = T),
  mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = degree(g)), na.rm = T),
  mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = eigen_centrality(g)$vector), na.rm = T),
  mean(sapply(1:vcount(g), simple_deg_homogeneity), na.rm = T)
  )
  }
  
  current_df
  
}

intra_cor_ff <- data.frame(intra_cor_ff)

names_var <- c("betweenness","closeness","page_rank"
                         ,"clustering","max_path_length","eigen","degree")

names_intra_cor_ff <- c( "fwprobs",
                         paste("neigh_sim", names_var,sep="_"),
                         paste("deg_homogeneity", names_var,sep="_"),
                         paste("simple_neigh_sim", names_var,sep="_"),
                         "simple_deg_homogeneity")

names(intra_cor_ff) <- names_intra_cor_ff

intra_cor_ff_summary <- intra_cor_ff %>%
  group_by(fwprobs) %>%
  summarise_all(mean)

saveRDS(intra_cor_ff_summary, "data/intra_cor_ff.rds")

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
      mean(sapply(1:vcount(g), neigh_sim, graph_stat = betweenness(g)), na.rm = T),
      mean(sapply(1:vcount(g), neigh_sim, graph_stat = closeness(g)), na.rm = T),
      mean(sapply(1:vcount(g), neigh_sim, graph_stat = page_rank(g)[[1]]), na.rm = T),
      mean(sapply(1:vcount(g), neigh_sim, graph_stat = transitivity(g, "local")), na.rm = T),
      mean(sapply(1:vcount(g), neigh_sim, graph_stat = max_path_length(g)), na.rm = T),
      mean(sapply(1:vcount(g), neigh_sim, graph_stat = degree(g)), na.rm = T),
      mean(sapply(1:vcount(g), neigh_sim, graph_stat = eigen_centrality(g)$vector), na.rm = T),
      mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = betweenness(g)), na.rm = T),
      mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = closeness(g)), na.rm = T),
      mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = page_rank(g)[[1]]), na.rm = T),
      mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = transitivity(g, "local")), na.rm = T),
      mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = max_path_length(g)), na.rm = T),
      mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = degree(g)), na.rm = T),
      mean(sapply(1:vcount(g), deg_homogeneity, graph_stat = eigen_centrality(g)$vector), na.rm = T),
      mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = betweenness(g)), na.rm = T),
      mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = closeness(g)), na.rm = T),
      mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = page_rank(g)[[1]]), na.rm = T),
      mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = transitivity(g, "local")), na.rm = T),
      mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = max_path_length(g)), na.rm = T),
      mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = degree(g)), na.rm = T),
      mean(sapply(1:vcount(g), simple_neigh_sim, graph_stat = eigen_centrality(g)$vector), na.rm = T),
      mean(sapply(1:vcount(g), simple_deg_homogeneity), na.rm = T)
    )
  
}


intra_cor_real_graphs <- data.frame(intra_cor_real_graphs)

names_var <- c("betweenness","closeness","page_rank"
               ,"clustering","max_path_length","eigen","degree")

names_intra_cor_real_graphs <- c( "fwprobs",
                         paste("neigh_sim", names_var,sep="_"),
                         paste("deg_homogeneity", names_var,sep="_"),
                         paste("simple_neigh_sim", names_var,sep="_"),
                         "simple_deg_homogeneity")

names(intra_cor_real_graphs) <- names_intra_cor_real_graphs

saveRDS(intra_cor_real_graphs, "data/intra_cor_real_graphs.rds")
