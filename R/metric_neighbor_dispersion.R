library(tidyverse)
library(igraph)
library(igraphdata)

data("yeast")
g <- yeast

graph_stat <- betweenness(g)

neigh_sim <- function(v, graph_stat) {
  sd(graph_stat[adjacent_vertices(g, v, "total")[[1]]])
}

mean(sapply(1:vcount(g), neigh_sim, graph_stat = betweenness(g)), na.rm = T)
mean(sapply(1:vcount(g), neigh_sim, graph_stat = closeness(g)), na.rm = T)
mean(sapply(1:vcount(g), neigh_sim, graph_stat = page_rank(g)[[1]]), na.rm = T)
mean(sapply(1:vcount(g), neigh_sim, graph_stat = transitivity(g, "local")), na.rm = T)

