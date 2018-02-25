library(igraph)
library(igraphdata)
library(foreach)
library(sampling)
library(tidyverse)
source("R/snowball.R")
source("R/metrics.R")

#### Model graph
nSimus_sample <- 500
n <- 50
set.seed(1005192119)

data(yeast)
g <- yeast

## Just for tests
# list_X <- list(closeness(g), betweenness(g))
# names(list_X) <- c("closeness", "betweenness")


# list_X <- list(NULL, closeness(g), betweenness(g), degree(g), page_rank(g)$vector,
#                eigen_centrality(g)$vector, transitivity(g, "local"))
# names(list_X) <- c("bernoulli", "closeness", "betweenness", "degree"
#                    , "page_rank", "eigen", "clustering")

graph_stat <- list(closeness(g), betweenness(g), degree(g), page_rank(g)$vector,
                   eigen_centrality(g)$vector, transitivity(g, "local"))
name_stat <- c("closeness", "betweenness", "degree"
               , "page_rank", "eigen", "clustering")

parameter <- "yeast"
# method_first <- c("bernoulli")

list_X <- list(NULL)
method_first_vec <- c("bernoulli")

yeast_estimators_bernoulli <- foreach(k=1:(length(list_X)), .combine=rbind) %do% {
    currentX <- list_X[[k]]
    current_name <- names(list_X)[k]
    method_first <- method_first_vec[k]
    print(paste("----", current_name))
    print(method_first)
    
    current_df <- graph_estimators(g, n, nSimus_sample, current_name, parameter,
                     name_stat,
                     graph_stat,
                     method_first = method_first, X = currentX)
    
}


saveRDS(current_df, "data/yeast_estimators_bernoulli.rds")

  