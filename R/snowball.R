library(tidyverse)
library(foreach)

## TODO doc
## TODO output graph from snowball sample
## Only for 1-degree snowball sampling
## method_first: "srwsor", "bernoulli", "pps" (Poisson proportional-to-size),
## "fixed.pps"
snowball_sample <- function(g, n, method_first="bernoulli", X=NULL) {
  
  N <- vcount(g)
  
  ## TODO other cases for method_first
  if(method_first == "bernoulli") {
    piks <- rep(n/N, N) 
    sample_first <- which(UPpoisson(piks) == 1)
  }
  
  if(method_first == "poisson.pps" && !is.null(X)) {
    piks <- inclusionprobabilities(X, n)  ## pps
    sample_first <- which(UPpoisson(piks) == 1)
  }
  
  if(method_first == "srswor") {
    sample_first <- srswor(n, N)
  }
  
  if(method_first == "fixed.pps" && !is.null(X)) {
    piks <- inclusionprobabilities(X, n)  ## pps
    sample_first <- which(UPsystematic(piks) == 1)
  }
  
  weights_first <- 1/(piks[sample_first])
  
  sample_snowball <- unique(c(sample_first,
                              unlist(adjacent_vertices(g,sample_first, mode="out"))))
  
  weights <- sapply(sample_snowball, function(x) {
    length(unique(c(x,
                    unlist(adjacent_vertices(graph = g, v = x, mode = "in"))))
    )
  })
  weights <- 1/(1-(1-n/N)**weights)
  
  ## TODO name results
  return(list(sample_snowball=sample_snowball, 
              weights=weights, sample_first=sample_first, 
              weights_first=weights_first))
}



#' Compute the sampling estimates for vertices stats
#' @param graph_stat vector of size number of vertices,
#' value for each vertex
#' @param weights vector of size number of vertices, sampling
#' weight for each vertex
#' @param type_est "Hajek" or "HT"
estimate_vertex_stat_mean <- function(graph_stat, selected_vertices,
                                      weights, type_est, N=NULL) {
  
  if( type_est != "Hajek" && !is.null(N) ) {
    size_est <- N
  } else {
    size_est <- sum(weights)
  }
  
  return( sum(graph_stat[selected_vertices] * (weights), na.rm=T) / size_est )
  
}


#' Return real statistic and estimators with simple sampling
#' design and 1-snowball (useful for simulations)
#' @param graph_stat function
#' @param snowball_sample
estimators_snowball <- function(graph_stat, snowball_sample, N) {
  
  return( c(mean(graph_stat, na.rm=T), 
            
            var(graph_stat, na.rm=TRUE),
            
            estimate_vertex_stat_mean(graph_stat, snowball_sample$sample_first,
                                      snowball_sample$weights_first, type_est = "HT", N = N),
            
            length(snowball_sample$sample_first),
            
            estimate_vertex_stat_mean(graph_stat, snowball_sample$sample_snowball,
                                      snowball_sample$weights, type_est = "HT", N = N),
            
            length(snowball_sample$sample_snowball)
  ) )
  
}


#' Simulations
#' @param graph_stat list of vecs of size N with stat value for each vertex 
get_snowball_sim <- function(g, n, nSimus_sample, name_stat, graph_stat) {
  
  ## TODO rewrite not using rbinds
  
  if(length(name_stat) != length(graph_stat)) {
    stop("name_stat and graph_stat must have same lengths")
  }
  
  df_sim_final <- NULL
  
  for(k in 1:length(name_stat)) {
    
    matrix_sims <-     foreach(j=1:nSimus_sample, .combine=rbind) %do% {
      snowball_sample_obj <- snowball_sample(g, n)
      c(name_stat[k], estimators_snowball(graph_stat[[k]], snowball_sample_obj, vcount(g)) )
    }
    
    df_sim <- as_tibble(matrix_sims)
    names(df_sim) <- c("stat_name","stat_value","stat_disp",
                       "est_simple","size_simple","est_snowball","size_snowball")
    
    df_sim_final <- rbind(df_sim, df_sim_final)
  }
  
  df_sim_final <- df_sim_final %>% mutate_at(vars(-stat_name), as.numeric)
  
  df_sim_final$se_simple <- (df_sim_final$est_simple - df_sim_final$stat_value)**2
  df_sim_final$se_snowball <- (df_sim_final$est_snowball - df_sim_final$stat_value)**2
  
  df_sim_final$bias_simple <- df_sim_final$est_simple - df_sim_final$stat_value
  df_sim_final$bias_snowball <- df_sim_final$est_snowball - df_sim_final$stat_value
  
  
  return(df_sim_final)
}