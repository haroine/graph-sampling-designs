library(tidyverse)
library(foreach)
library(sampling)

#' Draw a (1-degree) snowball sample and compute weights
#' @param g population graph (igraph object)
#' @param n size of the initial sample
#' @param method_first Can be "srwsor" (simple random sampling without replacement), 
#' "bernoulli", "pps" (Poisson proportional-to-size),
#' "fixed.pps" (fixed size unequal probability sampling)
#' @param X size vector to determing pps probabilities
snowball_sample <- function(g, n, method_first="bernoulli", X=NULL) {
  
  N <- vcount(g)
  
  ## TODO other cases for method_first
  if(method_first == "bernoulli") {
    piks <- rep(n/N, N) 
    sample_first <- which(UPpoisson(piks) == 1)
  }
  
  if(method_first == "poisson.pps" && !is.null(X)) {
    
    if(length(X[X <= 0]) > 0) {
      piks <- abs(X)+0.01
      piks[is.na(piks)] <- 0.01
      piks <- inclusionprobabilities(piks, n)  ## pps
    } else {
      piks <- X
      piks <- inclusionprobabilities(X, n)  ## pps
    }
    
    sample_first <- which(UPpoisson(piks) == 1)
  }
  
  # if(method_first == "srswor") {
  #   sample_first <- srswor(n, N)
  # }
  # 
  # if(method_first == "fixed.pps" && !is.null(X)) {
  #   piks <- inclusionprobabilities(X, n)  ## pps
  #   sample_first <- which(UPsystematic(piks) == 1)
  # }
  
  weights_first <- 1/(piks[sample_first])
  
  sample_snowball <- unique(c(sample_first,
                              unlist(adjacent_vertices(g,sample_first, mode="out"))))
  
  if(method_first == "bernoulli") {

    ## In degree of every vertex in snowball sample
    indeg <- sapply(sample_snowball, function(x) {
      length(unique(c(x,
                      unlist(adjacent_vertices(graph = g, v = x, mode = "in"))))
      )
    })
    
    weights <- 1/(1-(1-n/N)**indeg)
  }
  
  if(method_first == "poisson.pps" && !is.null(X)) {
    
    weights <- sapply(sample_snowball, function(x) {
      before <- unique(c(x,adjacent_vertices(graph = g, v = x, mode = "in")[[1]]))
      return(1/(1-prod(1-piks[before])))
    })
  }
  
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
get_snowball_sim <- function(g, n, nSimus_sample,
                             name_stat, graph_stat, 
                             method_first="bernoulli", X=NULL) {
  
  ## TODO rewrite not using rbinds
  
  if(length(name_stat) != length(graph_stat)) {
    stop("name_stat and graph_stat must have same lengths")
  }
  
  df_sim_final <- NULL
  
  for(k in 1:length(name_stat)) {
    
    matrix_sims <-     foreach(j=1:nSimus_sample, .combine=rbind) %do% {
      snowball_sample_obj <- snowball_sample(g, n, 
                       method_first = method_first, X = X)
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

#' Denominator for Deff
#' @param n sample size
#' @param N population size
#' @param s2 variable dispersion in population
deff_denom <- function(n, N, s2) { 
	return( (1-n/N)*s2/n  ) 
}

#' Get estimators stats from simulation results
#' @param results_sample data frame of simulation results,
#' generated by function get_snowball_sim
get_estimators_stats <- function(results_sample, name, parameter) {
  
  estimators_stats <- results_sample %>% 
    group_by(stat_name) %>% 
    summarise_at(vars(stat_value, se_simple, se_snowball, 
                      bias_simple, bias_snowball,
                      size_simple, size_snowball, stat_disp), 
                 mean, na.rm = T) 
  
  estimators_stats$name <- name
  estimators_stats$parameter <- parameter
  
  estimators_stats$relative_bias_simple <- estimators_stats$bias_simple / estimators_stats$stat_value
  estimators_stats$relative_bias_snowball <- estimators_stats$bias_snowball / estimators_stats$stat_value
  
  estimators_stats$var_simple <- estimators_stats$se_simple - (estimators_stats$bias_simple)**2
  estimators_stats$cv_simple <- sqrt(estimators_stats$var_simple) / abs(estimators_stats$stat_value)
  
  estimators_stats$var_snowball <- estimators_stats$se_snowball - (estimators_stats$bias_snowball)**2
  estimators_stats$cv_snowball <- sqrt(estimators_stats$var_snowball) / abs(estimators_stats$stat_value)
  
  estimators_stats$deff_simple <- estimators_stats$var_simple / 
    deff_denom(estimators_stats$size_simple, N, estimators_stats$stat_disp)
  
  estimators_stats$deff_snowball <- estimators_stats$var_snowball / 
    deff_denom(estimators_stats$size_snowball, N, estimators_stats$stat_disp)
  
  return(estimators_stats)
}

#' Wrapper to get simulations of graph sampling design
#' efficiency
graph_estimators <- function(g, n, nSimus_sample, name, parameter,
                             name_stat, graph_stat,
                             method_first="bernoulli", X=NULL) {
  
  results_sample <- get_snowball_sim(g, n, nSimus_sample, 
                                     name_stat, 
                                     graph_stat, 
                                     method_first = method_first, X = X)
  
  return(get_estimators_stats(results_sample, name, parameter))
}


#' Get graph estimators for a sequence of varying fw.prob
#' @param fwprob_vec vector of fw.prob
graph_estimators_param_fwprob <- function(fwprob_vec,
                                          g, n, nSimus_sample, name,
                                          name_stat, graph_stat,
                                          method_first = method_first, X = X) {
  
  estimators_stats <- foreach(param=fwprob_vec, .combine=rbind) %do% {
    print(param)
    g <- forest.fire.game(nodes = N, fw.prob = param, 
                          ambs = ambs, directed=F)
    parameter <- param
    graph_estimators(g, n, nSimus_sample, name, parameter,
                     name_stat,
                     graph_stat,
                     method_first = method_first, X = X)
    
    
  }
  
  return(estimators_stats)
  
}


#' Browse list of fw.prob and pps sampling design,
#' get estimators
#' @param list_X, name list of vectors for pps sampling design
graph_estimators_fwprob_pps <- function(list_X, fwprob_vec,
                                        g, n, nSimus_sample,
                                        name_stat, graph_stat) {
  
  foreach(k=1:(length(list_X)), .combine=rbind) %do% {
    currentX <- list_X[[k]]
    current_name <- names(list_X)[k]
    print(paste("----", current_name))
    graph_estimators_param_fwprob(fwprob_vec,
                                  g, n, nSimus_sample, current_name,
                                  name_stat, graph_stat,
                                  method_first = "poisson.pps", X = currentX)
  }
  
}