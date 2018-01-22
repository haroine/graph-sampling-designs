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