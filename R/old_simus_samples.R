
simus_samples <- foreach(i=1:nSimus_sample, .combine = rbind) %do% {
  
  
  ## First stage sample (Poisson)
  piks <- rep(n/N, N)  ## Bernoulli
  piks <- inclusionprobabilities(X, n)  ## pps
  # sample_first <- srswor(n,N) ## Simple random sampling
  
  sample_first <- UPpoisson(piks)
  sample_first <- which(sample_first==1)
  
  est_first_HT <- sum(Y[sample_first]*N/n)
  
  sample_snowball <- unique(c(sample_first,
                              unlist(adjacent_vertices(g,sample_first, mode="out"))))
  y <- Y[sample_snowball]
  
  weights <- sapply(sample_snowball, function(x) {
    length(unique(c(x,
                    unlist(adjacent_vertices(graph = g, v = x, mode = "in"))))
    )
  })
  weights <- 1/(1-(1-n/N)**weights)
  
  est_snowball <- sum(y*weights)
  
  c(est_first_HT,length(sample_first),est_snowball,length(sample_snowball))
}

testNormality <- simus_samples[,3]
qqnorm(testNormality)
qqline(testNormality, col = 2)

## TODO : calcul des deff
## TODO : Tableau des correlations pour la redaction de l'information auxiliaire