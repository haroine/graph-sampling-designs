library(igraph)
library(foreach)
library(sampling)
source("R/snowball.R")
#### Model graph
N <- 25

set.seed(1005192119)

g <- forest.fire.game(N, 0.35, directed=F)


#' Decides whehter unit is a drug user or not
#' (depends on local clustering coefficient and random factor)
#' @param g Population graph
does_drugs <- function(g) {
  boolReturn <- transitivity(g, "local") > 0.3 & runif(gorder(g)) > 0.35
  boolReturn[is.na(boolReturn)] <- FALSE
  return( boolReturn )
}

#'
#' @param g population graph
#' @param does_drugs_vec boolean vector of size order of g,
#' computed 
amount_spent <- function(g, does_drugs_vec) {
  
  amountReturn <- as.numeric(does_drugs_vec)
  amountReturn <- amountReturn*(degree(g)*10 + rnorm(gorder(g), 10))
  amountReturn <- round(amountReturn/10, 0)*10
  
  return(amountReturn)
}

does_drugs_vec <- does_drugs(g)
V(g)$color <- ifelse(does_drugs_vec, "red", "yellow")
V(g)$label <- amount_spent(g, does_drugs_vec)

plot(g)

# 
# factor <- 0.4
# pdf("drug_use.pdf",width=factor*21,height=factor*29.7,paper='special')
# plot(g)
# dev.off()

