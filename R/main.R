library(igraph)
library(foreach)
library(sampling)
source("snowball.R")
#### Model graph
N <- 25

set.seed(1005192119)

# g <- watts.strogatz.game(1, N, N*0.2, 0.4)
# g <- barabasi.game(N)
# g <- make_ring(N, circular = T)
# g <- graph_from_adjacency_matrix(matrix(1,nrow=N,ncol=N))
g <- forest.fire.game(N, 0.35, directed=F)
# g %>% rewire(each_edge(p = .2, loops = FALSE))
# get.adjacency(g)

transitivity(g, "local")
plot(g)


# Y <- rep(1,N)
# Y <- alpha_centrality(g)
X <- degree(g)
Y <- X + rnorm(length(X), mean(X), sd(X))

#### Sampling
nSimus_sample <- 10
n <- 10

test <- snowball_sample(g, n)
