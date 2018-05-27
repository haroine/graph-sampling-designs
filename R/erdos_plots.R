set.seed(1005192119)

p <- 0.01
N <- 100

er <- erdos.renyi.game(N, p, type = c("gnp", "gnm"))

plot.igraph(er, width=2, vertex.size=2, vertex.label=NA, layout=layout_nicely)

p <- 0.05
N <- 100

er <- erdos.renyi.game(N, p, type = c("gnp", "gnm"))

plot.igraph(er, width=2, vertex.size=2, vertex.label=NA, layout=layout_nicely)