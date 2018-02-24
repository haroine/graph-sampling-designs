
#' Return a vector with max path length to any other vertex
#' in graph
max_path_length <- function(g) {
  return(apply(distances(g), 1, max))
}

## TODO Distances between vertices with highest degrees, centralities, etc.
## TODO Describe datasets