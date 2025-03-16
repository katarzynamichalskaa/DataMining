library(igraph)

rm(list = ls())
setwd(getwd())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

degree_prestige <- function(graph) {
  adj_matrix <- as_adjacency_matrix(graph, sparse = FALSE)
  
  in_degree <- colSums(adj_matrix)
  
  degree_values <- in_degree
  
  n <- vcount(graph)
  
  prestige <- degree_values / (n - 1)
  
  return(prestige)
}

local_clustering_coefficient <- function(graph) {
  adj_matrix <- as_adjacency_matrix(graph, sparse = FALSE) 
  
  n <- nrow(adj_matrix)  
  
  local_clustering <- numeric(n)  
  
  for (i in 1:n) {
    neighbors <- which(adj_matrix[i,] == 1)  
    num_neighbors <- length(neighbors)  
    
    if (num_neighbors < 2) {
      local_clustering[i] <- 0  
    } else {
      num_edges_between_neighbors <- 0
      for (j in 1:(num_neighbors - 1)) {
        for (k in (j + 1):num_neighbors) {
          if (adj_matrix[neighbors[j], neighbors[k]] == 1 || adj_matrix[neighbors[k], neighbors[j]] == 1) {
            num_edges_between_neighbors <- num_edges_between_neighbors + 1
          }
        }
      }
      
      local_clustering[i] <- (2 * num_edges_between_neighbors) / (num_neighbors * (num_neighbors - 1))
    }
  }
  
  return(local_clustering)
}

degree_centrality <- function(graph) {
  adj_matrix <- as_adjacency_matrix(graph, sparse = FALSE)
  
  out_degree <- rowSums(adj_matrix)
  
  in_degree <- colSums(adj_matrix)
  
  degree_values <- out_degree + in_degree
  
  n <- vcount(graph)
  
  degree_centralities <- degree_values / (n - 1)
  
  return(degree_centralities)
}

degree_gregariousness <- function(graph) {
  adj_matrix <- as_adjacency_matrix(graph, sparse = FALSE)
  
  out_degree <- rowSums(adj_matrix)
  
  degree_values <- out_degree
  
  n <- vcount(graph)
  
  gregariousness <- degree_values / (n - 1)
  
  return(gregariousness)
}

closeness_centrality <- function(graph) {
  shortest_paths_matrix <- shortest.paths(graph, mode = "all", weights = NULL)
  
  n <- vcount(graph)
  
  centralities <- numeric(n)
  
  for (v in 1:n) {
    sum_shortest_paths <- sum(shortest_paths_matrix[v, ][-v], na.rm = TRUE)
    
    centralities[v] <- (n - 1) / sum_shortest_paths
  }
  
  return(centralities)
}


proximity_prestige <- function(graph) {
  n <- vcount(graph)
  
  prestige_values <- numeric(n)
  
  for (v in 1:n) {
    influence_v <- neighbors(graph, v, mode = "out") 
    
    sum_influence <- 0
    for (i in influence_v) {
      dist_i_v <- distances(graph, v = i, to = v, mode = "out")
      
      if (!is.infinite(dist_i_v)) {
        sum_influence <- sum_influence + length(neighbors(graph, i, mode = "out")) / dist_i_v
      }
    }
    
    prestige_values[v] <- sum_influence / (n - 1)
  }
  
  return(prestige_values)
}

betweenness_centrality <- function(graph) {
  n <- vcount(graph)
  
  betweenness_values <- numeric(n)
  
  for (v in 1:n) {
    count_paths_through_v <- 0
    
    for (s in 1:n) {
      if (s != v) {
        for (t in 1:n) {
          if (t != v && t != s) {
            sp <- shortest_paths(graph, from = s, to = t, output = "both")
            
            total_paths <- length(sp$vpath) 
            paths_through_v <- sum(sapply(sp$vpath, function(path) v %in% path))
            
            count_paths_through_v <- count_paths_through_v + (paths_through_v / total_paths)
          }
        }
      }
    }
    
    betweenness_values[v] <- count_paths_through_v / ((n - 1) * (n - 2))
  }
  
  return(betweenness_values)
}


common_neighbor_measure <- function(edges, i, j) {
  neighbors_i <- unique(c(edges$to[edges$from == i], edges$from[edges$to == i]))
  
  neighbors_j <- unique(c(edges$to[edges$from == j], edges$from[edges$to == j]))
  
  common_neighbors <- intersect(neighbors_i, neighbors_j)
  
  return(length(common_neighbors))
}

jaccard_measure <- function(edges, i, j) {
  neighbors_i <- unique(c(edges$to[edges$from == i], edges$from[edges$to == i]))
  neighbors_j <- unique(c(edges$to[edges$from == j], edges$from[edges$to == j]))
  
  common_neighbors <- length(intersect(neighbors_i, neighbors_j))
  union_neighbors <- length(unique(c(neighbors_i, neighbors_j)))
  
  return(common_neighbors / union_neighbors)
}

# read data
nodes <- read.csv("Dataset1-Media-Example-NODES.csv")
edges <- read.csv("Dataset1-Media-Example-EDGES.csv")

# plot graph
graph <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# simplify
graph <- simplify(graph, remove.loops = TRUE, remove.multiple = TRUE)
undirected = as_undirected(graph)

plot(graph, vertex.size=15, vertex.color="pink", edge.arrow.size=0.2, 
     main="")

# degree centrality
custom_degree_centrality <- degree_centrality(graph)
builtin_degree_centrality <- degree(graph, normalized = TRUE)

degree_centrality_comparison <- data.frame(
  Node = V(graph)$name,
  Custom = custom_degree_centrality,
  Built_in = builtin_degree_centrality,
  Match = custom_degree_centrality == builtin_degree_centrality
)
print(degree_centrality_comparison)


# local clustering coefficient
custom_local_clustering <- local_clustering_coefficient(undirected)
builtin_local_clustering <- transitivity(undirected, type = "local")

local_clustering_comparison <- data.frame(
  Node = V(undirected)$name,
  Custom = custom_local_clustering,
  Built_in = builtin_local_clustering,
  Match = custom_local_clustering == builtin_local_clustering 
)
print(local_clustering_comparison)

# degree prestige
custom_degree_prestige <- degree_prestige(graph)
builtin_degree_prestige <- degree(graph, mode = "in", normalized = TRUE)

degree_prestige_comparison <- data.frame(
  Node = V(graph)$name,
  Custom = custom_degree_prestige,
  Built_in = builtin_degree_prestige,
  Match = custom_degree_prestige == builtin_degree_prestige  
)
print(degree_prestige_comparison)

# degree gregariousness
custom_degree_gregariousness <- degree_gregariousness(graph)
builtin_degree_gregariousness <- degree(graph, mode = "out", normalized = TRUE)

degree_gregariousness_comparison <- data.frame(
  Node = V(graph)$name,
  Custom = custom_degree_gregariousness,
  Built_in = builtin_degree_gregariousness,
  Match = custom_degree_gregariousness == builtin_degree_gregariousness
)
print(degree_gregariousness_comparison)
 
# closest centrality
g.tree = make_tree(7, 3, mode = 'undirected')
plot(g.tree, vertex.size=15, vertex.color="pink", edge.arrow.size=0.2, main="")

custom_closeness_centrality <- closeness_centrality(g.tree)
buildin_closeness_centrality <- centr_clo(g.tree)$res

closeness_centrality_comparison <- data.frame(
  Custom = custom_closeness_centrality,
  Built_in = buildin_closeness_centrality,
  Match = custom_closeness_centrality == buildin_closeness_centrality
)
print(closeness_centrality_comparison)

# proximity prestige
proximity_prestige(graph)

# betweenness centrality
custom_betweenness <- betweenness_centrality(undirected)
buildin_betweenness <- betweenness(undirected, normalized = T, directed = F)

betweenness_comparison <- data.frame(
  Custom = custom_betweenness,
  Built_in = buildin_betweenness,
  Match = custom_betweenness == buildin_betweenness
)
print(betweenness_comparison)

# common_neighbor_measure
common_neighbor_measure(edges, "s01", "s02")

# jaccard_measure
jaccard_measure(edges, "s01", "s02")
