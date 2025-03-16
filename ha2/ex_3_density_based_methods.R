library(ggplot2)

euclidean_distance <- function(point1, point2) {
  sqrt(sum((point1 - point2)^2))
}

calculate_distance_matrix <- function(data) {
  n <- nrow(data)  
  dist_matrix <- matrix(0, nrow = n, ncol = n)  
  
  for (i in 1:n) {
    for (j in i:n) {
      if (i != j) {
        dist_matrix[i, j] <- euclidean_distance(data[i, ], data[j, ])
        dist_matrix[j, i] <- dist_matrix[i, j] 
      }
    }
  }
  return(dist_matrix)
}

dbscan_custom <- function(data, eps, minPts) {
  n <- nrow(data)
  cluster_labels <- rep(0, n)
  cluster_id <- 0
  
  distance_matrix <- calculate_distance_matrix(data)
  
  for (i in 1:n) {
    if (cluster_labels[i] != 0) next
    
    neighbors <- which(distance_matrix[i, ] <= eps)
    
    if (length(neighbors) < minPts) {
      cluster_labels[i] <- -1 
    } else {
      cluster_id <- cluster_id + 1
      cluster_labels[i] <- cluster_id
      
      j <- 1
      while (j <= length(neighbors)) {
        point <- neighbors[j]
        
        if (cluster_labels[point] == -1) {
          cluster_labels[point] <- cluster_id
        }
        
        if (cluster_labels[point] == 0) {
          cluster_labels[point] <- cluster_id
          
          point_neighbors <- which(distance_matrix[point, ] <= eps)
          
          if (length(point_neighbors) >= minPts) {
            neighbors <- unique(c(neighbors, point_neighbors))
          }
        }
        
        j <- j + 1
      }
    }
  }
  
  return(cluster_labels)
}


set.seed(123)
x1 <- rnorm(n = 50, mean = 10, sd = 0.5)
y1 <- rnorm(n = 50, mean = 20, sd = 1.5)

x2 <- rnorm(n = 50, mean = 10, sd = 0.5)
y2 <- rnorm(n = 50, mean = 12, sd = 0.5)

x3 <- rnorm(n = 50, mean = 20, sd = 0.5)
y3 <- rnorm(n = 50, mean = 5, sd = 1.5)

data <- data.frame(
  x = c(x1, x2, x3),
  y = c(y1, y2, y3)
)

eps1 <- 0.3
minPts1 <- 5
labels1 <- dbscan_custom(data, eps1, minPts1)

eps2 <- 2
minPts2 <- 3
labels2 <- dbscan_custom(data, eps2, minPts2)

data$cluster1 <- as.factor(labels1)
data$cluster2 <- as.factor(labels2)

plot1 <- ggplot(data, aes(x = x, y = y, color = cluster1)) +
  geom_point(size = 3) +
  theme_minimal()

plot2 <- ggplot(data, aes(x = x, y = y, color = cluster2)) +
  geom_point(size = 3) +
  theme_minimal()

print(plot1)
print(plot2)
