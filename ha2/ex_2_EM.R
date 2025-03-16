library(scatterplot3d)
library(animation)

set.seed(42)

plot_data <- function(data, colors=c(rep("red", 1500), rep("blue", 1500),
                                     rep("green", 1500))) {
  scatterplot3d(data$x, data$y, data$z, 
                pch = 16, 
                color = colors, 
                main = "",
                xlab = "X-axis", 
                ylab = "Y-axis", 
                zlab = "Z-axis",
                angle = 55)
}

plot_data_after_clustering <- function(data) {
  scatterplot3d(data$x, data$y, data$z, 
                pch = 16, 
                color = data$cluster_id, 
                main = "",
                xlab = "X-axis", 
                ylab = "Y-axis", 
                zlab = "Z-axis",
                angle = 55)
}
  
k_means_custom <- function(data, k, max_iter = 100, tol = 1e-6) {
  centroids <- data[sample(1:nrow(data), k), ]
  clusters_history <- list()
  
  for (i in 1:max_iter) {
    dist_matrix <- as.matrix(dist(rbind(centroids, data)))
    dist_matrix <- dist_matrix[1:k, (k + 1):(k + nrow(data))]
    
    cluster_ids <- apply(dist_matrix, 2, which.min)
    clusters_history[[i]] <- data.frame(data, cluster_id = cluster_ids)
    
    old_centroids <- centroids
    centroids <- do.call(rbind, lapply(1:k, function(j) {
      colMeans(data[cluster_ids == j, , drop = FALSE])
    }))
    
    if (sum((centroids - old_centroids)^2) < tol) {
      break
    }
  }
  result <- data.frame(data, cluster_id = cluster_ids)
  return(list(clusters_history = clusters_history, result = result))
}

sum.finite <- function(x) {
  sum(x[is.finite(x)])
}

dmvnorm_custom <- function(x, mean, sigma) {
  d <- ncol(x)
  inv_sigma <- solve(sigma)
  det_sigma <- det(sigma)
  norm_const <- 1 / ((2 * pi)^(d / 2) * sqrt(det_sigma))
  
  diff <- as.matrix(sweep(x, 2, mean))  
  exp_term <- exp(-0.5 * rowSums(diff %*% inv_sigma * diff))
  
  return(norm_const * exp_term)
}


em_custom <- function(data, k = 2, tol = 1e-6, max_iter = 100) {
  n <- nrow(data)
  d <- ncol(data)
  
  pi_k <- rep(1 / k, k)
  mu_k <- lapply(1:k, function(i) colMeans(data[((i - 1) * n / k + 1):(i * n / k), ]))
  sigma_k <- lapply(1:k, function(i) cov(data[((i - 1) * n / k + 1):(i * n / k), ]))
  
  Q <- numeric()
  Q[1] <- 0
  iter_count <- 2
  
  clusters_history <- list()
  
  for (iter in 1:max_iter) {
    # e-step
    responsibilities <- matrix(0, n, k)
    for (j in 1:k) {
      responsibilities[, j] <- pi_k[j] * dmvnorm_custom(data, mean = mu_k[[j]], 
                                                        sigma = sigma_k[[j]])
    }
    
    responsibilities_sum <- rowSums(responsibilities)
    responsibilities <- sweep(responsibilities, 1, responsibilities_sum, "/")
    
    clusters <- apply(responsibilities, 1, which.max)
    clusters_history[[iter]] <- data.frame(data, cluster_id = clusters)
    
    # m-step
    for (j in 1:k) {
      weight_sum <- sum(responsibilities[, j])
      pi_k[j] <- weight_sum / n
      mu_k[[j]] <- colSums(responsibilities[, j] * as.matrix(data)) / weight_sum
      sigma_k[[j]] <- cov.wt(data, wt = responsibilities[, j], center = mu_k[[j]])$cov
    }
    
    Q[iter_count] <- sum.finite(log(responsibilities_sum))
    
    if (abs(Q[iter_count] - Q[iter_count - 1]) < tol) {
      break
    }
    
    iter_count <- iter_count + 1
  }
  
  result <- data.frame(data, cluster_id = clusters)
  return(list(clusters_history = clusters_history, result = result))
}

show_gif <- function(clusters_history) {
  saveGIF({
    for (step in seq_along(clusters_history)) {
      unique_clusters <- unique(clusters_history[[step]]$cluster_id)
      colors <- rainbow(length(unique_clusters))[as.factor(clusters_history[[step]]$cluster_id)]
      scatterplot3d(clusters_history[[step]]$x, clusters_history[[step]]$y, clusters_history[[step]]$z,
                    color = colors,
                    pch = 16, angle = 55,
                    xlab = "X-axis", ylab = "Y-axis", zlab = "Z-axis",
                    main = paste("K-Means Iteration:", step))
    }
  }, movie.name = "kmeans_process.gif", interval = 0.5, ani.width = 600, ani.height = 600)
  
}

# well-separated data set
x1 <- rnorm(n = 1500, mean = 20, sd = 0.5)
y1 <- rnorm(n = 1500, mean = 20, sd = 1.5)
z1 <- rnorm(n = 1500, mean = 20, sd = 0.5)

x2 <- rnorm(n = 1500, mean = 30, sd = 0.5)
y2 <- rnorm(n = 1500, mean = 20, sd = 0.5)
z2 <- rnorm(n = 1500, mean = 20, sd = 0.5)

x3 <- rnorm(n = 1500, mean = 30, sd = 0.5)
y3 <- rnorm(n = 1500, mean = 20, sd = 1.5)
z3 <- rnorm(n = 1500, mean = 30, sd = 0.5)

data1 <- data.frame(
  x = c(x1, x2, x3),
  y = c(y1, y2, y3),
  z = c(z1, z2, z3)
)

plot_data(data1)

# for k = 3

result1_knn = k_means_custom(data1, 3)
result1_em <- em_custom(data1, 3)
plot_data_after_clustering(result1_knn$result)
plot_data_after_clustering(result1_em$result)
# show_gif(result1_knn$clusters_history)
# show_gif(result1_em$clusters_history)

# for k = 5

result1_knn = k_means_custom(data1, 5)
result1_em <- em_custom(data1, 5)
plot_data_after_clustering(result1_knn$result)
plot_data_after_clustering(result1_em$result)
# show_gif(result1_knn$clusters_history)
# show_gif(result1_em$clusters_history)

# more complicated data set
x1 <- rnorm(n = 1500, mean = 20, sd = 2.5)
y1 <- rnorm(n = 1500, mean = 20, sd = 1.5)
z1 <- rnorm(n = 1500, mean = 19, sd = 1.5)

x2 <- rnorm(n = 1500, mean = 20, sd = 1.5)
y2 <- rnorm(n = 1500, mean = 22, sd = 2.5)
z2 <- rnorm(n = 1500, mean = 20, sd = 2.5)

x3 <- rnorm(n = 1500, mean = 19, sd = 2.5)
y3 <- rnorm(n = 1500, mean = 20, sd = 1.5)
z3 <- rnorm(n = 1500, mean = 20, sd = 1.5)

data2 <- data.frame(
  x = c(x1, x2, x3),
  y = c(y1, y2, y3),
  z = c(z1, z2, z3)
)

plot_data(data2)

# for k = 3

result1_knn = k_means_custom(data2, 3)
result1_em <- em_custom(data2, 3)
plot_data_after_clustering(result1_knn$result)
plot_data_after_clustering(result1_em$result)
# show_gif(result1_knn$clusters_history)
# show_gif(result1_em$clusters_history)

# for k = 5

result1_knn = k_means_custom(data2, 5)
result1_em <- em_custom(data2, 5)
plot_data_after_clustering(result1_knn$result)
plot_data_after_clustering(result1_em$result)
# show_gif(result1_knn$clusters_history)
# show_gif(result1_em$clusters_history)