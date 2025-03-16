library(ggplot2)
library(scatterplot3d)
library(gridExtra)

# Hopkins statistic
hopkins_statistic <- function(D, r) {
  R <- D[sample(1:nrow(D), r), ]
  
  synthetic_range <- apply(D, 2, range)
  S <- matrix(runif(r * ncol(D), min = synthetic_range[1, ], max = synthetic_range[2, ]), nrow = r)
  
  nearest_neighbor_distance <- function(points, data) {
    dists <- apply(points, 1, function(p) {
      d <- sqrt(rowSums((data - p)^2))
      min(d[d > 0])
    })
    return(dists)
  }
  
  alpha <- nearest_neighbor_distance(R, D)
  beta <- nearest_neighbor_distance(S, D) 
  
  H <- sum(beta) / (sum(alpha) + sum(beta))
  
  return(H)
}

# plot pairs of features
plot_pairs <- function(data, combo, hopkins_value) {
  ggplot(data, aes_string(x = combo[1], y = combo[2])) +
    geom_point(alpha = 0.5, color = "pink") +
    labs(
      x = combo[1],
      y = combo[2],
    ) +
    theme_minimal()
}


# best feature combination based on Hopkins statistic
best_feature_combination <- function(data, r = 50) {
  feature_names <- colnames(data)
  combinations <- combn(feature_names, 2, simplify = FALSE)
  
  best_combination <- NULL
  best_hopkins_value <- -Inf 
  result_pairs <- list() 
  
  for (combo in combinations) {
    subset_data <- data[, combo, drop = FALSE]
    hopkins_value <- hopkins_statistic(subset_data, r)
    
    result_pairs[[paste(combo, collapse = " & ")]] <- hopkins_value
    
    cat("Combo: ", combo, " with Hopkins value: ", hopkins_value, "\n")
    
    if (hopkins_value > best_hopkins_value) {
      best_hopkins_value <- hopkins_value
      best_combination <- combo
    }
  }
  
  return(list(combination = best_combination, 
              hopkins_value = best_hopkins_value, 
              result_pairs = result_pairs))
}

# good clustered data
set.seed(123)

x1 <- rnorm(n = 1500, mean = 30, sd = 0.5)
y1 <- rnorm(n = 1500, mean = 20, sd = 1.5)
z1 <- rnorm(n = 1500, mean = 20, sd = 0.5)

x2 <- rnorm(n = 1500, mean = 20, sd = 0.5)
y2 <- rnorm(n = 1500, mean = 20, sd = 0.5)
z2 <- rnorm(n = 1500, mean = 20, sd = 0.5)

data <- data.frame(
  x = c(x1, x2),
  y = c(y1, y2),
  z = c(z1, z2)
)

# raw data plot
scatterplot3d(data$x, data$y, data$z, 
              pch = 16, 
              color = c(rep("pink", 1500), rep("lightblue", 1500)), 
              main = "",
              xlab = "X-axis", 
              ylab = "Y-axis", 
              zlab = "Z-axis",
              angle = 55)

# overall Hopkins and for each pair
r <- 50 
shuffled_data <- data[sample(nrow(data)), ]

H_value <- hopkins_statistic(shuffled_data, r)
print(H_value)

# the best feature combination
best_combination_result <- best_feature_combination(data, r)
cat("Best pair: ", best_combination_result$combination, "\n")

# each pair with their Hopkins values
for (combo in names(best_combination_result$result_pairs)) {
  variables <- strsplit(combo, " & ")[[1]]
  hopkins_value <- best_combination_result$result_pairs[[combo]]
  
  plot <- plot_pairs(shuffled_data, variables, hopkins_value)
  
  print(plot)
}
