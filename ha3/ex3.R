library(ggplot2)

set.seed(123) 
n_points <- 3000
n_periods <- 10
points_per_period <- n_points / n_periods

# data with concept drift
data_stream <- data.frame(
  index = 1:n_points,
  time_period = rep(1:n_periods, each = points_per_period),
  value = unlist(lapply(1:n_periods, function(p) rnorm(points_per_period, mean = p * 5, sd = 1)))
)

# reservoir sampling function
reservoir_update <- function(reservoir, new_point, k, i, adjust_for_drift = FALSE) {
  if (nrow(reservoir) < k) {
    reservoir <- rbind(reservoir, new_point)
    cat(sprintf("Added point %d to reservoir (size: %d).\n", new_point$index, nrow(reservoir)))
  } else {
    if (adjust_for_drift) {
      probability <- exp(-0.6 * (i - new_point$index))
    } else {
      probability <- k / i
    }
    
    j <- sample(1:k, 1)
    
    if (runif(1) <= probability) {
      replaced <- reservoir[j, ]
      reservoir[j, ] <- new_point
      cat(sprintf("Replaced point %d with point %d.\n", replaced$index, new_point$index))
    } else {
      cat(sprintf("Point %d skipped (reservoir unchanged).\n", new_point$index))
    }
  }
  
  return(reservoir)
}



# simulate streaming data to reservoir
simulate_streaming <- function(data_stream, k, adjust_for_drift = FALSE) {
  reservoir <- data.frame()
  
  for (i in seq_len(nrow(data_stream))) {
    current_point <- data_stream[i, , drop = FALSE]  
    reservoir <- reservoir_update(reservoir, current_point, k, i, adjust_for_drift)
  }
  
  return(reservoir)
}

# visualization
plot_reservoir <- function(reservoir) {
  ggplot(reservoir, aes(x = index, y = value, color = as.factor(time_period))) +
    geom_point(alpha = 0.5) + 
    labs(title = " ", color = "Time Period") +  
    theme_minimal()  
}

plot_distribution <- function(reservoir) {
  p2 <- ggplot(reservoir, aes(x = value, fill = as.factor(time_period))) +
    geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
    labs(title = " ", fill = "Time Period") +
    theme_minimal()
  
  return(p2)
}

# run scenarios
k_large <- n_points
k_small <- 300

# scenario 1: reservoir size equals entire dataset
result_large <- simulate_streaming(data_stream, k_large)

# scenario 2: small reservoir without concept drift adjustment
result_small_no_adjust <- simulate_streaming(data_stream, k_small)

# scenario 3: small reservoir with concept drift adjustment
result_small_with_adjust <- simulate_streaming(data_stream, k_small, adjust_for_drift = TRUE)

# plot results
plot1 <- plot_reservoir(result_large)
plot2 <- plot_reservoir(result_small_no_adjust)
plot3 <- plot_reservoir(result_small_with_adjust)

print(plot1)
print(plot2)
print(plot3)

# plot histograms
plot1 <- plot_distribution(result_large)
plot2 <- plot_distribution(result_small_no_adjust)
plot3 <- plot_distribution(result_small_with_adjust)

print(plot1)
print(plot2)
print(plot3)
