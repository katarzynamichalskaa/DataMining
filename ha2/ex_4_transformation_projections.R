library(caret)
library(ggplot2)
library(rgl)
library(e1071)

set.seed(122)

generate_halfmoon <- function(n) {
  n_class <- n / 2
  
  t1 <- seq(0, pi, length.out = n_class) 
  x1 <- cos(t1) + rnorm(n_class, mean=1, sd = 0.1)  
  y1 <- sin(t1) + rnorm(n_class, mean=-0.5, sd = 0.1)  
  
  t2 <- seq(pi, 2*pi, length.out = n_class)  
  x2 <- cos(t2) + rnorm(n_class, sd = 0.1) 
  y2 <- sin(t2) + rnorm(n_class, sd = 0.1)  
  
  X <- rbind(cbind(x1, y1), cbind(x2, y2))
  y <- c(rep(1, n_class), rep(2, n_class)) 
  
  data <- data.frame(x = X[, 1], y = X[, 2], class = as.factor(y))
  return(data)
}

train_logistic_regression_2D <- function(data_2d, train_index, train_data, 
                                         test_data) {
  
  model <- train(class ~ x + y, data = train_data, method = "glm", 
                 family = "binomial")
  
  predictions <- predict(model, newdata = test_data)
  
  accuracy <- mean(predictions == test_data$class)
  
  conf_matrix <- confusionMatrix(predictions, test_data$class)
  
  print(paste("2D accuracy:", round(accuracy, 3)))
  print(conf_matrix)
  
  ggplot(data_2d, aes(x = x, y = y, color = class)) +
    geom_point(alpha = 0.6) +
    stat_function(
      fun = function(x) {
        coef <- coef(model$finalModel)
        (-coef[1] - coef[2] * x) / coef[3]
      },
      color = "blue",
      size = 1
    ) +
    scale_color_manual(values = c("red", "green")) +
    theme_minimal()
}

transform_to_3d <- function(data) {
  data$z <- rep(NA, nrow(data)) 
  data$z[data$class == 1] <- 0.5 
  data$z[data$class == 2] <- 1 
  return(data)
}

train_logistic_regression_3D <- function(data_3d, train_data, test_data) {
  model_3d <- glm(class ~ x + y + z, data = train_data, family = "binomial")
  
  predictions_3d <- predict(model_3d, newdata = test_data_3d, type = "response")
  predicted_classes_3d <- ifelse(predictions_3d > 0.5, 2, 1)
  accuracy_3d <- mean(predicted_classes_3d == test_data_3d$class)
  conf_matrix_3d <- confusionMatrix(factor(predicted_classes_3d), test_data_3d$class)
  
  print(paste("3D accuracy:", round(accuracy_3d, 3)))
  print(conf_matrix_3d)
  
  coef_3d <- coef(model_3d)
  
  x_seq <- seq(min(data_3d$x), max(data_3d$x), length.out = 50)
  y_seq <- seq(min(data_3d$y), max(data_3d$y), length.out = 50)
  grid <- expand.grid(x = x_seq, y = y_seq)
  
  grid$z <- (-coef_3d[1] - coef_3d[2] * grid$x - coef_3d[3] * grid$y) / coef_3d[4]
  
  z_matrix <- matrix(grid$z, nrow = length(x_seq), ncol = length(y_seq), byrow = TRUE)
  
  colors <- c("red", "green")
  col_values <- colors[as.numeric(data_3d$class)]
  plot3d(data_3d$x, data_3d$y, data_3d$z, col = col_values, type = "s", size = 1, 
         xlab = "X", ylab = "Y", zlab = "Z")
  surface3d(unique(grid$x), unique(grid$y), matrix(grid$z, nrow = length(x_seq), 
                                                   ncol = length(y_seq)), 
            color = "blue", alpha = 0.5)
}

n <- 500  
data_2d <- generate_halfmoon(n)

train_index <- createDataPartition(data_2d$class, p = 0.8, list = FALSE)
train_data <- data_2d[train_index, ]
test_data <- data_2d[-train_index, ]

# training logistic regression in 2D 
train_logistic_regression_2D(data_2d, train_index, train_data, 
                             test_data)

# transformation function (with perfect separability)
data_3d <- transform_to_3d(data_2d)

train_data_3d <- transform_to_3d(train_data)
test_data_3d <- transform_to_3d(test_data)

# training logistic regression in 3D 
train_logistic_regression_3D(data_3d, train_data_3d, test_data_3d)