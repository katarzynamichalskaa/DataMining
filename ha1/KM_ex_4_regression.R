library(plotly)

michalska_backward_elimination <- function(sufficient_value=0.05){
  variables = michalska_create_dataset()
  
  x1 = variables$x1
  x2 = variables$x2
  x3 = variables$x3
  x4 = variables$x4
  x5 = variables$x5
  x6 = variables$x6
  y = variables$y
  
  data <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6, 
                     y = y)
  
  model <- lm(y ~ ., data = data)
  summary_model <- summary(model)
  print(summary_model)
  p_values <- summary_model$coefficients[, "Pr(>|t|)"]

  while (TRUE) {
    max_p_value <- max(p_values) 
    
    if (max_p_value < sufficient_value) {
      break
    }
    print(max_p_value)
    variable_to_remove <- names(which.max(p_values[-1])) 
    data <- subset(data, select = -which(names(data) == variable_to_remove))
    model <- lm(y ~ ., data = data)
    
    summary_model <- summary(model)
    p_values <- summary_model$coefficients[, "Pr(>|t|)"] 
    
  }
  
  return(list(model = model, data = data))
}

michalska_create_dataset <- function(points=1000, a1=0, a2=0.7, a3=0.001, 
                                     a4=0.3, a5=0, a6=0.002, b = 1){
  set.seed(123)
  
  x1 <- rnorm(points, sd=0.4)
  x2 <- rnorm(points, sd=0.2)
  x3 <- rnorm(points, sd=0.5)
  x4 <- rnorm(points, sd=0.4)
  x5 <- rnorm(points, sd=0.2)
  x6 <- rnorm(points, sd=0.1)
  
  y <- a1 * x1 + a2 * x2 + a3 * x3 + a4 * x4 + a5 * x5 + a6 * x6 + 
    b + rnorm(points, sd = 0.05)
  
  return(list(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6, y = y))
}

plotlinear_3d <- function(x2, x4, model, data, plot_title = "Linear Model Plot") {
  x2_seq <- seq(min(x2), max(x2), length.out = 30)
  x4_seq <- seq(min(x4), max(x4), length.out = 30)
  
  grid <- expand.grid(x2 = x2_seq, x4 = x4_seq)
  
  grid$y_pred <- predict(model, newdata = grid)
  
  plot_ly() %>%
    add_markers(data = data, x = ~x2, y = ~x4, z = ~y, 
                marker = list(color = 'red', size = 4),
                name = "Data") %>%
    add_surface(x = matrix(x2_seq, nrow = 30, ncol = 30),
                y = matrix(x4_seq, nrow = 30, ncol = 30, byrow = TRUE),
                z = matrix(grid$y_pred, nrow = 30, ncol = 30),
                opacity = 0.5, colorscale = list(c(0, "yellow"), c(1, "green")),
                showscale = FALSE, name = "Regression Plane") %>%
    layout(scene = list(
      xaxis = list(title = "x2"),
      yaxis = list(title = "x4"),
      zaxis = list(title = "y"),
      title = list(text = plot_title, font = list(size = 20)) 
    ))
}

info = michalska_backward_elimination()
summary(info$model)

cor_matrix <- cor(info$data[, c("x2", "x4", "y")])
print(cor_matrix)

plotlinear_3d(info$data$x2, info$data$x4, info$model, info$data)

