library(ggplot2)

michalska_euclidean <- function(p, b){
  return(sqrt(sum((p - b)^2)))
}

michalska_manhattan <- function(p, b){
  return(sum(abs(p-b)))
}

michalska_chebyshev <- function(p, b){
  return(max(abs(p-b)))
}

michalska_minkowsky <- function(a, b, p){
  return(sum(abs(a - b)^p)^(1/p))
}

michalska_test <- function(){
  test_set <- list(c(1, 1), c(0, 0),
                   c(1, 0), c(1, 0),
                   c(0, 1), c(0, 0),
                   c(2, 1), c(3, 0),
                   c(1, 1), c(1, 1),
                   c(1, 2), c(1, 0))
  
  for (i in seq(1, length(test_set), by=2)){  
    
    p <- test_set[[i]]
    b <- test_set[[i+1]]
      
    print(michalska_euclidean(p, b))
    print(michalska_minkowsky(p, b, 2)) # should be equal euclidean

    print(michalska_manhattan(p, b))
    print(michalska_minkowsky(p, b, 1)) # should be equal manhattan

    print(michalska_chebyshev(p, b))
    print(michalska_minkowsky(p, b, 100)) # should be equal chebyshev
  }
}

michalska_plot <- function(point1=c(-0.5, 1.5), point2=c(0, 1.5)){
  points_data <- data.frame(
    x = point1,
    y = point2,
    label = c("Point 1", "Point 2"),
    color = c("blue", "yellow")
  )
  
  lines_data <- data.frame(
    x_start = c(point1[1], point1[2], point1[1], point1[1], point1[1]),
    y_start = c(point2[1], point2[1], point2[1], point2[2], point2[1]),
    x_end = c(point1[2], point1[2], point1[1], point1[2], point1[2]),
    y_end = c(point2[2], point2[2], point2[2], point2[2], point2[1]),
    line_type = c("Euclidean", "Chebyshev Vertical", "Manhattan Vertical", 
                  "Manhattan Horizontal", "Chebyshev Horizontal"),
    color = c("green", "orange", "red", "red", "orange"),
    linetype = c("solid", "solid", "solid", "solid", "dotted")
  )
  
  ggplot() +
    geom_point(data = points_data, aes(x = x, y = y, color = color), 
               size = 5) +
    
    geom_segment(data = lines_data, aes(x = x_start, y = y_start, xend = x_end, 
                                        yend = y_end, color = color, 
                                        linetype = linetype), 
                 size = 1.5) +
    
    annotate("text", x = 0.5, y = 0.4, label = "Euclidean, P = 2", 
             color = "darkgreen", size = 4) +
    annotate("text", x = 1.1, y = -0.1, label = "Chebyshev, P -> ∞", 
             color = "darkorange", size = 4) +
    annotate("text", x = 0.5, y = 1.4, label = "Manhattan, P = 1", 
             color = "darkred", size = 4) +
    
    labs(title = "Distance Functions and parameter P 
         \nin the Minkovski Function", x = "X", y = "Y") +
    
    scale_color_identity() +
    scale_linetype_identity() +
    theme_minimal() +
    xlim(-0.5, 1.5) +
    ylim(-0.5, 1.5)
}

michalska_test()
michalska_plot()
