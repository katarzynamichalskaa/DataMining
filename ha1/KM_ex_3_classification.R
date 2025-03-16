library(ggplot2)
library(plotly)
library(pROC)

michalska_gini <- function(data) {
  labels <- table(data$Label)
  prob <- labels / sum(labels)
  return(1 - sum(prob^2))
}

michalska_split <- function(data, feature, threshold) {
  left <- data[data[[feature]] < threshold, ]
  right <- data[data[[feature]] >= threshold, ]
  return(list(left = left, right = right))
}

michalska_best_split <- function(data) {
  best_gini <- Inf
  best_split <- NULL
  features <- names(data)[1:3]  
  
  for (feature in features) {
    thresholds <- unique(data[[feature]])
    for (threshold in thresholds) {
      split_data <- michalska_split(data, feature, threshold)
      gini <- (nrow(split_data$left) / nrow(data)) * michalska_gini(split_data$left) +
        (nrow(split_data$right) / nrow(data)) * michalska_gini(split_data$right)
      if (gini < best_gini) {
        best_gini <- gini
        best_split <- list(feature = feature, threshold = threshold, left = split_data$left, right = split_data$right)
      }
    }
  }
  
  return(best_split)
}

michalska_decision_tree <- function(data, depth = 1, max_depth = 4) {
  if (nrow(data) == 0 || depth >= max_depth || length(unique(data$Label)) == 1) {
    return(as.character(names(sort(table(data$Label), decreasing = TRUE)[1])))
  }
  
  split <- michalska_best_split(data)
  
  if (is.null(split)) {
    return(as.character(names(sort(table(data$Label), decreasing = TRUE)[1])))
  }
  
  left_branch <- michalska_decision_tree(split$left, depth + 1, max_depth)
  right_branch <- michalska_decision_tree(split$right, depth + 1, max_depth)
  
  return(list(feature = split$feature, threshold = split$threshold, left = left_branch, right = right_branch))
}

michalska_predict <- function(tree, new_data) {
  if (!is.list(tree)) {
    return(tree)
  }
  
  feature <- tree$feature
  threshold <- tree$threshold
  
  if (new_data[[feature]] < threshold) {
    return(michalska_predict(tree$left, new_data))
  } else {
    return(michalska_predict(tree$right, new_data))
  }
}


michalska_goodness_metrics <- function(actual, predicted) {
  tp <- sum(actual == 1 & predicted == 1)
  tn <- sum(actual == 0 & predicted == 0)
  fp <- sum(actual == 0 & predicted == 1)
  fn <- sum(actual == 1 & predicted == 0)
  
  accuracy <- (tp + tn) / length(actual)
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  f1_score <- ifelse((precision + recall) > 0, 2 * precision * recall / (precision + recall), 0)
  
  return(list(accuracy = accuracy, precision = precision, recall = recall, 
              f1_score = f1_score))
}

michalska_cross_validation <- function(data, k = 5) {
  set.seed(100)  
  
  data <- data[sample(nrow(data)), ]
  
  accuracy_list <- c()
  precision_list <- c()
  recall_list <- c()
  f1_score_list <- c()
  auc_list <- c()
  
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
  
  for (i in 1:k) {
    test_indices <- which(folds == i, arr.ind = TRUE)
    test_data <- data[test_indices, ]
    train_data <- data[-test_indices, ]
    
    # training process
    tree <- michalska_decision_tree(train_data)
    print(tree)
    
    # testing process 
    # metrics
    test_data$Predicted_Label <- sapply(1:nrow(test_data), function(j) michalska_predict(tree, test_data[j, ]))
    test_data$Actual_Binary <- ifelse(test_data$Label == "A", 1, 0)
    test_data$Predicted_Binary <- ifelse(test_data$Predicted_Label == "A", 1, 0)
    
    metrics <- michalska_goodness_metrics(test_data$Actual_Binary, test_data$Predicted_Binary)
    
    accuracy_list <- c(accuracy_list, metrics$accuracy)
    precision_list <- c(precision_list, metrics$precision)
    recall_list <- c(recall_list, metrics$recall)
    f1_score_list <- c(f1_score_list, metrics$f1_score)
    
    # ROC AUC
    colors <- c("blue", "red", "green", 'yellow', 'orange')  
    labels <- c("k = 1", "k = 2", "k = 3", "k = 4", "k = 5")
    
    if(i == 1) {
      roc_curve <- roc(test_data$Actual_Binary, as.numeric(test_data$Predicted_Binary),
                       plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       show.thres=TRUE, main="ROC Curves",
                       col=colors[i])  
    } else {
      roc_curve <- roc(test_data$Actual_Binary, as.numeric(test_data$Predicted_Binary),
                       plot=TRUE, add=TRUE, col=colors[i])  
    }
    auc_list <- c(auc_list, auc(roc_curve))

  }
  legend("bottomright", legend=labels, col=colors, lwd=2)
  
  average_metrics <- list(
    accuracy = accuracy_list,
    precision = precision_list,
    recall = recall_list,
    f1_score = f1_score_list,
    auc = auc_list
  )
  
  return(average_metrics)
}

michalska_create_datasets <- function(n=200, mean1=2, mean2=1, mean3=2, sd=0.5, 
                                      label="A") {
  set.seed(123)
  
  x1 <- rnorm(n, mean = mean1, sd = sd)
  y1 <- rnorm(n, mean = mean2, sd = sd)
  z1 <- rnorm(n, mean = mean3, sd = sd)
  label1 <- rep(label, n)
  
  return(list(X = x1, Y = y1, Z = z1, Label = label1))
}

michalska_plot <- function(data, Title){
  plot_ly(data, x = ~X, y = ~Y, z = ~Z, color = ~Label, colors = c('red', 'green', 'blue'), 
          type = 'scatter3d', mode = 'markers', marker = list(size = 3)) %>%
    layout(title = Title, scene = list(xaxis = list(title = "X"), 
                                       yaxis = list(title = "Y"), 
                                       zaxis = list(title = "Z")))
}

michalska_train_test_split <- function(data, train_size = 0.7) {
  set.seed(123)  
  
  total_rows <- nrow(data)
  train_indices <- sample(1:total_rows, size = floor(train_size * total_rows))
  
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  return(list(train = train_data, test = test_data))
}

dataA <- michalska_create_datasets()
dataB <- michalska_create_datasets(mean1 = 3.7, label = "B")

x <- c(dataA$X, dataB$X)
y <- c(dataA$Y, dataB$Y)
z <- c(dataA$Z, dataB$Z)
label <- c(dataA$Label, dataB$Label)

data <- data.frame(X = x, Y = y, Z = z, Label = label)

michalska_plot(data, "Visualization of the dataset")
print(michalska_cross_validation(data))
