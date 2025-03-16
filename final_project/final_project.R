library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(dbscan)
library(caret)
library(arules)
library(arulesViz)
library(rpart)
library(lubridate)
library(zoo)
library(arules)

set.seed(123)
setwd(getwd())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

read_data <- function(){
  data <- read.csv("on.csv", header = TRUE, as.is = TRUE)
  print(data)
  return(data)
}

handle_nan <- function(data){
  data$UnitPrice <- as.numeric(data$UnitPrice)
  data$Quantity <- as.numeric(data$Quantity)
  
  data$UnitPrice[is.na(data$UnitPrice)] <- mean(data$UnitPrice, na.rm = TRUE)
  data$Quantity[is.na(data$Quantity)] <- mean(data$Quantity, na.rm = TRUE)
  
  data$InvoiceDate <- zoo::na.approx(as.Date(data$InvoiceDate, 
                                             format = "%Y-%m-%d"), 
                                     na.rm = FALSE)
  
  data <- data %>% filter(!is.na(CustomerID))
  
  print(data)
  return(data)
}

analyse_customers <- function(data){
  data <- as.matrix(data)
  plot <- fviz_nbclust(data, kmeans, method = "silhouette")
  print(plot)
}

select_customers_data <- function(data){
  customer_data <- data %>% 
    group_by(CustomerID) %>% 
    summarize(
      TotalSpent = sum(UnitPrice * Quantity, na.rm = TRUE),
      NumTransactions = n(),
      AvgBasketValue = mean(UnitPrice * Quantity, na.rm = TRUE)
    ) %>% 
    na.omit()
  return(customer_data)
}

detect_outliers <- function(data){
  customer_data <- select_customers_data(data)
  customer_scaled <- scale(customer_data[, -1]) 
  
  customer_scaled_df <- as.data.frame(customer_scaled)
  
  dbscan_result <- dbscan(customer_scaled_df, eps = 0.9, minPts = 5)
  print(dbscan_result)
  
  customer_data$Anomaly <- ifelse(dbscan_result$cluster == 0, "Yes", "No")
  
  customer_data_no_outliers <- customer_data %>%
    filter(Anomaly == "No")
  
  customer_data_no_outliers %>%
    summarize(across(c(TotalSpent, NumTransactions, AvgBasketValue), 
                     list(mean = mean, sd = sd)))
  
  plot(
    customer_scaled_df[customer_data_no_outliers$Anomaly == "No", 1], 
    customer_scaled_df[customer_data_no_outliers$Anomaly == "No", 2], 
    col = ifelse(dbscan_result$cluster[customer_data_no_outliers$Anomaly == "No"] == 0, 1, 2), 
    pch = 19, 
    main = " ", 
    xlab = "Feature 1", 
    ylab = "Feature 2"
  )
  
  legend(
    "topright", 
    legend = c("Outlier", "Not Outlier"), 
    col = c(1, 2), 
    pch = 19
  )
  
  customer_scaled_df <- as.data.frame(scale(customer_data_no_outliers %>%
                                              select(TotalSpent, 
                                                     NumTransactions, 
                                                     AvgBasketValue)))  
  
  return(customer_scaled_df)
}


cluster_customers <- function(customers){
  kmeans_result <- kmeans(customers, centers = 3, nstart = 25)
  customers$cluster <- kmeans_result$cluster
  graph <- fviz_cluster(kmeans_result, data = customers)
  sil <- silhouette(kmeans_result$cluster, dist(customers))
  silhouette_graph <- fviz_silhouette(sil)
  print(graph)
  print(silhouette_graph)
  return(customers)
}

decision_tree <- function(customers) {
  data_split <- split(customers, customers$cluster)
  
  split_class <- function(data) {
    train_index <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
    list(
      train = data[train_index, ],
      test = data[-train_index, ]
    )
  }
  
  split_data <- lapply(data_split, split_class)
  
  train_data <- do.call(rbind, lapply(split_data, function(x) x$train))
  test_data <- do.call(rbind, lapply(split_data, function(x) x$test))
  
  model <- rpart(cluster ~ TotalSpent + NumTransactions + AvgBasketValue, 
                 data = train_data, method = "class")
  
  predictions <- predict(model, test_data, type = "class")
  
  cm <- confusionMatrix(as.factor(predictions), as.factor(test_data$cluster))
  
  print(cm)
  
  rpart.plot::rpart.plot(model)
}

association_rules <- function(data) {
  basket_data <- data %>%
    select(InvoiceNo, StockCode, Description) %>%
    group_by(InvoiceNo) %>%
    summarize(Items = paste(Description, collapse = ","))
  
  transaction_list <- strsplit(basket_data$Items, ",")
  
  transactions <- as(transaction_list, "transactions")
  
  rules <- apriori(transactions, parameter = list(supp = 0.009, conf = 0.7))
  rule_metrics <- as(rules, "data.frame")
  antecedent_support <- interestMeasure(rules, "support", transactions, type = "lhs")
  consequent_support <- interestMeasure(rules, "support", transactions, type = "rhs")
  rule_support <- interestMeasure(rules, "support", transactions)
  
  complement_support <- 1 - rule_support
  
  collective_strength <- (rule_support * complement_support) / 
    (antecedent_support * consequent_support)
  
  rule_metrics$CollectiveStrength <- collective_strength
  
  print(rule_metrics)
}

data <- read_data()
data <- handle_nan(data)
customers <- detect_outliers(data)
analyse_customers(customers)
customers <- cluster_customers(customers)
decision_tree(customers)
association_rules(data)
