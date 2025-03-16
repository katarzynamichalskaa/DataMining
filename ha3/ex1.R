library(tm)
library(cluster)
library(ggplot2)
library(caTools)
library(e1071)  
library(Rtsne)

set.seed(123)
setwd(getwd())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

read_data <- function(file_name) {
  articles <- read.csv(file_name, header = TRUE, as.is = TRUE)
  articles <- articles[, c("Category", "Article.text")]
  articles <- articles[!articles$Category %in% c("entertainment", "health"), ]
  return(articles)
}

preprocess_text <- function(articles) {
  corpus <- VCorpus(VectorSource(articles$Article.text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

classify_and_plot <- function(dtm, articles) {
  # reduction
  tsne <- Rtsne(as.matrix(dtm), perplexity = 30)
  tsne_data <- data.frame(
    Dim1 = tsne$Y[, 1],
    Dim2 = tsne$Y[, 2],
    Category = as.factor(articles$Category)
  )
  
  # svm
  set.seed(123)
  split <- sample.split(tsne_data$Category, SplitRatio = 0.7)
  train_data <- tsne_data[split, c("Dim1", "Dim2")]
  test_data <- tsne_data[!split, c("Dim1", "Dim2")]
  train_labels <- tsne_data$Category[split]
  test_labels <- tsne_data$Category[!split]
  
  svm_model <- svm(train_data, train_labels, kernel = "linear")
  predictions <- predict(svm_model, test_data)
  print(table(Predicted = predictions, Actual = test_labels))
  
  x_min <- min(tsne_data$Dim1) - 1
  x_max <- max(tsne_data$Dim1) + 1
  y_min <- min(tsne_data$Dim2) - 1
  y_max <- max(tsne_data$Dim2) + 1
  
  grid <- expand.grid(
    Dim1 = seq(x_min, x_max, length.out = 200),
    Dim2 = seq(y_min, y_max, length.out = 200)
  )
  grid_predictions <- predict(svm_model, grid)
  grid$Category <- grid_predictions
  
  # plot decision boundary and points
  g1 <- ggplot() +
    geom_tile(data = grid, aes(x = Dim1, y = Dim2, fill = Category), alpha = 0.3) +
    geom_point(data = tsne_data, aes(x = Dim1, y = Dim2, color = Category), size = 2) +
    labs(title = "", x = "Dimension 1", y = "Dimension 2") +
    theme_minimal() +
    theme(legend.position = "right")
  print(g1)
  
  # centroids
  centroids <- aggregate(cbind(Dim1, Dim2) ~ Category, data = tsne_data, FUN = mean)
  
  # plot centroids
  g2 <- ggplot(tsne_data, aes(x = Dim1, y = Dim2, color = Category)) +
    geom_point(alpha = 0.2) +
    geom_point(data = centroids, aes(x = Dim1, y = Dim2, color = Category), size = 4, shape = 4, stroke = 2) +
    labs(title = "", x = "Dimension 1", y = "Dimension 2") +
    theme_minimal() +
    theme(legend.position = "right")
  print(g2)
}

articles <- read_data("./CNN_Articels_clean.csv")
corpus <- preprocess_text(articles[1:450, ])
dtm <- DocumentTermMatrix(corpus)

classify_and_plot(dtm, articles[1:450, ])