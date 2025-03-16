library(ggplot2)

michalska_fisher_score <- function(data){
  labels = unique(data$Label)
  features = names(data)[-ncol(data)]
  
  sum_d = 0
  sum_m = 0
  
  for (feature in features){
    mean_overall = mean(data[[feature]])
    for (label in labels){
      feature_data = data[data$Label == label, feature]
      label_mean = mean(feature_data)
      label_var = var(feature_data)
      label_N = length(feature_data)
      
      sum_d = sum_d + label_N * (label_mean - mean_overall)^2
      sum_m = sum_m + label_N * label_var
    }
    fisher_score = sum_d / sum_m
    print(feature)
    print(fisher_score)
  }
  
}

michalska_entropy <- function(f){
  f <- replace(f, f == 0, 1e-10)
  return(-sum(f * log2(f)))
}

michalska_create_datasets <- function(n=200, mean1=2, mean2=1, sd=0.5, 
                                      label="A") {
  set.seed(45)

  x1 <- rnorm(n, mean = mean1, sd = sd)
  y1 <- rnorm(n, mean = mean2, sd = sd)
  label1 <- rep(label, n)
  
  return(list(X = x1, Y = y1, Label = label1))
}

michalska_plot <- function(data, Title){
  ggplot(data, aes(x = X, y = Y, color = Label)) + 
    geom_point(size = 3) +  
    labs(title = Title, x = "X", y = "Y") +
    theme_minimal()
}

michalska_split_dataset_and_plot <- function(data, threshold, title){
  left_group <- data[data$X <= threshold, ]
  right_group <- data[data$X > threshold, ]
  
  left_prob <- table(left_group$Label) / nrow(left_group)
  right_prob <- table(right_group$Label) / nrow(right_group)
  
  left_entropy <- michalska_entropy(left_prob)
  right_entropy <- michalska_entropy(right_prob)
  
  cat('\nEntropy of the left: ', left_entropy)
  cat('\nEntropy of the right: ', right_entropy)
  
  ggplot(data, aes(x = X, y = Y, color = Label)) + 
    geom_point(size = 3) +  
    geom_vline(xintercept = threshold, 
               color = "green", size=1) +
    labs(title = title, x = "X", y = "Y") +
    theme_minimal()
}

# entropy
data = michalska_create_datasets()
data1 = michalska_create_datasets(mean1=0, mean2=1, sd=0.7, label="B")

x = c(data$X, data1$X)
y = c(data$Y, data1$Y)
label = c(data$Label, data1$Label)

data <- data.frame(X = x, Y = y, Label = label)

michalska_plot(data, "Visualization of the first dataset")

labels <- table(data$Label)
label_probabilities <- labels / sum(labels)
overall_entropy <- michalska_entropy(label_probabilities)
print(overall_entropy)

# entropy after split
michalska_split_dataset_and_plot(data, 1, "Splitted dataset, threshold=1")
michalska_split_dataset_and_plot(data, 3, "Splitted dataset, threshold=3")

# fisher score
# separable
data = michalska_create_datasets()
data2 = michalska_create_datasets(mean1=30, mean2=1, sd=2, label="B")

x = c(data$X, data2$X)
y = c(data$Y, data2$Y)
label = c(data$Label, data2$Label)

data <- data.frame(X = x, Y = y, Label = label)

michalska_plot(data, "Visualization of the separable dataset")

michalska_fisher_score(data)

# non-separable
data1_2 = michalska_create_datasets()
data2_2 = michalska_create_datasets(mean1=2.1, mean2=1, sd=1, label="B")

x_2 = c(data1_2$X, data2_2$X)
y_2 = c(data1_2$Y, data2_2$Y)
label_2 = c(data1_2$Label, data2_2$Label)

data2 <- data.frame(X = x_2, Y = y_2, Label = label_2)

michalska_plot(data2, "Visualization of the non-separable dataset")

michalska_fisher_score(data2)
