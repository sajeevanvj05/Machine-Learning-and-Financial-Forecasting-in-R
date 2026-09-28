# Load necessary libraries
library(readxl)
library(dplyr)
library(factoextra)
library(cluster)
library(NbClust)
library(ggplot2)

# Load and prepare data
Data1 <- read_excel("vehicles.xlsx")

# Select the required columns
selected_Data1 <- dplyr::select(Data1, 2:19)

# Scale the selected data
scaled_Data1 <- scale(selected_Data1)

# Function to remove outliers
Remove_outliers <- function(Data1) {
  Q1 <- apply(Data1, 2, quantile, probs = 0.25)
  Q3 <- apply(Data1, 2, quantile, probs = 0.75)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  clean_Data1 <- Data1
  for (i in 1:ncol(Data1)) {
    clean_Data1 <- clean_Data1[clean_Data1[,i] >= lower_bound[i] & clean_Data1[,i] <= upper_bound[i],]
  }
  return(clean_Data1)
}

# Remove outliers from the scaled data
cleaned_Data1 <- Remove_outliers(scaled_Data1)

# Elbow method plot
Elbow_plot <- fviz_nbclust(cleaned_Data1, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
print(Elbow_plot)

# NbClust results
Nb_results <- NbClust(cleaned_Data1, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
print(Nb_results)

# Silhouette method plot
Silhouette_plot <- fviz_nbclust(cleaned_Data1, kmeans, method = "silhouette")
print(Silhouette_plot)

# Set seed for reproducibility
set.seed(123)

# Determine number of clusters
num_clusters <- 3
kmeans_result <- kmeans(cleaned_Data1, centers = num_clusters, nstart = 50, iter.max = 100)

# Calculate total sum of squares
tss <- sum((cleaned_Data1 - colMeans(cleaned_Data1))^2)

# Calculate between-cluster sum of squares
bss <- sum(kmeans_result$size * apply(kmeans_result$centers, 1, function(center) sum((center - colMeans(cleaned_Data1))^2)))

# Calculate within-cluster sum of squares
wss <- kmeans_result$tot.withinwss

# Calculate BSS/TSS ratio
bss_tss_recio <- bss / tss

# Silhouette plot for k-means result
silhouette_result <- silhouette(kmeans_result$cluster, dist(cleaned_Data1))
mean_silhouette_width <- mean(silhouette_result[, "sil_width"])

# Function to perform Gap Statistic analysis
perform_gap_statistic <- function(Data1) {
  gap_stat <- clusGap(Data1, FUN = kmeans, nstart = 50, iter.max = 100, K.max = 10, B = 50)
  print(plot(gap_stat, main = "Gap Statistic"))
  
  optimal_k <- which.max(gap_stat$Tab[, "gap"])
  cat("Optimal number of clusters in Gap Statistic: ", optimal_k, "\n")
  
  return(optimal_k)
}

# Perform Gap Statistic analysis
optimal_k_gap <- perform_gap_statistic(cleaned_Data1)

# Function to perform Silhouette Method analysis
perform_silhouette_method <- function(Data1) {
  Silhouette_Score <- numeric(10)
  for (k in 2:10) {
    kmeans_result <- kmeans(Data1, centers = k, nstart = 50, iter.max = 100)
    Silhouette_Score[k] <- mean(silhouette(kmeans_result$cluster, dist(Data1))[, "sil_width"])
  }
  plot(2:10, Silhouette_Score[2:10], type = "b", xlab = "Number of Clusters", ylab = "Silhouette Score", main = "Silhouette Method")
  
  optimal_k <- which.max(Silhouette_Score)
  cat("Optimal number of clusters in Silhouette Method:", optimal_k, "\n")
  
  return(optimal_k)
}

# Perform Silhouette Method analysis
optimal_k_silhouette <- perform_silhouette_method(cleaned_Data1)

# Print results
cat("WSS :", wss, "\n")
cat("TSS :", tss, "\n")
cat("BSS :", bss, "\n")
cat("BSS/TSS Recio :", bss_tss_recio, "\n")
cat("Mean Silhouette Width :", mean_silhouette_width, "\n")
