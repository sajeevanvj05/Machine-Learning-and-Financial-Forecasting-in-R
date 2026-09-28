# Load necessary libraries
library(readxl)
library(dplyr)
library(factoextra)
library(cluster)
library(NbClust)
library(fpc)

# Function to remove outliers using IQR method
remove_outliers <- function(Data2) {
  Q1 <- apply(Data2, 2, quantile, probs = 0.25)
  Q3 <- apply(Data2, 2, quantile, probs = 0.75)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  clean_Data2 <- Data2
  for (i in 1:ncol(Data2)) {
    clean_Data2 <- clean_Data2[clean_Data2[, i] >= lower_bound[i] & clean_Data2[, i] <= upper_bound[i], ]
  }
  return(clean_Data2)
}

# Function to perform PCA and K-means clustering
perform_pca_and_clustering <- function(Data2_file) {
  # Read Data2
  Data2 <- read_excel(Data2_file)
  
  # Select relevant attributes and scale the Data2
  selected_Data2 <- select(Data2, 2:19)
  scaled_Data2 <- scale(selected_Data2)
  
  # Remove outliers from the scaled Data
  cleaned_Data2 <- remove_outliers(scaled_Data2)
  
  # Perform Principal Component Analysis (PCA)
  pca <- prcomp(cleaned_Data2, scale. = TRUE)
  pca_summary <- summary(pca)
  
  # Print PCA summary
  print(pca_summary)
  
  # Cumulative variance explained by Principal Components (PCs)
  cumulative_variance <- cumsum(pca_summary$importance[2, ])
  
  # Select PCA with cumulative variance > 92%
  num_components <- which(cumulative_variance > 0.92)[1]
  pca_Data2 <- data.frame(pca$x[, 1:num_components])  # Corrected function name
  
  # Print selected PCA and cumulative variance
  print(paste("Number of PCA selected:", num_components))
  print(paste("Cumulative variance explained:", cumulative_variance[num_components]))
  
  # Determine the number of clusters on PCA-transformed Data using NbClust
  nb_pca <- NbClust(pca_Data2, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
  
  # Perform K-means clustering on PCA-transformed Data
  k_pca <- nb_pca$Best.nc[1]  # Best number of clusters according to NbClust
  km_pca <- kmeans(pca_Data2, centers = k_pca, nstart = 25)
  
  # Calculate BSS (Between Sum of Squares) and WSS (Within Sum of Squares) for PCA-transformed Data2
  tss_pca <- sum((pca_Data2 - matrix(rep(colMeans(pca_Data2), nrow(pca_Data2)), nrow = nrow(pca_Data2), byrow = TRUE))^2)
  bss_pca <- sum(km_pca$betweenss)
  wss_pca <- km_pca$tot.withinss
  bss_tss_recio_pca <- bss_pca / tss_pca
  
  # Calculate Silhouette width for PCA-transformed Data
  silhouette_plot_pca <- silhouette(km_pca$cluster, dist(pca_Data2))
  mean_silhouette_width_pca <- mean(silhouette_plot_pca[, "sil_width"])
  
  # Calculate Calinski-Harabasz index for PCA-transformed Data
  ch_index <- cluster.stats(dist(pca_Data2), km_pca$cluster)$ch
  
  # Elbow method
  elbow_plot <- fviz_nbclust(pca_Data2, kmeans, method = "wss") +
    geom_vline(xintercept = k_pca, linetype = 2) +
    labs(title = "Elbow Method")
  
  # Gap statistic
  gap_stat <- clusGap(pca_Data2, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
  gap_plot <- fviz_gap_stat(gap_stat) +
    labs(title = "Gap Statistic Method")
  
  # Silhouette method
  silhouette_plot <- fviz_nbclust(pca_Data2, kmeans, method = "silhouette") +
    labs(title = "Silhouette Method")
  
  # Return results including PCA model and plots
  list(
    num_clusters = k_pca,
    bss = bss_pca,
    wss = wss_pca,
    bss_tss_ratio = bss_tss_recio_pca,
    mean_silhouette_width = mean_silhouette_width_pca,
    calinski_harabasz_index = ch_index,
    pca_model = pca,
    elbow_plot = elbow_plot,
    gap_plot = gap_plot,
    silhouette_plot = silhouette_plot
  )
}

# Perform PCA and clustering
result <- perform_pca_and_clustering(Data2_file)

# Print clustering metrics
cat("Number of clusters:", result$num_clusters, "\n")
cat("BSS :", result$bss, "\n")
cat("WSS :", result$wss, "\n")
cat("BSS/TSS Recio :", result$bss_tss_ratio, "\n")
cat("Mean Silhouette Width :", result$mean_silhouette_width, "\n")
cat("Calinski-Harabasz Index :", result$calinski_harabasz_index, "\n")

# Access the PCA model
pca_model <- result$pca_model

# Show plots
print(result$elbow_plot)
print(result$gap_plot)
print(result$silhouette_plot)
