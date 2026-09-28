
# Step 1: Data Scaling and Outlier Detection
library(readxl)
library(dplyr)

# Load data
data <- read_excel("Whitewine_v6.xlsx")

# Select only the first 11 attributes for clustering
data <- select(data, 1:11)

# Scaling
data_scaled <- scale(data)

# Outlier detection using IQR
Q1 <- apply(data_scaled, 2, quantile, probs = 0.25)
Q3 <- apply(data_scaled, 2, quantile, probs = 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Removing outliers
data_clean <- data_scaled
for(i in 1:ncol(data_scaled)) {
  data_clean <- data_clean[data_clean[,i] >= lower_bound[i] & data_clean[,i] <= upper_bound[i],]
}

# Now data_clean is ready for clustering

# Step 2: Determine Number of Cluster Centres

library(factoextra)
library(cluster)
library(NbClust)



# Step 2e: PCA Analysis
library(stats)

# Perform PCA
pca <- prcomp(data_clean, scale. = TRUE)
pca_summary <- summary(pca)

# Print eigenvalues and eigenvectors
eigenvalues <- pca$sdev^2
eigenvectors <- pca$rotation

# Cumulative variance explained by PCs
cumulative_variance <- cumsum(pca_summary$importance[2,])

# Selecting PCs with cumulative variance > 85%
num_components <- which(cumulative_variance > 0.85)[1]
pca_data <- data.frame(pca$x[, 1:num_components])

# Output PCA results
print(eigenvalues)
print(eigenvectors)
print(pca_summary)

# Step 2f: Determine Number of Clusters on PCA-transformed Data
# NBclust for PCA-transformed data
nb_pca <- NbClust(pca_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")

# Step 2g: K-means Clustering on PCA-transformed Data
# Assuming the best number of clusters (k_pca) is determined from NBclust
k_pca <- 3  # replace with the actual number determined from previous step
km_pca <- kmeans(pca_data, centers = k_pca, nstart = 25)

# BSS and WSS calculation for PCA-transformed data
total_ss_pca <- sum((pca_data - matrix(rep(colMeans(pca_data), nrow(pca_data)), nrow = nrow(pca_data), byrow = TRUE))^2)
bss_pca <- sum(km_pca$cluster %>% as.factor() %>% levels() %>% sapply(function(cl){
  sum((pca_data[km_pca$cluster == cl,] - km_pca$centers[cl,])^2)
}))
wss_pca <- km_pca$tot.withinss
tss_pca <- total_ss_pca
bss_tss_ratio_pca <- bss_pca / tss_pca

# Output BSS and WSS ratios for PCA-transformed data
cat("BSS (PCA):", bss_pca, "WSS (PCA):", wss_pca, "TSS (PCA):", tss_pca, "BSS/TSS Ratio (PCA):", bss_tss_ratio_pca, "\n")

# Step 2h: Silhouette Plot and Evaluation for PCA-transformed Data
silhouette_plot_pca <- silhouette(km_pca$cluster, dist(pca_data))
plot(silhouette_plot_pca)
mean_silhouette_width_pca <- mean(silhouette_plot_pca[, "sil_width"])
print(mean_silhouette_width_pca)

# Step 2i: Calinski-Harabasz Index for PCA-transformed Data
library(cluster)
calinski_harabasz_index_pca <- calinhara(pca_data, km_pca$cluster)
print(calinski_harabasz_index_pca)
