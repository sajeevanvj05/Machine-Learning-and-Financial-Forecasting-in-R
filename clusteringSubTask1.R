
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load data
data <- read_excel("/Users/sajeevan/Downloads/final/vehicles.xls")

# Select only the first 18 attributes for clustering
data <- select(data, 1:18)

# Scaling the data
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
# Load necessary libraries
library(cluster)
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(data_clean, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

# Silhouette method
fviz_nbclust(data_clean, kmeans, method = "silhouette")

# Gap Statistics
set.seed(123)  # for reproducibility
gap_stat <- clusGap(data_clean, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# NBClust
nb <- NbClust(data_clean, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")


# Step 2g: K-means Clustering on PCA-transformed Data
# Perform K-means clustering using the chosen number of clusters
set.seed(123)  # for reproducibility
k <- 4  # replace with the actual number determined from previous steps
km <- kmeans(data_clean, centers = k, nstart = 25)

# Print cluster centers
print(km$centers)

# BSS and WSS calculation
total_ss <- sum((data_clean - matrix(rep(colMeans(data_clean), nrow(data_clean)), nrow = nrow(data_clean), byrow = TRUE))^2)
bss <- km$betweenss
wss <- km$tot.withinss
tss <- total_ss
bss_tss_ratio <- bss / tss

# Output BSS and WSS ratios
cat("BSS:", bss, "WSS:", wss, "TSS:", tss, "BSS/TSS Ratio:", bss_tss_ratio, "\n")

# Silhouette plot
silhouette_plot <- silhouette(km$cluster, dist(data_clean))
fviz_silhouette(silhouette_plot)

# Average silhouette width
mean_silhouette_width <- mean(silhouette_plot[, "sil_width"])
print(mean_silhouette_width)
