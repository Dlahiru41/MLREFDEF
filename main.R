# Load necessary libraries
library(readxl)
library(NbClust)
library(ggplot2)
library(cluster)
library(factoextra)

# Load the dataset
data <- read_excel("C://Users//dlahi//OneDrive//Desktop//Assigments//ML ref def//vehicles.xlsx")

# Exclude the class column
data_features <- data[, -ncol(data)]

# Scale the features
data_scaled <- scale(data_features)

# Identify outliers using Z-scores
z_scores <- scale(data_scaled)
outliers <- apply(z_scores, 1, function(x) any(abs(x) > 3)) # Z-score threshold of 3

# Remove outliers
data_clean <- data_scaled[!outliers, ]

# Determine optimal clusters using NBclust
nb <- NbClust(data_clean, min.nc=2, max.nc=10, method="kmeans")
optimal_clusters_nbclust <- nb$Best.nc[1]

# Elbow Method
wss <- sapply(1:10, function(k) {
  kmeans(data_clean, centers=k, nstart=20)$tot.withinss
})
ggplot(data.frame(k=1:10, wss=wss), aes(x=k, y=wss)) +
  geom_line() + geom_point() +
  labs(title="Elbow Method", x="Number of Clusters", y="WSS")

# Gap Statistics
set.seed(123)
gap_stat <- clusGap(data_clean, FUN=kmeans, K.max=10, B=50)
fviz_gap_stat(gap_stat)
optimal_clusters_gap <- maxSE(gap_stat$Tab[,"gap"], gap_stat$Tab[,"SE.sim"])

# Silhouette Method
sil_width <- sapply(2:10, function(k) {
  pam(data_clean, k)$silinfo$avg.width
})
optimal_clusters_silhouette <- which.max(sil_width)

# Choose the most favored k (for demonstration, use NBclust result)
optimal_k <- optimal_clusters_nbclust

# Perform k-means clustering with the optimal number of clusters
set.seed(123)
kmeans_result <- kmeans(data_clean, centers=optimal_k, nstart=20)

# Cluster centers
centers <- kmeans_result$centers

# Cluster assignments
clusters <- kmeans_result$cluster

# Calculate BSS and TSS
tot_ss <- sum((data_clean - rowMeans(data_clean))^2)
bss <- sum(kmeans_result$betweenss)
wss <- sum(kmeans_result$withinss)

# Ratio of BSS to TSS
bss_tss_ratio <- bss / tot_ss

# Print results
print(paste("Optimal number of clusters (NBclust):", optimal_clusters_nbclust))
print(paste("Optimal number of clusters (Elbow Method):", which.min(diff(diff(wss)))))
print(paste("Optimal number of clusters (Gap Statistics):", optimal_clusters_gap))
print(paste("Optimal number of clusters (Silhouette Method):", optimal_clusters_silhouette))
print(paste("Chosen optimal number of clusters:", optimal_k))
print(paste("BSS/TSS ratio:", round(bss_tss_ratio, 4)))

# Silhouette plot
sil <- silhouette(clusters, dist(data_clean))
plot(sil, col=1:optimal_k, main="Silhouette Plot")
average_silhouette_width <- mean(sil[, 3])
print(paste("Average silhouette width:", round(average_silhouette_width, 4)))

# Visualization of clusters
fviz_cluster(kmeans_result, data = data_clean)
