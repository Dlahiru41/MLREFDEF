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

# Principal Component Analysis (PCA)
pca_result <- prcomp(data_clean, center = TRUE, scale. = TRUE)
summary_pca <- summary(pca_result)

# Print PCA summary
print(summary_pca)

# Eigenvalues and Eigenvectors
eigenvalues <- pca_result$sdev^2
eigenvectors <- pca_result$rotation
print("Eigenvalues:")
print(eigenvalues)
print("Eigenvectors:")
print(eigenvectors)

# Cumulative proportion of variance
cumulative_variance <- cumsum(eigenvalues) / sum(eigenvalues)
print("Cumulative Variance:")
print(cumulative_variance)

# Choose PCs that explain at least 92% of the variance
num_pcs <- which(cumulative_variance >= 0.92)[1]
print(paste("Number of PCs to retain (92% variance):", num_pcs))

# Create a new dataset with selected PCs
data_pca <- as.data.frame(pca_result$x[, 1:num_pcs])

# Check dimensions
print(dim(data_pca))
if (nrow(data_pca) == 0 || ncol(data_pca) == 0) {
  stop("The PCA-transformed dataset is empty or has incorrect dimensions.")
}

# Determine optimal clusters using k-means on PCA-transformed data

# Elbow Method
wss_pca <- sapply(1:10, function(k) {
  kmeans(data_pca, centers=k, nstart=20)$tot.withinss
})
if (length(wss_pca) > 1) {
  elbow_plot_pca <- ggplot(data.frame(k=1:10, wss=wss_pca), aes(x=k, y=wss)) +
    geom_line() + geom_point() +
    labs(title="Elbow Method (PCA Data)", x="Number of Clusters", y="WSS")
  print(elbow_plot_pca)
} else {
  print("Elbow Method: No sufficient data to plot.")
}

# Gap Statistics
set.seed(123)
gap_stat_pca <- clusGap(data_pca, FUN=kmeans, K.max=10, B=50)
if (nrow(gap_stat_pca$Tab) > 0) {
  gap_plot_pca <- ggplot(data.frame(K=1:nrow(gap_stat_pca$Tab), Gap=gap_stat_pca$Tab[, "gap"], SE=gap_stat_pca$Tab[, "SE.sim"]), aes(x=K)) +
    geom_line(aes(y=Gap), color="blue") +
    geom_point(aes(y=Gap), color="blue") +
    geom_errorbar(aes(ymin=Gap-SE, ymax=Gap+SE), width=0.2, color="blue") +
    labs(title="Gap Statistic (PCA Data)", x="Number of Clusters", y="Gap Statistic") +
    theme_minimal()
  print(gap_plot_pca)
  optimal_clusters_gap_pca <- which.max(gap_stat_pca$Tab[, "gap"])
} else {
  print("Gap Statistics: No sufficient data to plot.")
}

# Silhouette Method
sil_width_pca <- sapply(2:10, function(k) {
  if (nrow(data_pca) >= k) {
    pam_result <- pam(data_pca, k)
    sil_width <- pam_result$silinfo$avg.width
    return(sil_width)
  } else {
    return(NA)
  }
})
if (all(is.na(sil_width_pca))) {
  optimal_clusters_silhouette_pca <- NA
  print("Silhouette Method: No sufficient data to compute.")
} else {
  optimal_clusters_silhouette_pca <- which.max(sil_width_pca)
}

# Choose the most favored k (for demonstration, use silhouette method result if available)
optimal_k_pca <- if (!is.na(optimal_clusters_silhouette_pca)) optimal_clusters_silhouette_pca else optimal_clusters_gap_pca

# Perform k-means clustering with the optimal number of clusters on PCA-transformed data
if (!is.na(optimal_k_pca) && optimal_k_pca > 0) {
  set.seed(123)
  kmeans_result_pca <- kmeans(data_pca, centers=optimal_k_pca, nstart=20)

  # Cluster centers
  centers_pca <- kmeans_result_pca$centers

  # Cluster assignments
  clusters_pca <- kmeans_result_pca$cluster

  # Calculate BSS and TSS
  tot_ss_pca <- sum((data_pca - rowMeans(data_pca))^2)
  bss_pca <- sum(kmeans_result_pca$betweenss)
  wss_pca <- sum(kmeans_result_pca$withinss)

  # Ratio of BSS to TSS
  bss_tss_ratio_pca <- bss_pca / tot_ss_pca

  # Print results
  # print(paste("Optimal number of clusters (Elbow Method):", optimal_k_pca))
  # print(paste("Optimal number of clusters (Gap Statistics):", optimal_clusters_gap_pca))
  # print(paste("Optimal number of clusters (Silhouette Method):", optimal_clusters_silhouette_pca))
  # print(paste("Chosen optimal number of clusters:", optimal_k_pca))
  # print(paste("BSS/TSS ratio:", round(bss_tss_ratio_pca, 4)))

  # Silhouette plot
  if (length(clusters_pca) > 1) {
    dist_pca <- dist(data_pca)
    sil_pca <- silhouette(clusters_pca, dist_pca)
    if (nrow(sil_pca) > 0) {
      plot(sil_pca, col=1:optimal_k_pca, main="Silhouette Plot (PCA Data)")
      average_silhouette_width_pca <- mean(sil_pca[, 3])
      print(paste("Average silhouette width (PCA Data):", round(average_silhouette_width_pca, 4)))
    } else {
      print("Silhouette Plot: No sufficient data to plot.")
    }
  } else {
    print("Silhouette Plot: Not enough clusters.")
  }

  # Visualization of clusters
  fviz_cluster(kmeans_result_pca, data = data_pca)

  # Calinski-Harabasz Index
  calinski_harabasz_index <- cluster::clusGap(data_pca, FUN = kmeans, K.max = 10, B = 50)
  print(calinski_harabasz_index$Tab)
} else {
  print("No valid optimal number of clusters for k-means.")
}
