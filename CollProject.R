d <- Stable_isotope_evidence_for_pre_colonial_dataset

# unsupervised clustering using K means and hclusts
library(tidyverse)
library(readxl)
library(cluster)
install.packages("factoextra")
library(factoextra)

cluster_d <- d |>
  select(where(is.numeric)) |>
  drop_na()

d_scaled <- scale(cluster_d)

fviz_nbclust(d_scaled, kmeans, method = "wss")
fviz_nbclust(d_scaled, kmeans, method = "silhouette")

set.seed(123)
kmeans_result <- kmeans(d_scaled, centers = 3, nstart = 25)
kmeans_result
kmeans_result$cluster

fviz_cluster(kmeans_result, data = d_scaled)

data_kmeans <- cluster_d |>
  mutate(kmeans_cluster = as.factor(kmeans_result$cluster))

dist_matrix <- dist(d_scaled)

hclust_result <- hclust(dist_matrix, method = "ward.D2")

plot(hclust_result, labels = FALSE, main = "Hierarchical Clustering Dendrogram")
rect.hclust(hclust_result, k = 3, border = 2:4)

hclust_clusters <- cutree(hclust_result, k = 3)

data_hclust <- cluster_d |>
  mutate(hclust_cluster = as.factor(hclust_clusters))

table(
  Kmeans = data_kmeans$kmeans_cluster,
  Hclust = data_hclust$hclust_cluster
)

data_kmeans |>
  group_by(kmeans_cluster) |>
  summarise(across(everything(), mean, na.rm = TRUE))

data_hclust |>
  group_by(hclust_cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))











