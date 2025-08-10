library(ISLR)
library(caret)
install.packages("ROSE")
library(ROSE)
install.packages("caTools")
library(caTools)
install.packages("pROC")
library(pROC)
set.seed(300)
install.packages("caret")  # If not installed
library(caret)  # For confusion matrix
# Load necessary libraries
install.packages("ggplot2")
install.packages("ggforce")  # For drawing circles
library(ggplot2)
library(ggforce)




load(file="NNS.RData")
load(file="Annotation.RData")

z_score <- z_score_cluster[,-17]

# Filter rows where label > 0 and get dimensions
positive_data <- z_score[z_score$label > 0, ]  # Subset the data
negative_data <- z_score[z_score$label == 0, ]  # Subset the data

# run k-mean for positive first
df_n <- negative_data[, -c(17,18)]
kmeans_result_n <- kmeans(df_n, centers = 2, nstart = 10)
df_n_cluster2 <- df_n[kmeans_result_n$cluster == 2, ]

df_n_cluster1 <- df_n[kmeans_result_n$cluster == 1, ]

df_cluster2 <- df[kmeans_result$cluster == 2, ]
df_cluster1 <- df[kmeans_result$cluster == 1, ]

df_n$Cluster <- as.factor(kmeans_result_n$cluster)
df_n$label <- negative_data$label

# Perform k-means clustering with 3 clusters
df <- positive_data[, -c(17, 18)]
kmeans_result <- kmeans(df, centers = 2, nstart = 10)
# get index of a dataframe
# Add cluster assignments to the dataframe
df$Cluster <- as.factor(kmeans_result$cluster)
df$label <- positive_data$label

ne_cen <- data.frame(kmeans_result_n$centers)
ne_cen$label <- c(0,2)
po_cen <- data.frame(kmeans_result$centers)
po_cen$label <- c(1,1)
total <- rbind(po_cen, ne_cen) # centroid for kmean

index_1 <- rownames(df_cluster1) # category 1
index_2 <- rownames(df_cluster2) # category 2
index_m <- rownames(df_n_cluster2) # category 3
index_n <- c(rownames(df_n_cluster1)) # category 4

z_score$category = rep("Ready", dim(z_score)[1])
z_score[index_2,]$category = rep("Ready", length(index_2))
z_score[index_m,]$category = rep("Borderline", length(index_m))
z_score[index_n,]$category = rep("NotReady", length(index_n))
z_score$category <- as.factor(z_score$category)
# assign new labels to each sample, 1 is positive, 0 is negative, 2 is in between
# Load necessary libraries


kmeans_with_centroids(z_score[, -17], total[, -17])
df <- z_score[, -17]
centroids <- total[, -17]
# Define the K-Means Function
kmeans_with_centroids <- function(df, centroids) {
  kmeans_result <- kmeans(df[, -ncol(df)], centers = centroids, nstart = 1, iter.max = 100)
  df$Cluster <- as.factor(kmeans_result$cluster)
}
kmeans_cluster_plot(df[, -17], as.matrix(total[,-17]))

# visualize in pca
umap_result <- umap(df[, -c(17,18)], n_neighbors = 3)

# Convert to DataFrame
umap_df <- as.data.frame(umap_result)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$Cluster <- as.factor(df$Cluster)

# Plot UMAP-based Clustering
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Cluster)) +
  geom_point(size = 0.5) +
  labs(title = "K-Means Clustering (UMAP Projection)") +
  theme_minimal()



# Run the function
kmeans_cluster_plot(df_example, centroids_example)



# kmean for all the samples, given 4 centroid


# sample.split to create an index 
split = caTools::sample.split(data$category, 
                              SplitRatio = 0.9) # 0.75

train = subset(data, 
               split == TRUE)
test = subset(data, 
              split == FALSE)
# resample positive as the same number as negative
over <- ROSE::ovun.sample(category~., data = train, method = "over", 
                          N = dim(train[train$category==0,])[[1]] * 2)$data
over$category <- factor(over$category)
levels(over$category) <- make.names(levels(over$category))


ctrl <- trainControl(method="repeatedcv",
                     number=2, 
                     repeats = 3,
                     classProbs=TRUE,
                     summaryFunction = twoClassSummary)
knnFit <- caret::train(category ~ ., data = over, method = "knn", trControl = ctrl, tuneLength = 20)
knnFit 
plot(knnFit, print.thres = 0.5, type="S")

test$category <- factor(test$category)
levels(test$category) <- make.names(levels(test$category))
knnPredict <- predict(knnFit,newdata = test )
#Get the confusion matrix to see accuracy value and other parameter values

confusionMatrix(knnPredict, test$category, positive = "X1")

install.packages("randomForest")
library(randomForest)

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)

# Random forrest
rfFit <- train(category ~ ., data = over, method = "rf", trControl = ctrl, tuneLength = 20)

rfPredict <- predict(rfFit,newdata = test )
confusionMatrix(rfPredict, test$category )

#With twoclasssummary
ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)
# Random forrest
rfFit <- train(category ~ ., data = over, method = "rf", trControl = ctrl, tuneLength = 20)

rfFit
plot(rfFit)

rfFit$bestTune
plot(rfFit, print.thres = 0.5, type="S")

rfPredict <- predict(rfFit,newdata = test )
confusionMatrix(rfPredict, test$category )

rfPredict <- predict(rfFit,newdata = test , type="prob")
rfROC <- roc(test$category,rfPredict[,"X0"], levels = c("X0", "X1"))
rfROC
plot(rfROC, type="S", print.thres= 0.5)

