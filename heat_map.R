

###########################

data <- scale(mean[, 1:29])
data <- as.matrix(data)
library(pheatmap)

# Create the heatmap with row clusters
pheatmap(data, 
         clustering_method = "ward.D2", 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         cellheight = 10, 
         cellwidth = 20, 
         fontsize_row = 8, 
         fontsize_col = 10, 
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50), 
         scale = "column", 
         cutree_rows = 3, 
         margins = c(10, 5), 
         annotation_legend = TRUE)
# Define row and column clusters
row_annotation <- data.frame(
  Cluster = factor(cutree(hclust(dist(data), method = "ward.D2"), k = 3))
)
rownames(row_annotation) <- rownames(data)

col_annotation <- data.frame(
  Group = factor(cutree(hclust(dist(t(data)), method = "ward.D2"), k = 2))
)
rownames(col_annotation) <- colnames(data)

# Define annotation colors
mycolors = list(
  Cluster = c("1" = "tomato", "2" = "light sea green", "3" = "royal blue"), 
  Group = c("1" = "#9ACD32", "2" = "#F4A460")
)

# Final cluster heatmap with annotations

png("Heatmap.png", width = 2400, height = 1200, res =300)
pheatmap(data, 
         clustering_method = "ward.D2", 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         cellheight = 20, 
         cellwidth = 13, 
         fontsize_row = 8, 
         fontsize_col = 10, 
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50), 
         scale = "column", 
         cutree_rows = 3, 
         margins = c(10, 5), 
         annotation_legend = TRUE, 
         annotation_row = row_annotation, 
         annotation_col = col_annotation, 
         annotation_colors = mycolors)
dev.off()


data_transposed <- t(data)

png("T1_Heatmap.png", width = 1500, height = 1700, res =300)
pheatmap(data_transposed, 
         clustering_method = "ward.D2", 
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         cellheight = 10, 
         cellwidth = 20, 
         fontsize_row = 8, 
         fontsize_col = 10, 
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50), 
         scale = "column", 
         cutree_rows = 3, 
         margins = c(10, 5), 
         annotation_legend = TRUE, 
         annotation_row = col_annotation,  # Swap annotations
         annotation_col = row_annotation,  # Swap annotations
         annotation_colors = mycolors)
dev.off()
 

##########Corrrplot

data <- mean[, -1]

library(corrplot)

# Assuming 'mean' is your dataset
cor_matrix <- cor(data, use = "complete.obs")

png("corrplot1.png", units = "in", width = 10, height = 9, res = 300)

# Plot the lower half of the correlogram
corrplot(cor_matrix, 
         method = "circle", 
         type = "lower", 
         tl.cex = 0.6,  # Reduce the font size of the text labels
         tl.srt = 35)
dev.off()
################PCA analysis

pca <- prcomp(iris[, -5], 
              scale = TRUE)

biplot(pca, 
       scale = 0)

biplot(pca,
       col = c('darkblue', 'red'),
       scale = 0, xlabs = rep("*", 150))

fviz_pca_biplot(pca)
fviz_pca_biplot(pca,
                label="var",
                habillage = iris$Species)
fviz_pca_biplot(pca,
                label = "var",
                col.ind = "cos2",
                col.var = "black",
                gradient.cols = c("blue","green","red"))
fviz_pca_biplot(pca,
                label = "var",
                col.ind = "black",
                col.var = "contrib",
                gradient.cols = c("blue","green","red"))


