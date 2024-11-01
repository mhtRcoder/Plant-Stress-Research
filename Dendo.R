
install.packages("igraph")
getwd()

# Dendogram
ddata <- read.csv(file.choose(), header = TRUE)
ddata
str(ddata)

# Changing Row names (generally row names are denoted by 1,2.... I want to denote the row as vegetable name so that when I will delete the 1st row of Veg name still I can see the vegetable )
rownames(ddata)<- c(ddata$x)
head(ddata)
rownames(ddata)<- c(ddata$Vegetables)
head(ddata)

newdata <- ddata[-1]
newdata
head(newdata)


df<-newdata
df.scaled <- scale(df)

res.dist <- dist(x = df.scaled, 
                 method = "euclidean")
print(res.dist)

x <- as.matrix(res.dist)[1:5, 1:5]
x
round(x, digits = 3)

require(stats)
res.hc <- hclust(d = res.dist,
                 method = "complete")
plot(x = res.hc)



### Coloring
require(factoextra)
library(factoextra)
fviz_dend(x = res.hc, cex = 0.7, lwd = 0.7)
fviz_dend(x = res.hc, cex = 0.4, lwd = 0.8)
require(grDevices)
library(grDevices)
colors()

require(scales)
library(scales)
palette()
show_col(palette(rainbow(6)))
require("ggsci")
library(ggsci)
show_col(pal_jco(palette = c("default"))(10))
show_col(pal_jco("default", alpha = 0.6)(10))
# fviz_dend = Use fviz function for enhanced visualization of dendrogram
# x = an object of class dendrogram, hclust, agnes, diana, hcut, hkmeans or HCPC (FactoMineR).
# k = the number of groups for cutting the tree.
# cex = size of labels
# k_colors = a vector containing colors to be used for the groups.It should contains k number of colors. Allowed values include also "grey" for grey color palettes; brewer palettes e.g. "RdBu", "Blues", ...; and scientific journal palettes from ggsci R package, e.g.: "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty"
fviz_dend(x = res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          k_colors = c("red", "green3", "blue","magenta"))


fviz_dend(x = res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          k_colors = "jco")
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          rect = TRUE, 
          rect_border = "gray",
          rect_fill = FALSE)
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          rect = TRUE, 
          rect_border = "gray",
          rect_fill = TRUE)
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          rect = TRUE, 
          rect_border = "jco", 
          rect_fill = TRUE)
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE)
fviz_dend(res.hc, cex = 0.8, k=3, 
          rect = TRUE,  
          k_colors = "jco",
          rect_border = "jco", 
          rect_fill = TRUE, 
          horiz = TRUE)
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          ggtheme = theme_void())
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "phylogenic")
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE)
require(igraph)
library(igraph)
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_as_tree")
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 5, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          ggtheme = theme_void())
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 5, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE)
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 5, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_as_tree")
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 5, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "circular",
          repel = TRUE)
#Circular dendrogram with 3 cluster (k=3)
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 3, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "circular",
          repel = TRUE)
