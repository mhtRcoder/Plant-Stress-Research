#totan sir PCA analysis
getwd()

library(FactoMineR)
library(ggplot2)
library(factoextra)
library(mclust)
library(stats) #####
library(fpc)
library(ggforce)
library(grDevices)###
library(factoextra)
library(stats)
data <- BY

data <- read.csv(file.choose(), header = TRUE)
data
str(data)
data$Gen=as.factor(data$Gen)
data$Replication=as.factor(data$Replication)
data$treatment=as.factor(data$treatment)
#data$Maturity=as.factor(data$Maturity)
attach(data)
str(data)

# Data Normalization
data.pca.SB=prcomp(data[,-c(2:25)],
                   center=TRUE,
                   scale=TRUE)
data.pca.SB
p=data.pca.SB
print(p)
summary(p)

# Showing the plot where we are showing that which are positive and negative
# Change colour of bar plot
c.pc1 <- ifelse(p$rotation[,1] > 0, yes="green2", no="red2")
c.pc2 <- ifelse(p$rotation[,2] > 0, "green2", "red2")
# Get position for variable names
n.pc1 <- ifelse(p$rotation[,1] > 0, -0.01, p$rotation[,1]-0.01)
n.pc2 <- ifelse(p$rotation[,2] > 0, -0.01, p$rotation[,2]-0.01)
# Plot
layout(matrix(1:2, ncol=1)) # Set up layout
par(mar=c(1,3,2,1), oma=c(7.5,0,0,0)) # Set up margins
# Plot PC 1
b1 <- barplot(p$rotation[,1], main="PC 1 Loadings Plot", col=c.pc1, las=2, axisnames=FALSE)
abline(h=0)
# Plot PC 2
b2 <- barplot(p$rotation[,2], main="PC 2 Loadings Plot", col=c.pc2, las=2, axisnames=FALSE)
abline(h=0)
# Add variable names
text(x=b2, y=ifelse(p$rotation[,2] > 0, -0.01, p$rotation[,2]-0.01), labels=names(p$rotation[,2]), adj=1, srt=90, xpd=NA)

## Scree plot showing 
fviz_eig(p)
# Add labels
fviz_eig(p, addlabels=TRUE, hjust = -0.3)
# Change the y axis limits
fviz_eig(p, addlabels=TRUE, hjust = -0.3)+
  ylim(0, 60)

eig_val<-get_eigenvalue(p)
eig_val
# Temporarily increase the max print limit to a higher value (e.g., 5000)
options(max.print = 5000)

# Now when you print your data or PCA results, you will see more rows and columns
print(your_data_or_pca_result)


### ScreePlot-with Showing the eigen values _Visualize
fviz_eig(p, choice = "eigenvalue", 
         addlabels=TRUE)
##Change the y axis limits_for showing the Eigenvalue in the Y-axis
fviz_eig(p, choice = "eigenvalue", 
         addlabels=TRUE)+
  ylim(0,30)
res.var<-get_pca_var(p)
res.var
## To see the coordinates for the variables running res.var$coord command
res.var$coord
### To see the contributions of the each variable to the PCs running res.var$contrib
res.var$contrib
# To see the quality of the representation of the variables running res.var$cos2
res.var$cos2
## To see the correlations between variables and dimensions running res.var$cor
res.var$cor


### Biplot Preparation using the Package: factoextra(oldold)
library(factoextra)
fviz_pca_ind(p,
             col.ind="cos2",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel=TRUE)
fviz_pca_ind(p,
             col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel=TRUE)

## Variables_PCA-with Cos2 variable values_in circle shape with the variable contribution%-cos2
fviz_pca_var(p,
             col.var="cos2",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel=TRUE)
### Variables_PCA-With the variables contribution_contrib to the each PCA components:
fviz_pca_var(p,
             col.var="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel=TRUE)
fviz_pca_biplot(p,label="var", alpha.ind=1,col.var="tomato",habillage=data$treatment,
                repel=TRUE, invisible="quali",legend.title="Treatment")+xlab("PC 1 (55.6%)")+ylab("PC 2 (18.4%)")+
  theme_bw()
#mmu sir ffiz plot code
plot <- (fviz_pca_biplot(res.pca, axes = c(1, 2),
                         col.var = "contrib", # Color by contributions to the PC
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         geom.ind = c("point"),  
                         fill.ind = CORR$Cluster, palette = c("#FF6666", "#20B2AA", "#0066CC"), col.ind = "black", 
                         pointshape = 21, pointsize = 5, addEllipses = TRUE, ellipse.level=0.95, 
                         repel = TRUE) # Avoid text overlapping))
         + coord_cartesian(xlim =c(-10, 10), ylim = c(-4, 4))
         + theme(text = element_text(size = 16),
                 axis.text.x = element_text(color="black", size=16), 
                 axis.text.y = element_text(color="black", size=16)))

print(plot, title = "", fill = "Cluster", color = "Contribution")





### If we want to change the color of variables from blue to black, then:
fviz_pca_biplot(p,label="var", alpha.ind=1,col.var="black",habillage=data$treatment,
                repel=TRUE,addEllipses=TRUE, invisible="quali",legend.title="Treatment")+
  theme_bw()

#fviz_pca_biplot(p,label="var", alpha.ind=1,col.var="blue",habillage=data$Maturity,
               # repel=TRUE,addEllipses=TRUE, invisible="quali",legend.title="Maturity stages")+
  #theme_bw()







#totan sir heatmap
install.packages("NbClust")
library(NbClust)
library(pheatmap)
library(FactoMineR)
library(ggplot2)
library(factoextra)
library(mclust)
library(stats) #####
library(fpc)
library(ggforce)
library(grDevices)###
library(stats)
data <- HM
any(is.na(HM))

str(HM)
names(BY)
data <- HM[,1:48]
View(data)
data_matrix <- data.matrix(HM[,1:48])

#data <- Heat1[,1:9]
#data_matrix <- data.matrix(Heat1[,1:9])

cal_z_score <- function(x){
(x - mean(x)) / sd(x)}

data_matrix_norm <- t(apply(data_matrix, 1, cal_z_score))

data_heatmap <- pheatmap(data_matrix, method ="ward.D2", metric = "euclidean", Rowv=FALSE, Colv=FALSE, cellheight = 20, cellwidth = 25, fontsize_row = 10, fontsize_col = 14,
                         color = colorRampPalette(c("#FF6347" , "white","#1A5276"))(50), 
                         scale="column", cutree_rows=3, margins=c(10,5), 
                         annotation_legend = TRUE,annotation_row = h_col, annotation_col = h_col1, annotation_colors = mycolors) #we need to check the number of cluster first, so we run the code before annotation then later we will run annotation when lb,hcol all other codes are defined

hc <- data_heatmap$tree_row
lbl <- cutree(hc, 3)

#we dont need this part below
hc1 <- data_heatmap$tree_col
lbl1 <- cutree(hc1, 3)


h_col <- data.frame(Cluster = ifelse(test = lbl == 1, yes = "Cluster I", 
                                     ifelse(test = lbl == 2, yes = "Cluster II",
                                            no = "Cluster III")))
#we dont need this part below
h_col1 <- data.frame(Group = ifelse(test = lbl1 == 1, yes = "Group 1", 
                                    ifelse(test = lbl1 == 2, yes = "Group 2",
                                           no = "Group 3")))
mycolors = list(Cluster = c("Cluster I" = "#00B4D8",  "Cluster II" = "#800000", "Cluster III" = "#90BE6D"),
  Group = c("Group 1" = "#8A2BE2", "Group 2" = "#FFD700", "Group 3" = "darkgreen"))

