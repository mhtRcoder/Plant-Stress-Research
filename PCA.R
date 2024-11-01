library(FactoMineR)
library(ggplot2)
library(mclust)
library(stats)
library(fpc)
library(ggforce)
library(grDevices)
library(factoextra)
library(stats)
getwd()
df1 <- read.csv(choose.files()) 
df1
str(df1)
df1$Replication <- as.factor(df1$Replication)
df1$Genotype <- as.factor(df1$Genotype)
df1$Maturity <- as.factor(df1$Maturity)
attach(df1)

head(df1)
#Now Doing PCA analysis on df1
data.pca.df1 <- prcomp(df1[,-c(1:3)],
                       center = TRUE,
                       scale = TRUE)
data.pca.df1
p=data.pca.df1
summary(p)

#change the color of Bar plot
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
   fviz_eig(p, addlabels=TRUE, hjust = -0.3) +
     ylim(0, 45)
 eig_val<-get_eigenvalue(p)
eig_val
res.var <- get_pca_var(p)
## To see the coordinates for the variables running res.var$coord command
res.var$coord

### To see the contributions of the each variable to the PCs running res.var$contrib
 res.var$contrib
 # To see the quality of the representation of the variables running res.var$cos2
 res.var$cos2
 ## To see the correlations between variables and dimensions running res.var$cor
  res.var$cor
### Biplot Preparation using the Package: factoextra
  library(factoextra)
fviz_pca_ind(p,col.ind="cos2",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
               repel =   TRUE)
fviz_pca_ind(p,  col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
                      repel=TRUE)
## Variables_PCA-with Cos2 variable values_in circle shape with the variable contribution%-cos2

fviz_pca_var(p,col.var="cos2",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
              repel=TRUE)
### Variables_PCA-With the variables contribution_contrib to the each PCA components:
 fviz_pca_var(p,            col.var="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
                           repel=TRUE)
fviz_pca_biplot(p,label="var", alpha.ind=1,col.var="blue",habillage=df1$Genotype,
                                   repel=TRUE,addEllipses=TRUE, invisible="quali",legend.title="Genotypes")+
     theme_bw()
### If we want to change the color of variables from blue to black, then:
 fviz_pca_biplot(p,label="var", alpha.ind=1,col.var="black",habillage=df1$Genotype,
                                  repel=TRUE,addEllipses=TRUE, invisible="quali",legend.title="Genotypes")+
     theme_bw() 
 