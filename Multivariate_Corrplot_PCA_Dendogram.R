library(ggplot2)
library(ggpubr)
library(corrplot)
library(agricolae)
library(xts)
library(dplyr)
library(PerformanceAnalytics)
library(zoo)
library(Formula)
library(tidyverse)

getwd()
df <- read.csv(choose.files())
df
str(df)
head(df)
#to chcek the dependent variables then we need set the location of dependent variables
df$Replication <- as.factor(df$Replication)
df$Genotype <- as.factor(df$Genotype)
df$Time <- as.factor(df$Time)
#selecting only the nu erical columns
numeric_vers <- df%>%
  select(-Genotype,-Replication,-Time)%>%
  mutate_all(as.numeric)
numeric_vers

#compute the correlation matrix
cor_matrix <- cor(numeric_vers, use = "complete.obs", method = "pearson")
print(cor_matrix)

plot_cor_matrix <- corrplot(cor_matrix, method = "square", type="upper",diag = FALSE, tl.col = "tomato", tl.srt = 45, addCoef.col = "black", nmber.cex= 0.7, 
                            title = "Correlation Matrix of numerical variables", mar = c(0,0,1,0))
plot_cor_matrix
#sacling the data set
scale_numeric_vers <- scale_numeric_vers
#without Title
corrplot(cor_matrix, method="color", type = "upper", diag = FALSE, tl.col = "black", tl.srt = 45, addCoef.col = "black", nmber.cex= 0.7, 
         mar = c(0,0,1,0))                      