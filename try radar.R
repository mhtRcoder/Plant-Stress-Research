library(fmsb)
library(reshape2)
set.seed(99)
data <- RAD[,2:11]
radar <- rbind(rep(0,10) , rep(-40,10) , RAD[,2:11])
rownames(RAD) <- c("Control (All Clusters)", "Cluster 1 (Drought)", "Cluster 2 (Drought)", "Cluster 3 (Drought)")
colours <- c("#6e7f8fff", "#3f68e1ff", "#00aa88ff", "#ff6145ff")
radarchart(radar, seg = 4, axistype=1 , plwd=2, pcol=colours, caxislabels=seq(-40,0,10),
           plty=1, axislabcol="black", cglwd=0.8, vlcex=1.1)
legend(x=1.2, y=1.4, legend = rownames(RAD), bty = "n", pch=20, col=colours, text.col = "black", text.width = 4, cex=1, pt.cex=1.7, y.intersp=0.4)


library(fmsb)
library(reshape2)
set.seed(99)
data <- man[,2:7]
radar <- rbind(rep(0,10) , rep(-40,10) , man[,2:7])
rownames(man) <- c("BD 2330", "BU Soybean 1", "BU Soybean 2", "BU Soybean 3", "BU Soybean 4","G00056")
colours <- c("#6e7f8fff", "#3f68e1ff", "#00aa88ff", "#ff6145ff", "#ffa07aff", "#9c27b0ff","#4682b4ff")
radarchart(radar, seg = 7, axistype=1 , plwd=2, pcol=colours, caxislabels=seq(-40,0,10),
           plty=1, axislabcol="black", cglwd=0.8, vlcex=1.1)
legend(x=1.2, y=1.4, legend = rownames(man), bty = "n", pch=20, col=colours, text.col = "black", text.width = 4, cex=1, pt.cex=1.7, y.intersp=0.4)
