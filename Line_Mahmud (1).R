
library(ggplot2)
library(ggpubr)
library(reshape2)

df <- MAN1[1:144, c(1:2,4,6)]
df.m <- melt(df, id = c("Gen", "WR", "DAS"))
df.m

fig <- ggline(data=df.m, x="DAS", y="value", facet.by = "Gen", ncol=3,
              color= "WR", scales="free_y", add=c("mean_sd"),
              linetype = "WR", position= position_dodge(0), size= 0.5, shape= "WR",
              add.params = list(size=0.5, width=0.2))+
  
  scale_shape_manual(values=c(16, 1))+
  scale_linetype_manual(values=c("solid", "solid"))+
  scale_color_manual(values=c("blue", "red"))+
  
  stat_compare_means(aes(group = WR), method= "t.test", label = "p.signif", vjust = 0.5, size=4, color="black") +
  xlab("Days after sowing") + 
  ylab("LRWC")+
  theme_bw(base_size = 14)

fig

df <- Data[1:24, 1:19]
df.m <- melt(df, id = c("Time", "Var"))
df.m

ggboxplot(df.m, x = "WR", y = "value", color= "DAS", palette =c("navy", "red"), bxp.errorbar = TRUE, bxp.errorbar.width = 0.30,
          facet.by = "variable", scales="free_y",  ncol=6, outlier.colour = NA) +
  
  
  stat_compare_means(aes(group = Var), method= "t.test", label = "p.signif", vjust = 0.5, size=4, color="black") +
  theme_bw(base_size = 14) +
  stat_compare_means(aes(group = WR), method= "t.test", label = "p.signif", vjust = 0.5, size=4, color="black") +
  stat_summary(fun=mean, geom ="point", shape=20, color= "black", size=4) +
  geom_jitter(color = "light slate grey", size= 0.7, position = position_jitter(width = 0.15))+
  theme(legend.position="none", text = element_text(face ="bold", size=16, color = "black"))+
  theme(text = element_text(size = 16),
        axis.text.x = element_text(color="black", size=13), 
        axis.text.y = element_text(color="black", size=13))


#Create letters showing significance differences
library(multcomp)
library(multcompView)
library(dplyr)

anova <- aov(Proline ~ WR*DAS, data = df)
summary(anova)
tukey <- TukeyHSD(anova, ordered = T)
tukey
cld <- multcompLetters4(anova, tukey)
cld
cld <- as.data.frame.list(cld$'WR:Gen')
cld

#Link means with significant letters
dfsum <- df %>%
  group_by(DAS, WR) %>%
  summarise(mean = mean(Proline), sd = sd(Proline), .groups = "drop") %>%
  arrange(desc(mean)) %>%
  mutate(sig = cld$Letters)
dfsum

#Add letters showing significant differences in the final figure
p1 <- (fig+
  geom_text(data=dfsum,
            aes(x = DAS, y = mean+sd+0.02,
                group = WR, label = sig),
            position = position_dodge(0),
            color = "black", fontface = "plain", size = 4))
p1


