
#########
df <- Line
df

Stage <-df$Stage
Drop <- df$Drop
BS <- df$BS
df$Drop <- as.factor(df$Drop)
df$Stage <- as.factor(df$Stage)

library(ggplot2)
library(ggpubr)

fig <- ggline(data=df, x="Drop", y="BS",
              color= "Stage", add=c("mean_sd"),
              linetype = "Stage", position= position_dodge(0), size= 0.5, shape= "Stage",
              add.params = list(size=0.5, width=0.2))+
  scale_shape_manual(values=c(16, 1, 15, 0))+
  scale_linetype_manual(values=c("solid", "dotted", "solid", "dotted"))+
  scale_color_manual(values=c("blue", "blue", "red", "red"))+
  scale_y_continuous(limits = c(0.5, 4.5))+
  xlab("Days after dropping") + 
  ylab("Bruising severity")+
  theme_bw()

fig
  
  
#Create letters showing significance differences
library(multcomp)
library(multcompView)
library(dplyr)

anova <- aov(BS ~ Drop*Stage, data = df)
summary(anova)
tukey <- TukeyHSD(anova, ordered = T)
tukey
cld <- multcompLetters4(anova, tukey)
cld
cld <- as.data.frame.list(cld$'Drop:Stage')
cld

#Link means with significant letters
dfsum <- df %>%
  group_by(Drop, Stage) %>%
  summarise(mean = mean(BS), sd = sd(BS), .groups = "drop") %>%
  arrange(desc(mean)) %>%
  mutate(sig = cld$Letters)
dfsum

#Add letters showing significant differences in the final figure
fig+
  geom_text(data=dfsum,
            aes(x = Drop, y = mean+sd+0.1,
                group = Stage, label = sig),
            position = position_dodge(0),
            color = "black", fontface = "plain", size = 4)
  

