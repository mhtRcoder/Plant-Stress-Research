#Mahmud Yield bar plots for panellin
#####Two-way ANOVA & Bar plot###############
library(ggplot2)
library(ggthemes)
library(multcompView)
library(dplyr)
library(stats) 
library(ggpubr)
library(cowplot)
data <- BY
View(BY)
data$Gen = as.factor(data$Gen)
data$GC = as.factor(data$GC)
data$ID = as.factor(data$ID)
names(BY)
###Calculating Two Way ANOVA
anova <- aov(PH ~ Gen*GC, data = data) #two way anova
summary(anova)

###Multiple Mean Comparison Analysis (only if you have significant differences in Two-way anova)
tukey <- TukeyHSD(anova)
tukey

###Extract lettering from TWO-WAY ANOVA and Tukey’s Test
group_lettering <- multcompLetters4(anova, tukey)
group_lettering

group_lettering2 <- data.frame(group_lettering$`Gen:GC`$Letters)
group_lettering2

###Calculating and adding mean, sd and lettering columns to the data set
mean_data <- data %>% 
  group_by(Gen, GC) %>% 
  summarise(mean=mean(PH), sd = sd(PH)) %>% #to calculate mean and SD
  arrange(desc(mean)) #to arange in descending order

tibble(mean_data)

mean_data$group_lettering <- group_lettering2$group_lettering..Gen.GC..Letters

###Pub_Ready Barplot
p1 <- ggplot(mean_data, aes(x = Gen, y = mean,group=GC))  +
  geom_bar(color="black", position=position_dodge(0.8), width = 0.7, stat = "identity", aes(fill = GC), show.legend = F) + #barplot
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd),width = 0.3, position=position_dodge(0.8)) + 
  geom_text(aes(label = group_lettering, y = mean + sd), vjust=-0.4, position=position_dodge(0.8)) + #add letters
  scale_fill_manual(values = c("#1E90FF", "#FF7F50")) + #theme setting
  theme(axis.text.x = element_text(size=8, angle = 0, vjust = 0.5, hjust=1))+
  labs(#this will add labels 
    x = "", 
    y = "Plant height (cm)",
    title = "",
    subtitle = "",
    fill = "GC"
  ) +
  #facet_wrap(~Tre)+
  ylim(0,65)+ #change your yaxis limits based on the letters
  ggthemes::theme_par(); p1

#2

###Calculating Two Way ANOVA stem wt
anova <- aov(Stem.wt ~ Gen*GC, data = data) #two way anova
summary(anova)

###Multiple Mean Comparison Analysis (only if you have significant differences in Two-way anova)
tukey <- TukeyHSD(anova)
tukey

###Extract lettering from TWO-WAY ANOVA and Tukey’s Test
group_lettering <- multcompLetters4(anova, tukey)
group_lettering

group_lettering2 <- data.frame(group_lettering$`Gen:GC`$Letters)
group_lettering2

###Calculating and adding mean, sd and lettering columns to the data set
mean_data <- data %>% 
  group_by(Gen, GC) %>% 
  summarise(mean=mean(Stem.wt), sd = sd(Stem.wt)) %>% #to calculate mean and SD
  arrange(desc(mean)) #to arange in descending order

tibble(mean_data)

mean_data$group_lettering <- group_lettering2$group_lettering..Gen.GC..Letters

###Pub_Ready Barplot
p2 <- ggplot(mean_data, aes(x = Gen, y = mean,group=GC))  +
  geom_bar(color="black", position=position_dodge(0.8), width = 0.7, stat = "identity", aes(fill = GC), show.legend = F) + #barplot
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd),width = 0.3, position=position_dodge(0.8)) + 
  geom_text(aes(label = group_lettering, y = mean + sd), vjust=-0.4, position=position_dodge(0.8)) + #add letters
  scale_fill_manual(values = c("#1E90FF", "#FF7F50")) + #theme setting
  theme(axis.text.x = element_text(size=8, angle = 0, vjust = 0.5, hjust=1))+
  labs(#this will add labels 
    x = "", 
    y = "Stem wt (gm)",
    title = "",
    subtitle = "",
    fill = "GC"
  ) +
  #facet_wrap(~Tre)+
  ylim(0,7)+ #change your yaxis limits based on the letters
  ggthemes::theme_par(); p2

#3

#Mahmud Yield bar plots for panellin Leaf wt
#####Two-way ANOVA & Bar plot###############
###Calculating Two Way ANOVA
###Calculating Two Way ANOVA
anova <- aov(Leaf.wt ~ Gen*GC, data = data) #two way anova
summary(anova)

###Multiple Mean Comparison Analysis (only if you have significant differences in Two-way anova)
tukey <- TukeyHSD(anova)
tukey

###Extract lettering from TWO-WAY ANOVA and Tukey’s Test
group_lettering <- multcompLetters4(anova, tukey)
group_lettering

group_lettering2 <- data.frame(group_lettering$`Gen:GC`$Letters)
group_lettering2

###Calculating and adding mean, sd and lettering columns to the data set
mean_data <- data %>% 
  group_by(Gen, GC) %>% 
  summarise(mean=mean(Leaf.wt), sd = sd(Leaf.wt)) %>% #to calculate mean and SD
  arrange(desc(mean)) #to arange in descending order

tibble(mean_data)

mean_data$group_lettering <- group_lettering2$group_lettering..Gen.GC..Letters

###Pub_Ready Barplot
p3 <- ggplot(mean_data, aes(x = Gen, y = mean,group=GC))  +
  geom_bar(color="black", position=position_dodge(0.8), width = 0.7, stat = "identity", aes(fill = GC), show.legend = F) + #barplot
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd),width = 0.3, position=position_dodge(0.8)) + 
  geom_text(aes(label = group_lettering, y = mean + sd), vjust=-0.4, position=position_dodge(0.8)) + #add letters
  scale_fill_manual(values = c("#1E90FF", "#FF7F50")) + #theme setting
  theme(axis.text.x = element_text(size=8, angle = 0, vjust = 0.5, hjust=1))+
  labs(#this will add labels 
    x = "", 
    y = "Leaf wt (gm)",
    title = "",
    subtitle = "",
    fill = "GC"
  ) +
  #facet_wrap(~Tre)+
  ylim(0,7)+ #change your yaxis limits based on the letters
  ggthemes::theme_par(); p3


#4
anova <- aov(Pod.wt ~ Gen*GC, data = data) #two way anova
summary(anova)

###Multiple Mean Comparison Analysis (only if you have significant differences in Two-way anova)
tukey <- TukeyHSD(anova)
tukey

###Extract lettering from TWO-WAY ANOVA and Tukey’s Test
group_lettering <- multcompLetters4(anova, tukey)
group_lettering

group_lettering2 <- data.frame(group_lettering$`Gen:GC`$Letters)
group_lettering2

###Calculating and adding mean, sd and lettering columns to the data set
mean_data <- data %>% 
  group_by(Gen, GC) %>% 
  summarise(mean=mean(Pod.wt), sd = sd(Pod.wt)) %>% #to calculate mean and SD
  arrange(desc(mean)) #to arange in descending order

tibble(mean_data)

mean_data$group_lettering <- group_lettering2$group_lettering..Gen.GC..Letters


###Pub_Ready Barplot
p4 <- ggplot(mean_data, aes(x = Gen, y = mean,group=GC))  +
  geom_bar(color="black", position=position_dodge(0.8), width = 0.7, stat = "identity", aes(fill = GC), show.legend = F) + #barplot
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd),width = 0.3, position=position_dodge(0.8)) + 
  geom_text(aes(label = group_lettering, y = mean + sd), vjust=-0.4, position=position_dodge(0.8)) + #add letters
  scale_fill_manual(values = c("#1E90FF", "#FF7F50")) + #theme setting
  theme(axis.text.x = element_text(size=8, angle = 0, vjust = 0.5, hjust=1))+
  labs(#this will add labels 
    x = "", 
    y = "Pod wt (gm)",
    title = "",
    subtitle = "",
    fill = "GC"
  ) +
  #facet_wrap(~Tre)+
  ylim(0,6)+ #change your yaxis limits based on the letters
  ggthemes::theme_par(); p4



#5 Mahmud Yield bar plots for panellin

###Calculating Two Way ANOVA
anova <- aov(Grain.Wt ~ Gen*GC, data = data) #two way anova
summary(anova)

###Multiple Mean Comparison Analysis (only if you have significant differences in Two-way anova)
tukey <- TukeyHSD(anova)
tukey

###Extract lettering from TWO-WAY ANOVA and Tukey’s Test
group_lettering <- multcompLetters4(anova, tukey)
group_lettering

group_lettering2 <- data.frame(group_lettering$`Gen:GC`$Letters)
group_lettering2

###Calculating and adding mean, sd and lettering columns to the data set
mean_data <- data %>% 
  group_by(Gen, GC) %>% 
  summarise(mean=mean(Grain.Wt), sd = sd(Grain.Wt)) %>% #to calculate mean and SD
  arrange(desc(mean)) #to arange in descending order

tibble(mean_data)

mean_data$group_lettering <- group_lettering2$group_lettering..Gen.GC..Letters

###Pub_Ready Barplot
p5 <- ggplot(mean_data, aes(x = Gen, y = mean,group=GC))  +
  geom_bar(color="black", position=position_dodge(0.8), width = 0.7, stat = "identity", aes(fill = GC), show.legend = F) + #barplot
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd),width = 0.3, position=position_dodge(0.8)) + 
  geom_text(aes(label = group_lettering, y = mean + sd), vjust=-0.4, position=position_dodge(0.8)) + #add letters
  scale_fill_manual(values = c("#1E90FF", "#FF7F50")) + #theme setting
  theme(axis.text.x = element_text(size=8, angle = 0, vjust = 0.5, hjust=1))+
  labs(#this will add labels 
    x = "", 
    y = "Grain wt (gm)",
    title = "",
    subtitle = "",
    fill = "GC"
  ) +
  #facet_wrap(~Tre)+
  ylim(0,2)+ #change your yaxis limits based on the letters
  ggthemes::theme_par(); p5




#6 Mahmud Yield bar plots for panellin
###Calculating Two Way ANOVA
anova <- aov(AGB ~ Gen*GC, data = data) #two way anova
summary(anova)

###Multiple Mean Comparison Analysis (only if you have significant differences in Two-way anova)
tukey <- TukeyHSD(anova)
tukey

###Extract lettering from TWO-WAY ANOVA and Tukey’s Test
group_lettering <- multcompLetters4(anova, tukey)
group_lettering

group_lettering2 <- data.frame(group_lettering$`Gen:GC`$Letters)
group_lettering2

###Calculating and adding mean, sd and lettering columns to the data set
mean_data <- data %>% 
  group_by(Gen, GC) %>% 
  summarise(mean=mean(AGB), sd = sd(AGB)) %>% #to calculate mean and SD
  arrange(desc(mean)) #to arange in descending order

tibble(mean_data)

mean_data$group_lettering <- group_lettering2$group_lettering..Gen.GC..Letters

###Pub_Ready Barplot
p6 <- ggplot(mean_data, aes(x = Gen, y = mean,group=GC))  +
  geom_bar(color="black", position=position_dodge(0.8), width = 0.7, stat = "identity", aes(fill = GC), show.legend =TRUE) + #barplot
  geom_errorbar(aes(ymin = mean-sd, ymax=mean+sd),width = 0.3, position=position_dodge(0.8)) + 
  geom_text(aes(label = group_lettering, y = mean + sd), vjust=-0.4, position=position_dodge(0.8)) + #add letters
  scale_fill_manual(values = c("#1E90FF", "#FF7F50")) + #theme setting
  theme(axis.text.x = element_text(size=8, angle = 0, vjust = 0.5, hjust=1))+
  labs(#this will add labels 
    x = "", 
    y = "Above ground biomass (gm)",
    title = "",
    subtitle = "",
    fill = "GC"
  ) +
  #facet_wrap(~Tre)+
  ylim(0,19)+ #change your yaxis limits based on the letters
  ggthemes::theme_par(); p6


#paneling all graphs 2 col and no width fixed
combined_plots <- cowplot::plot_grid(
  p1, p2, p3, p4, p5, p6, 
  ncol = 3)
# Display the combined plot
print(combined_plots)
#to rotate the x axis label vertically 
# Assuming p1, p2, p3, p4, p5, p6 are your individual plots

p1 <- p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2 <- p2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p3 <- p3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p4 <- p4 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p5 <- p5 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p6 <- p6 + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Combine the modified plots
combined_plots <- cowplot::plot_grid(
  p1, p2, p3, p4, p5, p6, 
  ncol = 3
)

# Display the combined plot
print(combined_plots)




#paneling all graphs
combined_plots <- cowplot::plot_grid(
  p1, p2, p3, p4, p5, p6, 
  ncol = 3, 
  rel_widths = c(.8, .8, .8, .8, .8, .8),  # Adjust relative widths
  rel_heights = c(.8, .8)  # Adjust relative heights for rows
)
# Display the combined plot
print(combined_plots)

#arrangement
combined_plots <- cowplot::plot_grid(p1, p2, ncol = 2)

# Display the combined plot
print(combined_plots)
