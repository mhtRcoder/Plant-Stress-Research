library(tidyverse)
library(ggplot2)
library(rstatix)
library(dplyr)
library(agricolae)

Color <- c("Green","Red","Orange","Blue")
Height <- c(30,40,50,70)
yield <- c(5,8,11,7)
biomass <- c(10,12,14,15)
table <- data.frame(Color,Height,yield,biomass)
view(table)
newdf <- data.frame(Color="Violet", Height=80, yield=8, biomass=17)
table <- rbind(table,newdf)
view(table)
getwd()
table <- select(table,COlor,Height,yield)

#jahid_sir_analyzing_crd

setwd("F:/CST_Training")
getwd()
df <- read.csv(file.choose("jsa"),header = TRUE)
df
head(df)
df$Replication <-factor(df$Genotype)
df$
class(df)
str(df)

df$Genotype <- as.factor(df$Genotype)
df$Replication <- as.factor(df$Replication)
attach(df)
FGPanova <- aov(FGP ~ Genotype)
summary(FGPanova)

## DMRT test for FGP parameter (dependent variable)
### For FGP:

duncan_FGP =duncan.test(y = FGP,
                         trt = Genotype, 
                         DFerror = FGPanova$df.residual, 
                         MSerror = deviance(FGPanova)/FGPanova$df.residual, 
                         group = T, 
                         console = T)


### To see the SE for the parameter FGP as per the independent variables of the studied (Genotype) Genotypes

FGP_SE = df %>%
  group_by(Genotype) %>%
  summarise(FGP.mean = mean(FGP),
            std = sd(FGP),
            SE = sd(FGP)/sqrt(n()))

FGP_SE

print(FGP_SE)


#for *GR* now we will calculate the ANOVA:

GRanova <- aov(GR ~ Genotype)
summary(GRanova)

## DMRT test for GR parameter (dependent variable)
### For GR:

duncan_GR =duncan.test(y = GR,
                        trt = Genotype, 
                        DFerror = GRanova$df.residual, 
                        MSerror = deviance(GRanova)/GRanova$df.residual, 
                        group = T, 
                        console = T)



### To see the SE for the parameter GR as per the independent variables of the studied (Genotype) Genotypes

GR_SE = df %>%
  group_by(Genotype) %>%
  summarise(GR.mean = mean(GR),
            std = sd(GR),
            SE = sd(GR)/sqrt(n()))

GR_SE

print(GR_SE)



