
library(agricolae)
library(dplyr)
library(tidyverse)
library(multcompView)

##### RCBD_Two-way ANOVA-DMRT_SE_----26Sept2024

##### Interspecific Grafting compatibility evaluation_Jackfruit-Dewa-Chama kathal

## Design: RCBD with 3 Replications
## Graftcombinations: 9 (Treatment)

### Set working directory: "D:/Zubayer_JackGraft_Analysis_26Sept24/GraftCombinations_RCBD_26Sept24"

getwd()
### Read the csv file for Analysis
# Read csv file by R by giving another dataset name that is read by R

JGC_RCBD=read.csv(file.choose(), header = T)
JGC_RCBD

## To see the structure of the R read dataset JGC_RCBD run the following command
str(JGC_RCBD)

### To remove the NA value that is available in column 18
SC_RCBD <- JGC_RCBD[-18]
SC_RCBD
str(SC_RCBD)
### As there is no NA therefore we maintain the same name as SC_RCBD
SC_RCBD<-S_RCBD
SC_RCBD
str(SC_RCBD)

JGC_RCBD$Replication <- as.factor(JGC_RCBD$Replication)
JGC_RCBD$Treatment <- as.factor(JGC_RCBD$Treatment)
attach(JGC_RCBD)
# Variable conversion is needed before going to ANOVA-DMRT test_SE-SD-Mean estimation

JGC_RCBD$Replication = as.factor(JGC_RCBD$Replication)
JGC_RCBD$Graftcombinations = as.factor(JGC_RCBD$Graftcombinations)


attach(JGC_RCBD)
names(JGC_RCBD)

### Two-way ANOVA_RCBD Design with 3-Replicaiton_9 Grafting Combinations_for the Parameter: SR


pHanova <- aov(pH ~ Replication + Treatment)
summary(pHanova)




### Mean separation and lettering_SR
# DMRT mean comparison test-for the parameter of SR


## DMRT test for SR parameter (dependent variable)
### For SR:

duncan.SR = duncan.test(y = pH,
                        trt = Treatment, 
                        DFerror = pHanova$df.residual, 
                        MSerror = deviance(pHanova)/pHanova$df.residual, 
                        group = T, 
                        console = T)


#tukey hsd method
TukeyHSD(aov(pH ~ Treatment))


###Extract lettering from TWO-WAY ANOVA and Tukey’s Test
group_lettering <- multcompLetters4(pHanova, tukey)
group_lettering


### To see the SE for the parameter SR as per the independent variables of the studied Grafting Combinations

SR_SE = JGC_RCBD %>%
  group_by(Graftcombinations) %>%
  summarise(SR.mean = mean(SR),
            std = sd(SR),
            SE = sd(SR)/sqrt(n()))

SR_SE
print(SR_SE)


### GR
GRanova <- aov(GR ~ Replication + Graftcombinations)
summary(GRanova)

duncan.GR = duncan.test(y = GR,
                        trt = Graftcombinations, 
                        DFerror = GRanova$df.residual, 
                        MSerror = deviance(GRanova)/GRanova$df.residual, 
                        group = T, 
                        console = T)


GR_SE = JGC_RCBD %>%
  group_by(Graftcombinations) %>%
  summarise(GR.mean = mean(GR),
            std = sd(GR),
            SE = sd(GR)/sqrt(n()))

GR_SE
print(GR_SE)

## MR
MRanova <- aov(MR ~ Replication + Graftcombinations)
summary(MRanova)

duncan.MR = duncan.test(y = MR,
                        trt = Graftcombinations, 
                        DFerror = MRanova$df.residual, 
                        MSerror = deviance(MRanova)/MRanova$df.residual, 
                        group = T, 
                        console = T)


MR_SE = JGC_RCBD %>%
  group_by(Graftcombinations) %>%
  summarise(MR.mean = mean(MR),
            std = sd(MR),
            SE = sd(MR)/sqrt(n()))

MR_SE
print(MR_SE)

## OLIP
OLIPanova <- aov(OLIP ~ Replication + Graftcombinations)
summary(OLIPanova)

duncan.OLIP = duncan.test(y = OLIP,
                        trt = Graftcombinations, 
                        DFerror = OLIPanova$df.residual, 
                        MSerror = deviance(OLIPanova)/OLIPanova$df.residual, 
                        group = T, 
                        console = T)


OLIP_SE = JGC_RCBD %>%
  group_by(Graftcombinations) %>%
  summarise(OLIP.mean = mean(OLIP),
            std = sd(OLIP),
            SE = sd(OLIP)/sqrt(n()))

OLIP_SE
print(OLIP_SE)


##TLIP
TLIPanova <- aov(TLIP ~ Replication + Graftcombinations)
summary(TLIPanova)

duncan.TLIP = duncan.test(y = TLIP,
                          trt = Graftcombinations, 
                          DFerror = TLIPanova$df.residual, 
                          MSerror = deviance(TLIPanova)/TLIPanova$df.residual, 
                          group = T, 
                          console = T)


TLIP_SE = JGC_RCBD %>%
  group_by(Graftcombinations) %>%
  summarise(TLIP.mean = mean(TLIP),
            std = sd(TLIP),
            SE = sd(TLIP)/sqrt(n()))

TLIP_SE
print(TLIP_SE)

##ThLIP

ThLIPanova <- aov(ThLIP ~ Replication + Graftcombinations)
summary(ThLIPanova)

duncan.ThLIP = duncan.test(y = ThLIP,
                          trt = Graftcombinations, 
                          DFerror = ThLIPanova$df.residual, 
                          MSerror = deviance(ThLIPanova)/ThLIPanova$df.residual, 
                          group = T, 
                          console = T)


ThLIP_SE = JGC_RCBD %>%
  group_by(Graftcombinations) %>%
  summarise(ThLIP.mean = mean(ThLIP),
            std = sd(ThLIP),
            SE = sd(ThLIP)/sqrt(n()))

ThLIP_SE
print(ThLIP_SE)


