# Calcium-Analysis-for-differentiating-LUHMES-cells.R

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggstatsplot)


###########################################{DAY 1}#######################################################
##after alignment and ROIs\DataAnalysis2
Day1 <-read.csv2("C:/Users/0200166839/Documents/analysis/day11.csv", sep=",")
Day3 <-read.csv2("C:/Users/0200166839/Documents/analysis/day3.csv", sep=",")
Day6 <-read.csv2("C:/Users/0200166839/Documents/analysis/day66.csv", sep=",")

dim(Day1)
dim(Day3)
dim(Day6)



Day1$Day = "Day1"
Day1$Concentration = "100"



Day3$Day = "Day3"
Day3$Concentration = "100"


Day6$Day = "day6"
Day6$Concentration = "100"




#Merge dataset
M<-bind_rows(Day1,Day3, Day6)



#conversion into numeric values
M$SpikeWidth = as.numeric(M$SpikeWidth)
M$Tav = as.numeric(M$Tav)
M$SpikeAreaMean = as.numeric(M$SpikeAreaMean)

#remove outliers
M = M %>%
  group_by(Day1a,Concentration) %>%
  filter(!(abs(SpikeWidth - median(SpikeWidth)) > 2*sd(SpikeWidth)) &
           !(abs(Tav - median(Tav)) > 2*sd(Tav)) &  
           !(abs(SpikeAreaMean - median(SpikeAreaMean)) > 2*sd(SpikeAreaMean)) &
           !(abs(SpikeAreaSD - median(SpikeAreaSD)) > 2*sd(SpikeAreaSD))) 



#####################################################################  
#Plots
1. 
M %>%
  ggplot(aes(x=Day, y=SpikeAreaMean, group=Day)) + 
  geom_boxplot(outlier.shape = NA) +
  ggtitle("Spike Area Mean of all the cells from Day 1, Day 3 and Day 6") +
  ylim(1,3.5)
2. 
M %>%
  ggplot(aes(x=Day, y=Tav, group=Day)) + 
  geom_boxplot(outlier.shape = NA) +
  #scale_y_continuous(breaks = seq(0, 200, 50)) +
  ggtitle("Tav of all the cells from Day 1, Day 3 and Day 6") +
  ylim(10,85)
3. 
M %>%
  ggplot(aes(x=Day, y=SpikeWidth, group=Day)) + 
  geom_boxplot(outlier.shape = NA) +
  #scale_y_continuous(breaks = seq(0, 60, 20)) 
  ggtitle("Spike width of all the cells from Day 1, Day 3 and Day 6") +
  ylim(10,50)
#####################################################################################
#p-values
##########Tav##############
D1_Tav <- as.numeric(Day1[,18])
t.test(D1_Tav)
#p-value < = 1.535e-05
wilcox.test(D1_Tav)
#p-value < 2.2e-16
###
D3_Tav <- as.numeric(Day3[,18])
t.test(D3_Tav)
#p-value = 1.8e-13
wilcox.test(D3_Tav)
#p-value < 5.33e-13
###
D6_Tav <- as.numeric(Day6[,18])
t.test(D6_Tav)
#p-value = 2.072e-12
wilcox.test(D6_Tav)
#p-value = < 2.2e-16

##########Spike Width##############
D1_SW <- as.numeric(Day1[,21])
t.test(D1_SW)
#p-value = = 8.621e-06
wilcox.test(D1_SW)
#p-value < 1.151e-10
###
D3_SW <- as.numeric(Day3[,21])
t.test(D3_SW)
#p-value = 2.258e-11
wilcox.test(D3_SW)
#p-value < 4.144e-12
###
D6_SW <- as.numeric(Day6[,21])
t.test(D6_SW)
#p-value = 1.515e-06
wilcox.test(D6_SW)
#p-value = < 2.2e-16
##########Spike Area Mean##############
D1_SAM <- as.numeric(Day1[,26])
t.test(D1_SAM)
#p-value = 0.02189
wilcox.test(D1_SAM)
#p-value <  4.547e-13
###
D3_SAM <- as.numeric(Day3[,26])
t.test(D3_SAM)
#p-value = = 7.813e-13
wilcox.test(D3_SAM)
#p-value < 7.813e-13
###
D6_SAM <- as.numeric(Day6[,26])
t.test(D6_SAM)
#p-value = < 2.2e-16
wilcox.test(D6_SAM)
#p-value = < 2.2e-16
#############################################################


