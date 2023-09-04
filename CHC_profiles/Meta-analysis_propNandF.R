library(vegan)
library(ggplot2)
library(reshape2)
library(MASS)
library(adegenet)
library(dendextend)
library(colorspace)
library(gplots)
library(RColorBrewer)

setwd("/Volumes/gr_Engel/lab_resources/Manuscripts/2023_Liberti_Maturation/Code/CHC_profiles/")

### Input metadata
id<-read.table("Meta-analysis_propNandF.txt", header = T )
id$Cage<-as.factor(id$Cage)

# Count the number of nurses and foragers in each treatment
dplyr::count(id, Experiment, Day, Treatment) 

# Statistical analyses - distribution of N and F 
library(lmerTest)
resul <- glmer(factor(NandF) ~ Day*Treatment+Size + (1|Experiment/Hive/Cage), family=binomial, data = id) 
summary(resul)
