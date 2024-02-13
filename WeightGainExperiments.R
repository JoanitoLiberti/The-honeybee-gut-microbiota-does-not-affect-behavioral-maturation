# Joanito Liberti, UNIL
# Script usage: Plot weight gain of bees

library(tidyr)
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)

setwd("/Volumes/gr_Engel/lab_resources/Manuscripts/2023_Liberti_Maturation/Code/Weight/")

data_wide<-read.table("WeightGain_Data.txt", header = TRUE, fill=TRUE, sep="\t")

data_long <- gather(data_wide, Day, WeightGain, Day2:Day10, factor_key=TRUE)
data_long

data_long$Day <- as.character(data_long$Day)
data_long$Day[data_long$Day=="Day2"] <- 2
data_long$Day[data_long$Day=="Day4"] <- 4
data_long$Day[data_long$Day=="Day6"] <- 6
data_long$Day[data_long$Day=="Day8"] <- 8
data_long$Day[data_long$Day=="Day10"] <- 10
data_long$Day <- factor(data_long$Day, levels = c("2","4","6", "8", "10"), order=T)
data_long$Treatment <- factor(data_long$Treatment, levels = c("MD","CL"))
data_long$NandF <- factor(data_long$NandF, levels = c("nurse","forager"), order=T)

# Plot as repeated measures
data_long$BeeID <- with(data_long, interaction(as.factor(data_long$Sample_ID),  data_long$HiveBox))

give.n <- function(x){return(c(y = 1, label = length(x))) # experiment with the multiplier to find the perfect position
}
gg.base <- ggplot(data_long, aes(x = Day, y = WeightGain, color = Treatment))
gg.treat <- gg.base + geom_line(aes(color = Treatment, group = BeeID), alpha=0.4) +
  stat_summary(aes(group = Treatment, color = Treatment), geom = "line", fun = mean, size = 2.5) +
  stat_summary(aes(group = Treatment, color = Treatment), geom = "errorbar", width = .1, position = position_dodge(.1), color = "black") +
  theme_bw() +
  color_palette(c("#C77CFF","#00BFC4")) +
  ylab("Weight gain relative to initial body weight") +
  xlab("Days from inoculation") +
  facet_wrap(.~Experiment) + 
  theme_bw(base_size=14) +
  guides(color="none") + 
  stat_summary(fun.data = give.n, geom = "text", position=position_nudge(x = c(-0.2,0.2), y = c(0,0))) # add number of observations
gg.treat

ggsave(filename="WeightGainExperiments.pdf", height=10, width=14, dpi=300, useDingbats=FALSE)

pollen <- subset(data_long, Experiment=="Pollen exp")
beebread <- subset(data_long, Experiment=="Bee Bread exp")

# Statistical analyses - effect of time and treatment on weight gain in the two experiments
library(lmerTest)
resul <- lmer(WeightGain ~ Day*Treatment + (1|Hive/UniqueBox/BeeID), data = pollen)
summary(resul)
anova(resul)

resul <- lmer(WeightGain ~ Day*Treatment + (1|Hive/UniqueBox/BeeID), data = beebread)
summary(resul)
anova(resul)

