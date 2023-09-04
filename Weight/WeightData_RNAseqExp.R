library(ggplot2)
library(ggbeeswarm)
library(magrittr)
library(ggpubr) 
library(RColorBrewer)

setwd("/Volumes/gr_Engel/lab_resources/Manuscripts/2023_Liberti_Maturation/Code/Weight/")

# load data
dt <- read.csv("Weights_RNAseqExp.csv")
dt$Treatment <- factor(dt$Treatment, levels = c("MD","CL_Bifi","CL_13","CL"))
dt$Hive <- factor(dt$Hive, levels = c("H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10"))

give.n <- function(x){return(c(y = 78, label = length(x))) # experiment with the multiplier to find the perfect position
}
bee1 <- ggplot(dt, aes(x = Treatment, y = Bee_weight_mg))+ 
  geom_beeswarm(aes(color=Treatment),cex=2)+
  geom_boxplot(outlier.shape = NA, alpha=0.1)+
  scale_x_discrete()+
  ylab("Body weight (mg)") +
  stat_summary(fun.data = give.n, geom = "text") + # add number of observations
  scale_color_manual(values = c("#C77CFF","#F8766D","#7CAE00","#00BFC4")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(bee1)

give.n <- function(x){return(c(y = 8, label = length(x))) # experiment with the multiplier to find the perfect position
}
bee2 <- ggplot(dt, aes(x = Treatment, y = Gut_weight_mg))+ 
  geom_beeswarm(aes(color=Treatment),cex=2)+
  geom_boxplot(outlier.shape = NA, alpha=0.1)+
  scale_x_discrete()+
  ylab("Gut weight (mg)")+
  stat_summary(fun.data = give.n, geom = "text") + # add number of observations
  scale_color_manual(values = c("#C77CFF","#F8766D","#7CAE00","#00BFC4")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

print(bee2)

ggarrange(bee1, bee2, ncol=2, common.legend = T, legend="right")
# ggsave(file="BeeWeights.pdf", width=7, height=3.5, useDingbats=FALSE)

dt$cage<-as.factor(paste0(dt$Hive,dt$Box))

#Stats
library(lmerTest)
resul <- lmer(Bee_weight_mg ~ Treatment + (1|Hive/cage) , data=dt)
summary(resul)
anova(resul)

resul <- lmer(Gut_weight_mg ~ Treatment + (1|Hive/cage), data=dt)
summary(resul)
anova(resul)
