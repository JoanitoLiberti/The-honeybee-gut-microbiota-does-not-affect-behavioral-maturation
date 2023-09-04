library(ggplot2)
library(dplyr)
library(scales)
library(forcats)

setwd("/Volumes/gr_Engel/lab_resources/Manuscripts/2023_Liberti_Maturation/Code/HPG_size/")

dt<-read.delim("HPGsize_data.txt", header = T, fill=T, na.strings=c(""))

dt$HG_size<-factor(dt$HG_size)
dt <- dt %>%
  mutate(HG_size = fct_rev(HG_size))

dt$Hive<-as.factor(dt$Hive)

# Stacked bar plots
p1 <- ggplot(dt, 
       aes(x = factor(Behaviour, levels=c("nurse", "forager")), 
           fill = factor(HG_size))) + 
  geom_bar(position = "fill") +
  ylab("Percentage of bees") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Blues", limits=rev) + 
  labs(
       fill = "Hg size",
       x = NULL) +
  theme_minimal()
p1

ggsave(height=5,width=3,dpi=300, filename="HGsizeByBehav.pdf", useDingbats=FALSE)

p2 <- ggplot(dt, 
             aes(x = factor(Treatment, levels=c("MD", "CL_Bifi", "CL_13", "CL")), 
                 fill = factor(HG_size))) + 
  geom_bar(position = "fill") +
  ylab("Percentage of bees") +
  # scale_x_discrete(labels=c('MD', 'CL_Bifi', 'CL_13', 'CL')) +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Blues", limits=rev) + 
  labs(
    fill = "Hg size",
    x = NULL) +
  theme_minimal()
p2
ggsave(height=4,width=4,dpi=300, filename="HGsizeByTreatment.pdf", useDingbats=FALSE)

# Count the number of observations
dplyr::count(dt, Behaviour) 
dplyr::count(dt, Treatment) 

# Statistical analyses
kruskal.test(as.numeric(HG_size) ~ Treatment , data = dt)

kruskal.test(as.numeric(HG_size) ~ Behaviour , data = dt)
