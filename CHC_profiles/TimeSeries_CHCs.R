library(vegan)
library(ggplot2)
library(reshape2)
library(MASS)
library(adegenet)
library(dendextend)
library(colorspace)
library(gplots)
library(RColorBrewer)
library(ggpubr)

setwd("/Volumes/gr_Engel/lab_resources/Manuscripts/2023_Liberti_Maturation/Code/CHC_profiles/")

### Load CHC data
newbee<-read.csv("Timeseries_CHCdata.csv", header = T, fill=TRUE, row.names=1)
bee2<-newbee[,-c(1:4)]

Mapping <- read.table("IDs_TimeSeries_CHCs.txt", sep="\t", header=T)

bee2<-bee2/100
bee2<-t(bee2)

Mapping <- Mapping[Mapping$Sample_ID %in% rownames(bee2),]
Mapping$Cage<-factor(paste0(Mapping$Treatment, Mapping$Hive))

# Count the number of nurses and foragers in each treatment
dplyr::count(Mapping, Treatment, Work) 
dplyr::count(Mapping, Hive, Day, Treatment) 

p1 <- ggplot(subset(Mapping, !is.na(Work)), 
             aes(x = factor(Treatment, levels=c("MD", "CL")), 
                 fill = factor(Work, levels=c("Newly emerged", "Nurse transition", "Nurse", "Forager transition", "Forager")))) + 
  geom_bar(position = "fill") +
  ylab("Percentage of bees") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = scales::percent) +
  scale_fill_manual(values = c("Newly emerged" = "#C9C9C9", "Nurse transition" = "#D8B465", "Nurse" = "#E69F00", "Forager transition" = "#7AAFAF", "Forager" = "#56B4E9")) +
  facet_grid(.~Day)+
  labs(
    fill = "CHC-classification",
    x = NULL) +
  theme_minimal()

p1
ggsave(height=3,width=5,dpi=300, filename="PropByTreatment-TimeSeries.pdf", useDingbats=FALSE)

Mapping$Work <- factor(Mapping$Work, levels=c("Newly emerged", "Nurse transition", "Nurse", "Forager transition", "Forager"), ordered = TRUE )

# We remove Day 0 as we do not expect treatment effects at time of inoculation
Day<-subset(Mapping, Day!="0") 
bee3 <- bee2[-c(1:18),]

Day$Work <- factor(Day$Work, levels=c("Nurse transition",  "Nurse", "Forager transition", "Forager"), ordered = TRUE )
Day$WorkNum <- as.numeric(Day$Work)

# Ordered logistic regression
library(dplyr)
Day$Treatment <- recode_factor(Day$Treatment, MD  = "0", CL = "1")

library(ordinal)
full_mod<-clmm(Work~Treatment*Day + Hive + (1|HiveBox),data=Day, Hess = T)
no.int<-clmm(Work~Treatment + Day + Hive + (1|HiveBox),data=Day, Hess = T)
no.treat<-clmm(Work~Day + Hive + (1|HiveBox),data=Day, Hess = T)
as.data.frame(anova(full_mod, no.int)) #interaction not significant
as.data.frame(anova(no.treat, no.int)) #treatment not significant
# (Cumulative Link Mixed Model fitted with the Laplace approximation with Hive as random effect, LR=2.04, df=1, p=0.153)

# in classic R
mds1 <- metaMDS(bee2, distance="bray", k=2, trymax=100, autotransform=TRUE, noshare=0.1, expand=TRUE, trace=1)
ordiplot(mds1, type="text", display = "sites")

# in ggplot with hivebees
## create table for NMDS X and Y values of chemical compounds
data.scores<-as.data.frame(scores(mds1)$sites)
data.scores$site<-rownames(data.scores)
data.scores

## create table for NMDS X and Y values of individuals
species.scores<-as.data.frame(scores(mds1,"species"))
species.scores$species<-rownames(species.scores)
species.scores

# We make sure the order of the columns (Sample_IDs) in the metadata matches that of the ordination scores
Mapping <- Mapping[ order(match(Mapping$Sample_ID, rownames(data.scores))), ]

#Graph
pl1 <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,color=Mapping$Day, shape=Mapping$Treatment),size=3) +
  coord_equal() +
  scale_color_viridis_c(breaks = c(0, 2, 4, 6, 8, 10)) +
  theme_bw() +
  labs(shape = "Treatment", color = "Days") +
  theme(axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
pl1
ggsave(file="CHCordination-TimeSeries.pdf",width=6,height=6, pointsize=8)

# Calculate cage centroids 
beta <- betadisper(vegdist(bee3, "bray"), Day$HiveBox)
cage_centroids<- beta$centroids + abs(min(beta$centroids)) # We add a constant to make values positive for Bray Curtis dissimilarities

# Metadata by cage
idTest <- unique(Day[c("HiveBox","Hive","Treatment")])

# We make sure the order of the columns in the metadata matches that of the CHC data
idTest <- idTest[ order(match(idTest$HiveBox, rownames(cage_centroids))), ]

##Stats full model
set.seed(7586)

adonis2(cage_centroids ~ idTest$Treatment + idTest$Hive) # main effect

adonis2(bee3 ~ Day$Day +  Day$HiveBox + Day$Day:Day$Treatment) # other effects


##Heatmap + Dendrogram
###calculate clustering method
bee2
dmega<-dist(bee2,method="euclidean")
hmega<-stats:::hclust(dmega,method="ward.D2")

dend<-as.dendrogram(hmega)

dend<-dendextend:::rotate(dend,1:108)
dend<-color_branches(dend,k=5,col=c("#E69F00","#7AAFAF","#56B4E9","#D8B465","#C9C9C9"))

Treatment2<-as.factor(Mapping$timetreat)
labels_colors(dend) <-
  rainbow_hcl(10)[sort_levels_values(
    as.numeric(Treatment2)[order.dendrogram(dend)]
  )]
plot(hmega)

## Make Cluster Dendrogram
dend
dend <- hang.dendrogram(dend,hang_height=0.1)

dend <- dendextend::set(dend, "labels_cex", 0.5)
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Honeybee Microbiome depleted CHC over time: Euclidean distances with Ward's Hierarchical clustering:Ward_2", 
     horiz =  TRUE,  nodePar = list(cex = .007))

library(RColorBrewer)
nb.cols <- 100
mycolors <- colorRampPalette(brewer.pal(9, "Oranges"))(nb.cols)

# Heatmap with time as side color bar
cols = c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
names(cols) <- levels(factor(Mapping$Day))
Mapping$colors_day = cols[as.character(Mapping$Day)]

dev.new()
pdf(file="CHCheatmap-TimeSeries_Time.pdf",width=4,height=4, pointsize=8)

{gplots::heatmap.2(as.matrix(bee2), 
                   main = "Relative percentages clustering of microbiome CHC Honeybees",
                   srtCol = 90,
                   dendrogram = "row",
                   Rowv = dend,
                   Colv = "NA", # this to make sure the columns are not ordered
                   trace="none",          
                   margins =c(5,0.1),      
                   key.xlab = "Relative Percentage on Cuticle",
                   RowSideColors = Mapping$colors_day, # to add nice colored strips        
                   col = mycolors)
  legend("topright", legend = levels(factor(Mapping$Day)), fill = cols)}

dev.off()

# Now we plot the same heatmap but with treatment as side color bar
cols = c("#00BFC4","#C77CFF")
names(cols) <- levels(factor(Mapping$Treatment))
Mapping$colors_treat = cols[as.character(Mapping$Treatment)]

dev.new()
pdf(file="CHCheatmap-TimeSeries_Treatment.pdf",width=4,height=4, pointsize=8)

{gplots::heatmap.2(as.matrix(bee2), 
                   main = "Relative percentages clustering of microbiome CHC Honeybees",
                   srtCol = 90,
                   dendrogram = "row",
                   Rowv = dend,
                   Colv = "NA", # this to make sure the columns are not ordered
                   trace="none",          
                   margins =c(5,0.1),      
                   key.xlab = "Relative Percentage on Cuticle",
                   RowSideColors = Mapping$colors_treat, # to add nice colored strips        
                   col = mycolors)
  legend("topright", legend = levels(factor(Mapping$Treatment)), fill = cols)}

dev.off()
# Here we classify bees based on dendrogram clusters
# (note that results of this assignment have already been added to the initial metadata file and we used the assignment for the statistical analyses above)
Work <- c(rep("Nurse", each=41),rep("Forager transition", each=13), rep("Forager", each=24), rep("Nurse transition", each=15), rep("Newly emerged", each=15)) 
dendSampleOrder <- labels(dend) 
Work<-data.frame(dendSampleOrder,Work)
rownames(Work)<-Work[,1]

rownames(Mapping)<-Mapping$Sample_ID
Mapping3 <- merge(Mapping, Work, by=0)
Mapping3$Sample_ID
Mapping<-Mapping3[match(rownames(bee2), Mapping3$Sample_ID),]
Mapping

# Plot ordination with work categories
pl2 <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,color=Mapping$Work.x, shape=Mapping$Treatment),size=3) +
  coord_equal() +
  scale_color_manual(values = c("Newly emerged" = "#C9C9C9", "Nurse transition" = "#D8B465", "Nurse" = "#E69F00", "Forager transition" = "#7AAFAF", "Forager" = "#56B4E9")) +
  theme_bw() +
  labs(shape = "Treatment", color = "CHC-cluster") +
  theme(axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
pl2
ggsave(file="CHCordination-TimeSeries-CHCclassification.pdf",width=7,height=7, pointsize=8)
