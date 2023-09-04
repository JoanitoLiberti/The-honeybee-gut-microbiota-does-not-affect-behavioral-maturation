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

### Input CHC data and metadata
newbee<-read.csv("WeightGain_CHCdata.csv", row.names = 1)
newbee<-newbee[,-1]
newbee<-t(newbee)

id<-read.table("IDs_WeightGain_CHCs.txt", header = T, fill = T )
id$Hive<-as.factor(id$Hive)
id$Box<-as.factor(id$Box)
id$HiveBox<-as.factor(id$HiveBox)

# Order data table by sample ID in the metadata so we make sure they have the same order
newbee <- newbee[id$Sample_ID,,drop=FALSE]

##Heatmap
###calculate clustering method and create dendrogram
dbee<-dist(newbee,method="euclidean")
hbee<-stats:::hclust(dbee,method="ward.D2")

dend<-as.dendrogram(hbee)

dend<-dendextend:::rotate(dend,1:182)

dendextend:::labels_colors(dend) <-
  rainbow_hcl(2)[dendextend:::sort_levels_values(
    as.numeric(as.factor(id$Treatment))[order.dendrogram(dend)]
  )]


dend <- dendextend:::hang.dendrogram(dend,hang_height=0.1)

dend <- dendextend:::set(dend, "labels_cex", 0.5)
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Honeybee CHC-Microbiota: Euclidean distances with Ward's Hierarchical clustering:Ward_2", 
     horiz =  TRUE,  nodePar = list(cex = .007))

microbio_levels <- c("CL","MD")
legend("topleft", legend = microbio_levels, fill = rainbow_hcl(2))

cols = c("#00BFC4","#C77CFF")
id$Cols <- cols[as.numeric(factor(id$Treatment))] 
# Define a palette of orange shades for the heatmap
nb.cols <- 100
mycolors <- colorRampPalette(brewer.pal(9, "Oranges"))(nb.cols)

dev.new()
pdf(file="Heatmap_weightgainCHCexp.pdf",width=4,height=4, pointsize=8)
{gplots::heatmap.2(as.matrix(newbee), 
                   main = "Relative percentages clustering of microbiome CHC Honeybees",
                   srtCol = 90,
                   dendrogram = "row",
                   Rowv = dend,
                   Colv = "NA", # this to make sure the columns are not ordered
                   trace="none",          
                   margins =c(5,0.1),      
                   key.xlab = "Relative Percentage on Cuticle",
                   #denscol = "grey",
                   #density.info = "density",
                   RowSideColors = id$Cols, # to add nice colored strips        
                   col = mycolors)
  legend("topright", legend = microbio_levels, fill = cols)}
dev.off()

# Here we derive who is forager and who is nurse based on clustering of CHCs and add a column to the metadata 
NandF <- c(rep("forager", each=89),rep ("nurse", each=93)) 
dendSampleOrder <- labels(dend) 
NandF<-data.frame(dendSampleOrder,NandF)
rownames(NandF)<-NandF[,1]

rownames(id)<-id$Sample_ID
id2 <- merge(id, NandF, by=0)
id2$Sample_ID
id<-id2[match(rownames(newbee), id2$Sample_ID),]

# Count the number of nurses and foragers in each treatment
dplyr::count(id, Experiment, Hive, Treatment, NandF) 

pl1 <- ggplot(subset(id, !is.na(NandF)), 
              aes(x = factor(Treatment, levels = c("MD", "CL")), 
                  fill = factor(NandF, levels=c("nurse", "forager")))) + 
  geom_bar(position = "fill") +
  ylab("Percentage of bees") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = scales::percent) +
  scale_fill_manual(values = c('nurse' = '#E69F00', 'forager' = '#56B4E9'))+
  facet_grid(.~Experiment + Hive, scales = "free_x")+
  guides(fill="none")+
  theme_minimal()
pl1
# ggsave(height=3,width=3,dpi=300, filename="PropNandFbyTreatment-WeightGain.pdf", useDingbats=FALSE)


pol <- id[id$Experiment == "Pollen", ]
bre <- id[id$Experiment == "BeeBread", ]

# Statistical analyses - distribution of N and F
library(lmerTest)
resul <- glmer(factor(NandF) ~ Treatment + (1|Hive:HiveBox), family=binomial, data = pol)
summary(resul)

resul <- glmer(factor(NandF) ~ Treatment + (1|Hive:HiveBox), family=binomial, data = bre)
summary(resul)
