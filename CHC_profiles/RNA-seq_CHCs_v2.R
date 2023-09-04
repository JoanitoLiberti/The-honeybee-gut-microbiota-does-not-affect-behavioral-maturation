library(vegan)
library(ggplot2)
library(reshape2)
library(MASS)
library(adegenet)
library(dendextend)
library(colorspace)
library(gplots)
library(RColorBrewer)
library(edgeR)

setwd("/Volumes/gr_Engel/lab_resources/Manuscripts/2023_Liberti_Maturation/Code/CHC_profiles/")

# Load CHC data
newbee<-read.csv("RNAseq_CHCdata.csv",row.names = 1)
newbee
bee2<-newbee[,-c(1:3)]
bee2<-t(bee2/100)

id<-read.csv("IDs_RNAseq_CHCs.csv")

# Count the number of nurses and foragers in each treatment
dplyr::count(id, Treatment, NandF) 

id2 <- id[id$Treatment != "Hive", ]   

#Graph without hive bees
beenohive<-newbee[,-c(1:63)]/100
beenohive2 <- removeBatchEffect(beenohive, id2$Batch)

set.seed(98510)
mds2 <- metaMDS(t(beenohive2), distance="euclidean", k=2, trymax=100, autotransform=TRUE, noshare=0.1, expand=TRUE, trace=1)
ordiplot(mds2, type="text", display = "sites")
# in ggplot
## create table for NMDS X and Y values of chemical compounds
data.scores2<-as.data.frame(scores(mds2))
data.scores2$site<-rownames(data.scores2)
data.scores2
## create table for NMDS X and Y values of individuals
species.scores2<-as.data.frame(scores(mds2,"species"))
species.scores2$species<-rownames(species.scores2)
species.scores2

id2$Treatment <- factor(id2$Treatment, levels = c("MD", "CL_Bifi", "CL_13", "CL"))

#Graph
dev.new()
pdf(file="nmds_RNAseqExp-CHCs.pdf",width=5,height=5, pointsize=8)
ggplot() + 
  geom_point(data=data.scores2,aes(x=NMDS1,y=NMDS2,color=id2$Treatment,shape=id2$NandF),size=2) +
  coord_equal() +
  theme_bw() +
  xlim(-0.28, 0.33) +
  ylim(-0.25, 0.2) +
  scale_color_manual(values = c("#C77CFF","#F8766D","#7CAE00","#00BFC4"))+
  theme(axis.title.x = element_text(size=10), # remove x-axis labels
        axis.title.y = element_text(size=10), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
dev.off()

##Heatmap
###calculate clustering method and create dendrogram
dbee<-dist(bee2,method="euclidean")
hbee<-stats:::hclust(dbee,method="ward.D2")

dend<-as.dendrogram(hbee)

dend<-dendextend::rotate(dend,1:180)

id$Treatment<-as.factor(id$Type)

labels_colors(dend) <-
  c("#ADADAD","#F01686","#2396B4")[sort_levels_values(
    as.numeric(id$Treatment)[order.dendrogram(dend)]
  )]

dend <- hang.dendrogram(dend,hang_height=0.1)

dend <- dendextend::set(dend, "labels_cex", 0.5)
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Honeybee CHC-Microbiota: Euclidean distances with Ward's Hierarchical clustering:Ward_2", 
     horiz =  TRUE,  nodePar = list(cex = .007))

type_levels <- c("Gnotobiotic bees","Conventional nurses","Conventional foragers")
legend("topleft", legend = type_levels, fill = c("#ADADAD","#2396B4","#F01686"))

# Heatmap
cols = c("#F01686","#2396B4","#ADADAD")
id$cols<-cols[as.numeric(factor(id$Type))]
nb.cols <- 100
mycolors <- colorRampPalette(brewer.pal(9, "Oranges"))(nb.cols)

dev.new()
pdf(file="Heatmap_RNAseqExp-CHCs.pdf",width=6,height=5, pointsize=8)
{gplots::heatmap.2(as.matrix(bee2), 
                  main = "Relative percentages clustering of microbiome CHC Honeybees",
                  srtCol = 90,
                  dendrogram = "row",
                  Rowv = dend,
                  Colv = "NA", # this to make sure the columns are not ordered
                  trace="none",          
                  margins =c(5,0.1),      
                  key.xlab = "Relative Percentage on Cuticle",
                  denscol = "grey",
                  density.info = "density",
                  RowSideColors = id$cols, # to add nice colored strips        
                  col = mycolors)
legend("topright", legend = type_levels, fill = c("#ADADAD","#2396B4","#F01686"))}
dev.off()


# Calculate cage centroids 
beta <- betadisper(vegdist(t(beenohive), "bray"), id2$Box)
cage_centroids<- beta$centroids + abs(min(beta$centroids)) # We add a constant to make values positive for Bray Curtis dissimilarities

# Metadata by cage
idTest <- unique(id2[c("Box","Hive","Treatment")])

# We make sure the order of the columns in the metadata matches that of the CHC data
idTest <- idTest[ order(match(idTest$Box, rownames(cage_centroids))), ]

##Stats 
set.seed(7586)

adonis2(cage_centroids ~ idTest$Treatment + idTest$Hive) # main effect

adonis2(t(beenohive) ~ id2$Hive + id2$Hive:id2$Treatment) # other effects


# Here we derive who is forager and who is nurse based on dendrogram clustering of CHC profiles and add a column to the metadata 
# (note that results of this assignment have already been added to the initial metadata file and we used the assignment for the statistical analyses on the distribution of N and F by treatment, see above)
NandF <- c(rep("forager", each=74),rep ("nurse", each=106)) 
dendSampleOrder <- labels(dend) 
NandF<-data.frame(dendSampleOrder,NandF)
colnames(NandF)[which(names(NandF) == "dendSampleOrder")] <- "Sample_ID"

setdiff(id$Sample_ID,NandF$Sample_ID)

id3 <- merge(id, NandF, by="Sample_ID")

# Statistical analyses - distribution of N and F by treatment
library(lmerTest)
id2$Treatment <- factor(id2$Treatment)
id2$Treatment <- relevel(id2$Treatment, ref = "MD")
resul <- glmer(factor(NandF) ~ Treatment + (1|Hive/Box), family=binomial, data = id2)
summary(resul)
