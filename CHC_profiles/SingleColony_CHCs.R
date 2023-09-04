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
newbee<-read.csv("SingleColony_CHCdata.csv", row.names = 1)
newbee<-newbee[,-c(1:2)]
newbee<-t(newbee)
newbee <- as.matrix(newbee)/rowSums(as.matrix(newbee)) # Transform into proportions

id<-read.table("IDs_SingleColony_CHCs.txt", header = T )
id<-id[id$Sample_ID!="C74",] # Sample C74 did not work on the GC-MS so we discard it from the metadata
id$Cage<-as.factor(id$Cage)
id$Day<-as.factor(id$Day)

# Order data table by sample ID in the metadata so we make sure they have the same order
newbee <- newbee[id$Sample_ID,,drop=FALSE]

set.seed(64527) # Make analyses reproducible

# NMDS in classic R
mds1 <- metaMDS(newbee, distance="bray", k=2, trymax=100, autotransform=TRUE, noshare=0.1, expand=TRUE, trace=1)
ordiplot(mds1, type="text", display = "sites")

# in ggplot 
## create table for NMDS X and Y values of chemical compounds
data.scores<-as.data.frame(scores(mds1)$sites)
data.scores$site<-rownames(data.scores)
data.scores
## create table for NMDS X and Y values of individuals
species.scores<-as.data.frame(scores(mds1,"species"))
species.scores$species<-rownames(species.scores)
species.scores

##Heatmap
###calculate clustering method and create dendrogram
dbee<-dist(newbee,method="euclidean")
hbee<-stats:::hclust(dbee,method="ward.D2")

dend<-as.dendrogram(hbee)

dend<-dendextend:::rotate(dend,1:118)

cols = c("#00BFC4","#C77CFF")

labels_colors(dend) <-
  cols[sort_levels_values(
    as.numeric(as.factor(id$Treatment))[order.dendrogram(dend)]
  )]


dend <- hang.dendrogram(dend,hang_height=0.1)

dend <- dendextend:::set(dend, "labels_cex", 0.5)
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Honeybee CHC-Microbiota: Euclidean distances with Ward's Hierarchical clustering:Ward_2", 
     horiz =  TRUE,  nodePar = list(cex = .007))

microbio_levels <- c("CL","MD")
legend("topleft", legend = microbio_levels, fill = cols)


id$Cols <- cols[as.numeric(factor(id$Treatment))] 

# Define a palette of orange shades for the heatmap
nb.cols <- 100
mycolors <- colorRampPalette(brewer.pal(9, "Oranges"))(nb.cols)

dev.new()
pdf(file="Heatmap_SingleColonyCHCexp.pdf",width=6,height=5, pointsize=8)
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


# Calculate cage centroids 
beta <- betadisper(vegdist(newbee, "bray"), id$Cage)
cage_centroids<- beta$centroids + abs(min(beta$centroids)) # We add a constant to make values positive for Bray Curtis dissimilarities

# Metadata by cage
idTest <- unique(id[c("Cage","Treatment")])

# We make sure the order of the columns in the metadata matches that of the CHC data
idTest <- idTest[ order(match(idTest$Cage, rownames(cage_centroids))), ]

# Stats
set.seed(7586)

adonis2(cage_centroids ~ idTest$Treatment) # main effect

adonis2(newbee ~ id$Day + id$Cage + id$Day:id$Treatment) # other effects


# Here we derive who is forager and who is nurse based on clustering of CHCs and add a column to the metadata 
NandF <- c(rep("nurse", each=68),rep ("forager", each=50)) 
dendSampleOrder <- labels(dend) 
NandF<-data.frame(dendSampleOrder,NandF)
rownames(NandF)<-NandF[,1]

rownames(id)<-id$Sample_ID
id2 <- merge(id, NandF, by=0)
id2$Sample_ID
id<-id2[match(rownames(newbee), id2$Sample_ID),]

# Count the number of nurses and foragers in each treatment
dplyr::count(id, Day, Treatment, NandF) 

p1 <- ggplot(subset(id, !is.na(NandF)), 
             aes(x = factor(Treatment, levels = c("MD", "CL")), 
                 fill = factor(NandF, levels=c("nurse", "forager")))) + 
  geom_bar(position = "fill") +
  ylab("Percentage of bees") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = scales::percent) +
  scale_fill_manual(values = c('nurse' = '#E69F00', 'forager' = '#56B4E9'))+
  facet_grid(.~Day, scales = "free_x")+
  labs(
    fill = "CHC-cluster",
    x = NULL) +
  theme_minimal()

p1
ggsave(height=3,width=3,dpi=300, filename="PropNandFbyTreatment-SingleColonyExp.pdf", useDingbats=FALSE)

# Statistical analyses - distribution of N and F 
library(lmerTest)
resul <- glmer(factor(NandF) ~ Day*Treatment + (1|Cage), family=binomial, data = id)
summary(resul)

# Plot the nmds again with caste as shape
dev.new()
pdf(file="nmds_SingleColonyCHCexp.pdf",width=6,height=6, pointsize=8)
ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,color=id$Treatment,shape=id$NandF),size=2) + 
  coord_equal() +
  theme_bw() +
  facet_wrap(.~id$Day, ncol = 1)+
  scale_color_manual(values = c("#00BFC4","#C77CFF")) +
  theme(axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) 
dev.off()
