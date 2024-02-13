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

###CSV: BeeTracking data.csv
newbee<-read.csv("BeeTracking_CHCdata.csv", row.names=1, check.names = F)
bee2<-newbee[,-c(1:4)]

bee2<-bee2/100
bee2<-t(bee2)

id<-read.csv("IDs_BeeTracking_CHCs.csv")
id <- id[id$Sample_ID %in% rownames(bee2),]
id$Replicate<-factor(id$Replicate)

# Count the number of nurses and foragers in each treatment
dplyr::count(id, Treatment, NandF) 
dplyr::count(id, Replicate, Treatment) 

p1 <- ggplot(subset(id, !is.na(NandF)), 
              aes(x = factor(Treatment, levels=c("MD", "CL")), 
                  fill = factor(NandF, levels=c("Nurse", "Forager")))) + 
  geom_bar(position = "fill") +
  ylab("Percentage of bees") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = scales::percent) +
  scale_fill_manual(values = c('Nurse' = '#E69F00', 'Forager' = '#56B4E9'))+
  facet_grid(.~Replicate)+
  labs(
    fill = "Sub-caste",
    x = NULL) +
  theme_minimal()

p1
ggsave(height=3,width=7,dpi=300, filename="PropNandFbyTreatment-BeeTracking.pdf", useDingbats=FALSE)

# Statistical analyses - distribution of N and F by treatment
library(lmerTest)
resul <- glmer(factor(NandF) ~ Treatment + (1|Replicate:Cage), family=binomial, data = id)
summary(resul)

set.seed(98510)
# ordination
mds1 <- metaMDS(bee2, distance="bray", k=3, trymax=100, autotransform=TRUE, noshare=0.1, expand=TRUE, trace=1)
ordiplot(mds1, type="text", display = "sites")

## create table for NMDS X and Y values of chemical compounds
data.scores<-as.data.frame(scores(mds1)$sites)
data.scores$site<-rownames(data.scores)
data.scores

## create table for NMDS X and Y values of individuals
species.scores<-as.data.frame(scores(mds1,"species"))
species.scores$species<-rownames(species.scores)
species.scores

# We make sure the order of the columns (Sample_IDs) in the metadata matches that of the ordination scores
id.scores <- id[ order(match(id$Sample_ID, rownames(data.scores))), ]

#Graph
pl1 <- ggplot() + 
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,color=id.scores$Treatment,shape=id.scores$NandF),size=2) +
  coord_equal() +
  theme_bw() +
  # facet_wrap(.~id$Replicate)+
  ylim(-0.6, 0.6) +
  xlim(-0.9, 0.7) +
  scale_color_manual(values = c("MD" = "#C77CFF", "CL" = "#00BFC4")) +
  labs(color = "Treatment", shape = "Sub-caste") +
  theme(axis.title.x = element_text(size=10), # remove x-axis labels
        axis.title.y = element_text(size=10), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
pl1

ggsave(file="nmds_BeeTracking-CHCs.pdf",width=5,height=5, pointsize=8)

##Heatmap
###calculate clustering method
library(dendextend)
library(colorspace)
dbee<-dist(bee2,method="euclidean")
hbee<-hclust(dbee,method="ward.D2")

dend<-as.dendrogram(hbee)

dend<-dendextend::rotate(dend,1:177)

cols = c("#00BFC4","#C77CFF")

labels_colors(dend) <- cols[
    as.numeric(factor(id$Treatment))[order.dendrogram(dend)]
  ]

dend <- hang.dendrogram(dend,hang_height=0.1)

dend <- dendextend::set(dend, "labels_cex", 0.5)
dend %>% dendextend::set("branches_lwd", 1)

par(mar = c(3,3,3,7))
plot(dend,  
     main = "Honeybee CHC-Microbiota: Euclidean distances with Ward's Hierarchical clustering:Ward_2", 
     horiz =  TRUE,  nodePar = list(cex = .007))

microbio_levels <- c("MD","CL")
legend("topleft", legend = microbio_levels, fill = c("#C77CFF","#00BFC4"))

# unscaled heatmap
library(gplots)
library(RColorBrewer)

microlab<-cols[as.numeric(factor(id$Treatment))]

dev.new()
pdf(file="CHCheatmap-BeeTracking.pdf",width=4,height=4, pointsize=8)

library(RColorBrewer)
nb.cols <- 100
mycolors <- colorRampPalette(brewer.pal(9, "Oranges"))(nb.cols)
{gplots::heatmap.2(as.matrix(bee2), 
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
                  RowSideColors = microlab, # to add nice colored strips        
                  col = mycolors)
                  #breaks=colorcode)
legend("topright", legend = microbio_levels, fill = c("#C77CFF","#00BFC4"))}
dev.off()


# Calculate cage centroids 
beta <- betadisper(vegdist(bee2, "bray"), id$Cage)
cage_centroids<- beta$centroids + abs(min(beta$centroids)) # We add a constant to make values positive for Bray Curtis dissimilarities

# Metadata by cage
idTest <- unique(id[c("Cage","Replicate","Treatment")])

# We make sure the order of the columns in the metadata matches that of the CHC data
idTest <- idTest[ order(match(idTest$Cage, rownames(cage_centroids))), ]

##Stats full model
set.seed(7586)

adonis2(cage_centroids ~ idTest$Treatment + idTest$Replicate) # main effect

adonis2(bee2 ~ id$Replicate + id$Replicate:id$Treatment) # other effects


# Here we derive who is forager and who is nurse based on dendrogram clustering of CHC profiles and add a column to the metadata 
# (note that results of this assignment have already been added to the initial metadata file and we used the assignment for the statistical analyses on the distribution of N and F by treatment, see above)
NandF <- c(rep("Forager", each=37),rep ("Nurse", each=140)) 
dendSampleOrder <- labels(dend) 
NandF<-data.frame(dendSampleOrder,NandF)
colnames(NandF)[which(names(NandF) == "dendSampleOrder")] <- "Sample_ID"

setdiff(id$Sample_ID,NandF$Sample_ID)

id2 <- merge(id, NandF, by="Sample_ID")
