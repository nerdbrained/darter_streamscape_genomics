
## read genotype data
## -------> get this file!!
genos = read.table("arkrapt_CSUEcrag_prunedfin.t.lfmm")
genos$site=sitef2_prune$site
genos$meta=sitef2_prune$meta

genosort=genos[with(genos,order(genos$meta,genos$site)),]
write.csv(genosort,"arkrapt_CSUEcrag_prunefinsort.csv")

library(adegenet)
library(hierfstat)

allmeta=hierfstat::read.fstat("arkrapt_CSUEcrag_metarem_head.dat")
pwise.WC=pairwise.WCfst(allmeta)
pwise.nei=pairwise.neifst(allmeta)
plot(pwise.WC[upper.tri(pwise.WC)],pwise.nei[upper.tri(pwise.nei)])
write.table(pwise.WC,"metapop_WCFST",row.names=FALSE,col.names=FALSE)
write.table(pwise.nei,"metapop_neiFST",row.names=FALSE,col.names=FALSE)

laremmeta=hierfstat::read.fstat("arkrapt_CSUEcrag_metalarem_head.dat")
pwise.WC=pairwise.WCfst(laremmeta)

pwise.nei=pairwise.neifst(laremmeta)
plot(pwise.WC[upper.tri(pwise.WC)],pwise.nei[upper.tri(pwise.nei)])
write.table(pwise.WC,"metapop_larem_WCFST",row.names=FALSE,col.names=FALSE)
write.table(pwise.nei,"metapop_larem_neiFST",row.names=FALSE,col.names=FALSE)

### fst heatmap
metafst=read.table("/Users/nerdbrained/Desktop/CSUEcrag/metapop_larem_neiFST")

names(metafst)=c("Big Sandy + Rush Creeks","Chikaskia River","Cimarron River","Illinois River","Lower Arkansas River","Medicine Lodge River","Middle Arkansas River","North Fork Ninnescah River","Rattlesnake Creek","Salt Fork Arkansas River","South Fork Ninnescah River","Spring River","Upper Arkansas River","Walnut Creek")
rownames(metafst)=c("Big Sandy + Rush Creeks","Chikaskia River","Cimarron River","Illinois River","Lower Arkansas River","Medicine Lodge River","Middle Arkansas River","North Fork Ninnescah River","Rattlesnake Creek","Salt Fork Arkansas River","South Fork Ninnescah River","Spring River","Upper Arkansas River","Walnut Creek")

library(reshape2)

fstmat=as.matrix(metafst)

melted_metafst=melt(fstmat)

head(melted_metafst)

library(ggplot2)

ggplot(data=melted_metafst,aes(x=Var1,y=Var2,fill=value)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", 
                       limit = c(0,1), space = "Lab", 
                       name="FST") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

cormat <- reorder_cormat(fstmat)
upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", 
                       limit = c(0,1), space = "Lab", 
                       name="FST") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)