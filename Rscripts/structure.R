## load packages
library(LEA)
library(RColorBrewer)
library(reshape2)
library(plyr)
library(mapplots)
library(maps)
library(raster)
library(rgdal)
library(rgeos)


## wrangle metadata
###load files
sitenames=read.csv("metadata/indsites_meta3.csv")
indos=read.table("metadata/bamlistRapt_IDs.txt",header=FALSE)
keepers=read.table("metadata/arkrapt_CSUEcrag_filt8_unlinked_indkeep.csv",header=FALSE)
allsites=read.csv("metadata/dartersitelocations.csv",header=T)
sitelocs=read.csv("metadata/sitelocs.csv")

###prune to individuals used in final dataset
keepind=indos[which(keepers > 0),]
sitef2=sitenames[match(keepind,sitenames$ind),]
weirdind=c(2684,2703,2732,1925,2481,2483,2056,2071,2015,1608,1626,1795,1284,2972,2212,1335,1607,1908)
match(weirdind,sitef2$ind)
hugo=c(125,126,127,128,129)
match(hugo,sitef2$ind)

sitef2_prune=sitef2[-na.omit(match(weirdind,sitef2$ind)),]
sitef2_prune=subset(sitef2_prune,meta!="Hugo Ponds Hatchery Population")
sitef2_prune=subset(sitef2_prune,site!="HCR")
sitef2_prune=subset(sitef2_prune,site!="BSQ")

sitef2_prune$long=allsites$long[match(sitef2_prune$site,allsites$site)]

sitef2_prune$lat=allsites$lat[match(sitef2_prune$site,allsites$site)]
metalat=aggregate(sitef2_prune$lat,list(sitef2_prune$meta),FUN=mean)
metalat=metalat[order(metalat$x,decreasing=TRUE),]
metalat$order=c(1,2,4,3,5,6,7,8,9,10,11,13,12,14)
sitef2_prune$metalat=metalat$order[match(sitef2_prune$meta,metalat$Group.1)]

### mapping

USstreams=readOGR(dsn="mapping/USA_Rivers_and_Streams", layer="USA_Rivers_and_Streams")
x=c(-105,-94)
y=c(36,40)
xy <- cbind(x,y)
S <- SpatialPoints(xy)
bbox(S)

darterbox=bbox(S)

#gClip <- function(shp, bb){
#  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
#  else b_poly <- as(extent(bb), "SpatialPolygons")
#  gIntersection(shp, b_poly, byid = TRUE)
#}

#USstreams_clipped <- gClip(USstreams,darterbox)


### making barplot - populations arranged N to S 

neutQ<- read.table("analysis_outputs/arkrapt_CSUEcrag_prunedfin.t_r3.8.q")

neutQ$meta <- sitef2_prune$meta
neutQ$ind=sitef2_prune$ind
neutQ$metalat=sitef2_prune$metalat
neutQ$long<-sitef2_prune$long
Qsortlat=neutQ[order(neutQ$metalat,neutQ$long),]
sitecounts<-as.data.frame(dplyr::count(Qsortlat,meta))
sitecounts$metalat=metalat$order[match(sitecounts$meta,metalat$Group.1)]
sitecounts=sitecounts[order(sitecounts$metalat),]
sitecounts$delinpos<-cumsum(sitecounts$n)
sitecounts$labelpos<-(sitecounts$delinpos-(sitecounts$n/2))
Qsortlat$meta=NULL
Qsortlat$ind=NULL
Qsortlat$site=NULL
Qsortlat$metalat=NULL
Qsortlat$long=NULL


my.colors=brewer.pal(n=8,name="Set2")

my.colors=my.colors[c(1,2,3,6,5,4,8,7)]
#exchange 6 and 4
#exchange 7 and 8


par(mai=c(1.5,0.8,0.2,0.5),xpd=T)
barplot(t(Qsortlat),col=my.colors,space=0,border=NA,xlab="",xaxt="n",ylab="Admixture proportions")
axis(side=1, at=sitecounts$labelpos, labels = FALSE,lwd=0,lwd.ticks=1)
axis(side=1, at=c(0,1775), labels = FALSE,lwd=1,lwd.ticks=0)
abline(v=sitecounts$delinpos,col="white",lwd=1.5)
text(x=sitecounts$labelpos,y=-0.05,labels=sitecounts$meta,cex=0.8,adj=0,col="black",srt=315)



###### making pie chart map


neutQ<- read.table("analysis_outputs/arkrapt_CSUEcrag_prunedfin.t_r3.8.q")

neutQ$site <- sitef2_prune$site



aggQ <-ddply(neutQ,~site,summarise,p1=mean(V1),p2=mean(V2),p3=mean(V3),p4=mean(V4),p5=mean(V5),p6=mean(V6),p7=mean(V7),p8=mean(V8))

meltQ <- melt(aggQ)

meltQ$x=sitelocs$x[match(meltQ$site, sitelocs$SITE.NAME)]
meltQ$y=sitelocs$y[match(meltQ$site, sitelocs$SITE.NAME)]
neutxyz=make.xyz(x=meltQ$x,y=meltQ$y,z=meltQ$value,group=meltQ$variable)

#plot(USstreams_clipped,lwd=0.1,col="gray",xlim=c(-105,-94),ylim=c(36,40))

plot(USstreams,lwd=0.1,col="gray",xlim=c(-105,-94),ylim=c(36,40))

ArkRiv=USstreams[which(USstreams$NAME == "Arkansas River"),]
CimRiv=USstreams[which(USstreams$NAME == "Cimarron River"),]
ChikRiv=USstreams[which(USstreams$NAME == "Chikaskia River"),]
MedRiv=USstreams[which(USstreams$NAME == "Medicine Lodge River"),]
SaltRiv=USstreams[which(USstreams$NAME == "Salt Fork Arkansas River"),]
NFNRiv=USstreams[which(USstreams$NAME == "North Fork Ninnescah River"),]
SFNRiv=USstreams[which(USstreams$NAME == "South Fork Ninnescah River"),]
WalnutCrk=USstreams[which(USstreams$NAME == "Walnut Creek"),]
RattCrk=USstreams[which(USstreams$NAME == "Rattlesnake Creek"),]
IllRiv=USstreams[which(USstreams$NAME == "Illinois River"),]
SpringRiv=USstreams[which(USstreams$NAME == "Spring River"),]
BigSandy=USstreams[which(USstreams$NAME == "Big Sandy Creek"),]
Rush=USstreams[which(USstreams$NAME == "Rush Creek"),]
Fountain=USstreams[which(USstreams$NAME == "Fountain Creek"),]
Chico=USstreams[which(USstreams$NAME == "Chico Creek"),]
BlkSq=USstreams[which(USstreams$NAME == "Black Squirrel Creek"),]


plot(ArkRiv,lwd=0.25,col="black",add=T)
plot(CimRiv,lwd=0.25,col="black",add=T)
plot(ChikRiv,lwd=0.25,col="black",add=T)
plot(MedRiv,lwd=0.25,col="black",add=T)
plot(SaltRiv,lwd=0.25,col="black",add=T)
plot(NFNRiv,lwd=0.25,col="black",add=T)
plot(SFNRiv,lwd=0.25,col="black",add=T)
plot(WalnutCrk,lwd=0.25,col="black",add=T)
plot(RattCrk,lwd=0.25,col="black",add=T)
plot(IllRiv,lwd=0.25,col="black",add=T)
plot(SpringRiv,lwd=0.25,col="black",add=T)
plot(BigSandy,lwd=0.25,col="black",add=T)
plot(Rush,lwd=0.25,col="black",add=T)
plot(Fountain,lwd=0.25,col="black",add=T)
plot(Chico,lwd=0.25,col="black",add=T)
plot(BlkSq,lwd=0.25,col="black",add=T)

map('state',fill=FALSE,add=TRUE,lwd=2,col="lightblue")
draw.pie(x=neutxyz$x,y=neutxyz$y,z=neutxyz$z,radius=rep(0.08,240),col=my.colors)
