library(adegenet)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(ggplot2)
library(reshape2)
#library(RStoolbox)
library(maps)
library(stringr)

# read metadata and genotype data

allsites=read.csv("metadata/dartersitelocations.csv",header=T)

sitedat=adegenet::read.fstat("genotype_files/arkrapt_CSUEcrag_allsites.dat")

#calculate heterozygosity

sitehet=Hs(sitedat)

sitehetlocs=allsites

sitehetlocs$het=sitehet[sitehetlocs$site]

sitehetlocs=sitehetlocs[-2,]

plot(sitehetlocs$long,sitehetlocs$lat,cex=sitehetlocs$het*5)

metalist=read.csv("~/Dropbox/rapture/ECRapture45/metalist.csv",header=F)
metalist$V2=rainbow(n=11)

my.colors <- c("#00FF2E", "#E8FF00", "#E800FF", "#002EFF", "#5D00FF","#00B9FF","#FF0000","#FF8B00")

metalist$V2[4]<-"#00B9FF"
metalist$V2[6]<-"#5D00FF"
metalist$V2[11]<-"#00B9FF"

sitehetlocs$col<-metalist$V2[match(sitehetlocs$meta,metalist$V1)]

USstreams=readOGR(dsn="mapping/USA_Rivers_and_Streams", layer="USA_Rivers_and_Streams")
x=c(-105,-94)
y=c(36,40)
xy <- cbind(x,y)
S <- SpatialPoints(xy)
bbox(S)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb)[1] == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

USstreams_clipped <- gClip(USstreams,darterbox)

plot(USstreams_clipped,lwd=0.1,col="gray")
#plot(ArkRiv,lwd=0.25,col="black",add=T)
#plot(CimRiv,lwd=0.25,col="black",add=T)
#plot(ChikRiv,lwd=0.25,col="black",add=T)
#plot(MedRiv,lwd=0.25,col="black",add=T)
#plot(SaltRiv,lwd=0.25,col="black",add=T)
#plot(NFNRiv,lwd=0.25,col="black",add=T)
#plot(SFNRiv,lwd=0.25,col="black",add=T)
#plot(WalnutCrk,lwd=0.25,col="black",add=T)
#plot(RattCrk,lwd=0.25,col="black",add=T)
#plot(IllRiv,lwd=0.25,col="black",add=T)
#plot(SpringRiv,lwd=0.25,col="black",add=T)
#plot(BigSandy,lwd=0.25,col="black",add=T)
#plot(Rush,lwd=0.25,col="black",add=T)
#plot(Fountain,lwd=0.25,col="black",add=T)
#plot(Chico,lwd=0.25,col="black",add=T)
#plot(BlkSq,lwd=0.25,col="black",add=T)
map('state',fill=FALSE,add=TRUE,lwd=2)
points(sitehetlocs$long,sitehetlocs$lat,bg=sitehetlocs$col,cex=sitehetlocs$het*7,pch=21)
legend(x=-97,y=39,legend=c(0.05,0.3),pch=c(21,21),pt.cex=c(0.05*7,0.3*7),title="Heterozygosity")


plot(x=NULL,y=NULL,xlim=c(-95,-94),ylim=c(35,37))
text(x=sitehetlocs$long,y=sitehetlocs$lat,labels=sitehetlocs$site)
plot(USstreams,lwd=1,add=TRUE)


streamdist=read.csv("mapping/DarterLengths.csv")
sitelocs=read.csv("metadata/sitelocs.csv")
indsites=read.csv("metadata/indsites_Ec12345.csv",header=T)
metalist=read.csv("metadata/metalist.csv",header=F)

chiklandscape=read.csv("mapping/landscape_data/chikaskiadata.csv")
chiklandscape$combo=paste(chiklandscape$site1,chiklandscape$site2)
cimlandscape=read.csv("mapping/landscape_data/cimarrondata.csv")
cimlandscape$combo=paste(cimlandscape$site1,cimlandscape$site2)
cololandscape=read.csv("mapping/landscape_data/Coloradodata.csv")
cololandscape$combo=paste(cololandscape$site1,cololandscape$site2)
arklandscape=read.csv("mapping/landscape_data/arkrattledata.csv")
arklandscape$combo=paste(arklandscape$site1,arklandscape$site2)
illlandscape=read.csv("mapping/landscape_data/illinoisdata.csv")
illlandscape$combo=paste(illlandscape$site1,illlandscape$site2)
medlandscape=read.csv("mapping/landscape_data/medicinedata.csv")
medlandscape$combo=paste(medlandscape$site1,medlandscape$site2)
nfnlandscape=read.csv("mapping/landscape_data/NFNlandscapedata.csv")
nfnlandscape$combo=paste(nfnlandscape$site1,nfnlandscape$site2)
sfnlandscape=read.csv("mapping/landscape_data/SFNlandscapedata.csv")
sfnlandscape$combo=paste(sfnlandscape$site1,sfnlandscape$site2)
saltlandscape=read.csv("mapping/landscape_data/saltforkdata.csv")
saltlandscape$combo=paste(saltlandscape$site1,saltlandscape$site2)
wallandscape=read.csv("mapping/landscape_data/walnutdata.csv")
wallandscape$combo=paste(wallandscape$site1,wallandscape$site2)

#calculate upstream distance (from furthest downstream site within each drainage) for each site

sitehetlocs$upstream=0
for (i in which(sitehetlocs$meta=="Colorado")){
site = sitehetlocs$site[i]
sitecomb1=paste(site,"WHC")
sitecomb2=paste("WHC",site)
ups=cololandscape$riverdist[which(cololandscape$combo == sitecomb1 | cololandscape$combo == sitecomb2)]
if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

for (i in which(sitehetlocs$meta=="Chikaskia")){
  site = sitehetlocs$site[i]
  sitecomb1=paste(site,"CHIK27")
  sitecomb2=paste("CHIK27",site)
  ups=chiklandscape$riverdist[which(chiklandscape$combo == sitecomb1 | chiklandscape$combo == sitecomb2)]
  if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

for (i in which(sitehetlocs$meta=="Cimarron")){
  site = sitehetlocs$site[i]
  sitecomb1=paste(site,"OKLA")
  sitecomb2=paste("OKLA",site)
  ups=cimlandscape$riverdist[which(cimlandscape$combo == sitecomb1 | cimlandscape$combo == sitecomb2)]
  if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

for (i in which(sitehetlocs$meta=="LowerArk" | sitehetlocs$meta=="Rattlesnake")){
  site = sitehetlocs$site[i]
  sitecomb1=paste(site,"KAWN")
  sitecomb2=paste("KAWN",site)
  ups=arklandscape$riverdist[which(arklandscape$combo == sitecomb1 | arklandscape$combo == sitecomb2)]
  if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

for (i in which(sitehetlocs$meta=="Illinois")){
  site = sitehetlocs$site[i]
  sitecomb1=paste(site,"PAHO")
  sitecomb2=paste("PAHO",site)
  ups=illlandscape$riverdist[which(illlandscape$combo == sitecomb1 | illlandscape$combo == sitecomb2)]
  if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

for (i in which(sitehetlocs$meta=="Medicine")){
  site = sitehetlocs$site[i]
  sitecomb1=paste(site,"MLR3")
  sitecomb2=paste("MLR3",site)
  ups=medlandscape$riverdist[which(medlandscape$combo == sitecomb1 | medlandscape$combo == sitecomb2)]
  if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

for (i in which(sitehetlocs$meta=="NFNinnescah")){
  site = sitehetlocs$site[i]
  sitecomb1=paste(site,"NFNTS")
  sitecomb2=paste("NFNTS",site)
  ups=nfnlandscape$riverdist[which(nfnlandscape$combo == sitecomb1 | nfnlandscape$combo == sitecomb2)]
  if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

for (i in which(sitehetlocs$meta=="SFNinnescah")){
  site = sitehetlocs$site[i]
  sitecomb1=paste(site,"NTNR1")
  sitecomb2=paste("NTNR1",site)
  ups=sfnlandscape$riverdist[which(sfnlandscape$combo == sitecomb1 | sfnlandscape$combo == sitecomb2)]
  if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

for (i in which(sitehetlocs$meta=="SaltFork")){
  site = sitehetlocs$site[i]
  sitecomb1=paste(site,"ZBAR1")
  sitecomb2=paste("ZBAR1",site)
  ups=saltlandscape$riverdist[which(saltlandscape$combo == sitecomb1 | saltlandscape$combo == sitecomb2)]
  if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

for (i in which(sitehetlocs$meta=="Walnut")){
  site = sitehetlocs$site[i]
  sitecomb1=paste(site,"NESS1")
  sitecomb2=paste("NESS1",site)
  ups=wallandscape$riverdist[which(wallandscape$combo == sitecomb1 | wallandscape$combo == sitecomb2)]
  if(length(ups)>0){sitehetlocs$upstream[i]=ups}else{sitehetlocs$upstream[i]=0}
}

# initial test for an effect of upstream distance

upslm=lm(het~upstream+meta,data=sitehetlocs)

#############landcover!
### note that you need to install the development version of FedData from GitHub
### install.packages("devtools")
### library(devtools)
### devtools::install_github("ropensci/FedData",force=TRUE)

library(knitr)
library(raster)
library(FedData)
library(rgdal)

site_df <- data.frame(site = sitelocs$SITE.NAME, 
                      lat = sitelocs$y, 
                      lon = sitelocs$x)

# create spatial point data frame
coordinates(site_df) <- ~lon + lat
proj4string(site_df) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

###delete nlcd tiff in home dir first?

nlcd_raster <- get_nlcd(site_df, 
                        label = 'categorial-extraction', 
                        year = 2011, 
                        extraction.dir = '.')

# reproject midpoint to raster's crs
sites <- spTransform(site_df, projection(nlcd_raster))

buffer_distance_meters <- 1000

# visualize buffered point and land cover data
buff_shp <- buffer(sites, buffer_distance_meters)
plot(buff_shp) # I will plot over this, but it sets the plotting extent
plot(nlcd_raster, add = TRUE)
plot(buff_shp, add = TRUE)

landcover <- raster::extract(nlcd_raster, sites, buffer = buffer_distance_meters)

landcover_proportions <- lapply(landcover, function(x) {
  counts_x <- table(x)
  proportions_x <- prop.table(counts_x)
  sort(proportions_x)
})
sort(unlist(landcover_proportions))


unique(unlist(landcover))

# forest + shrub = 41 42 43 52
# water + wetland = 11 90 95
# grassland = 71
# developed = 21 22 23 24
# crop = 81 82

sitehetlocs$fprop=rep(0,nrow(sitehetlocs))
sitehetlocs$wprop=rep(0,nrow(sitehetlocs))
sitehetlocs$gprop=rep(0,nrow(sitehetlocs))
sitehetlocs$dprop=rep(0,nrow(sitehetlocs))
sitehetlocs$cprop=rep(0,nrow(sitehetlocs))


for (value in sitehetlocs$site) {
  df=as.data.frame(table(landcover[match(value,sites$site)]))
  count_temp=sum(df$Freq)
  f1=if(length(subset(df,Var1==41)[,2]) == 0) 0 else (subset(df,Var1==41)[,2])
  f2=if(length(subset(df,Var1==42)[,2]) == 0) 0 else (subset(df,Var1==42)[,2])
  f3=if(length(subset(df,Var1==43)[,2]) == 0) 0 else (subset(df,Var1==43)[,2])
  f4=if(length(subset(df,Var1==44)[,2]) == 0) 0 else (subset(df,Var1==44)[,2])
  w1=if(length(subset(df,Var1==11)[,2]) == 0) 0 else (subset(df,Var1==11)[,2])
  w2=if(length(subset(df,Var1==90)[,2]) == 0) 0 else (subset(df,Var1==90)[,2])
  w3=if(length(subset(df,Var1==95)[,2]) == 0) 0 else (subset(df,Var1==95)[,2])
  g=if(length(subset(df,Var1==71)[,2]) == 0) 0 else (subset(df,Var1==71)[,2])
  d1=if(length(subset(df,Var1==21)[,2]) == 0) 0 else (subset(df,Var1==21)[,2])
  d2=if(length(subset(df,Var1==22)[,2]) == 0) 0 else (subset(df,Var1==22)[,2])
  d3=if(length(subset(df,Var1==23)[,2]) == 0) 0 else (subset(df,Var1==23)[,2])
  d4=if(length(subset(df,Var1==24)[,2]) == 0) 0 else (subset(df,Var1==24)[,2])
  c1=if(length(subset(df,Var1==81)[,2]) == 0) 0 else (subset(df,Var1==81)[,2])
  c2=if(length(subset(df,Var1==82)[,2]) == 0) 0 else (subset(df,Var1==82)[,2])
  sitehetlocs$fprop[match(value,sitehetlocs$site)]=(f1+f2+f3+f4)/count_temp
  sitehetlocs$wprop[match(value,sitehetlocs$site)]=(w1+w2+w3)/count_temp
  sitehetlocs$gprop[match(value,sitehetlocs$site)]=g/count_temp
  sitehetlocs$dprop[match(value,sitehetlocs$site)]=(d1+d2+d3)/count_temp
  sitehetlocs$cprop[match(value,sitehetlocs$site)]=(c1+c2)/count_temp
}

#write.csv(sitehetlocs,"~/Dropbox/CSUEcrag/sitehetlocs_landcov.csv")

sitehetlocs=read.csv("mapping/landscape_data/sitehetlocs_landcov.csv")

streamtypedat=read.csv("mapping/landscape_data/KSDarterStreamType.csv")

sitehetlocs$type=streamtypedat$Descrip[match(sitehetlocs$site,streamtypedat$site)]
sitehetlocs$type=str_replace(sitehetlocs$type,"ARTIFICIAL PATH","Perennial")
sitehetlocs$type=str_replace(sitehetlocs$type,"CANAL/DITCH","Perennial")

###FTN01 (JCC), BSY04(BSC), RCR06?(RC2), RCR07?(RSHC), 

sitehetlocs$type[which(sitehetlocs$meta=="Colorado" | sitehetlocs$meta=="SpringRiver")]="Perennial"

sitehetlocs$type[which(sitehetlocs$site=="JCC" | sitehetlocs$site=="BSC" | sitehetlocs$site=="RC2" | sitehetlocs$site=="RSHC")]="Intermittent"

library(MuMIn)

global_lm=lm(het~type+meta+upstream+fprop+wprop+gprop+dprop+cprop,data=sitehetlocs)
options(na.action = "na.fail")
dd=dredge(global_lm)

#### submetas

indsitesmeta=read.csv("~/Dropbox/rapture/ECRapture45/indsites_meta3.csv")

#remove hatchery sites
sitehetlocs_KSCOAR=sitehetlocs[-which(sitehetlocs$site=="HPSWA" | sitehetlocs$site=="BSQ" | sitehetlocs$site=="HCR"),]
#add correct metapopulation data
sitehetlocs_KSCOAR$submeta=indsitesmeta$meta[match(sitehetlocs_KSCOAR$site,indsitesmeta$site)]

### add dam data

sitehetlocs_KSCOAR$obstruct = ifelse(sitehetlocs_KSCOAR$site == "CAV1" | sitehetlocs_KSCOAR$site == "AMBER2" | sitehetlocs_KSCOAR$site == "NBSC" | sitehetlocs_KSCOAR$site == "NFN1" | sitehetlocs_KSCOAR$site == "NFNINNSF" | sitehetlocs_KSCOAR$site == "ZANG" | sitehetlocs_KSCOAR$site == "SFNR4" | sitehetlocs_KSCOAR$site == "TEASLY" | sitehetlocs_KSCOAR$site == "SFNR3","dammed","undammed")
sitehetlocs_KSCOAR$reservoir = ifelse(sitehetlocs_KSCOAR$site == "CHN2" |sitehetlocs_KSCOAR$site == "CHN3" |sitehetlocs_KSCOAR$site == "CHN4" | sitehetlocs_KSCOAR$site == "CHN5" | sitehetlocs_KSCOAR$site == "CHN6" | sitehetlocs_KSCOAR$site == "CHN7" | sitehetlocs_KSCOAR$site == "CHN8" | sitehetlocs_KSCOAR$site == "CHN9" | sitehetlocs_KSCOAR$site == "CHN10" | sitehetlocs_KSCOAR$site == "2008-24","reservoir","nores")

global_lm_KSCOAR_dam=lm(het~type+submeta+upstream+fprop+gprop+dprop+cprop+obstruct+reservoir+obstruct,data=sitehetlocs_KSCOAR)
dd_KSCOAR_dam=dredge(global_lm_KSCOAR_dam)

res = lm(het~submeta+dprop+reservoir,data=sitehetlocs_KSCOAR)

best=lm(het~submeta+dprop,data=sitehetlocs_KSCOAR)
second=lm(het~submeta+dprop+reservoir,data=sitehetlocs_KSCOAR)
third=lm(het~submeta+dprop+upstream,data=sitehetlocs_KSCOAR)
fourth=lm(het~submeta+dprop+gprop,data=sitehetlocs_KSCOAR)
fifth=lm(het~submeta,data=sitehetlocs_KSCOAR)
sixth=lm(het~submeta+dprop+upstream,data=sitehetlocs_KSCOAR)


full_modsel=data.frame(dd_KSCOAR_dam$df,dd_KSCOAR_dam$logLik,dd_KSCOAR_dam$AICc,dd_KSCOAR_dam$delta,dd_KSCOAR_dam$weight)

write.csv(full_modsel,"analysis_outputs/heterozygosity_modsel_fulltable_dam.csv")

### het distribution

plot(density(sitehetlocs_KSCOAR$het))
median(sitehetlocs_KSCOAR$het)
mode(sitehetlocs_KSCOAR$het)

##correlations btwn vars

#cor(sitehetlocs_KSCOAR[,c('upstream','fprop','gprop','dprop','cprop','iprop')])
cor(sitehetlocs_KSCOAR[,c('upstream','fprop','gprop','dprop','cprop')])

#vars by metapop
ggplot(data=sitehetlocs_KSCOAR,mapping=aes(x=submeta,y=upstream)) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3)
ggplot(data=sitehetlocs_KSCOAR,mapping=aes(x=submeta,y=fprop)) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3)
ggplot(data=sitehetlocs_KSCOAR,mapping=aes(x=submeta,y=gprop)) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3)
ggplot(data=sitehetlocs_KSCOAR,mapping=aes(x=submeta,y=cprop)) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3)
ggplot(data=sitehetlocs_KSCOAR,mapping=aes(x=submeta,y=dprop)) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3)
