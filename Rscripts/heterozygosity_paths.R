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

allsites=read.csv("~/Dropbox/kansasGIS/dartersitelocations.csv",header=T)

sitedat=adegenet::read.fstat("~/Dropbox/CSUEcrag/arkrapt_CSUEcrag_allsites.dat")

sitehet=Hs(sitedat)

#siteinb=inbreeding(sitedat,res.type="estimate")
### not right - fix sitef2 order
#sitef2_prune$inb=siteinb

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

USstreams=readOGR(dsn="~/Dropbox/kansasGIS/USA_Rivers_and_Streams", layer="USA_Rivers_and_Streams")
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


streamdist=read.csv("~/Dropbox/rapture/landscape_manus/pwisefsts/DarterLengths.csv")
sitelocs=read.csv("~/Dropbox/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Dropbox/rapture/ECRapture45/indsites_Ec12345.csv",header=T)
metalist=read.csv("~/Dropbox/rapture/ECRapture45/metalist.csv",header=F)

chiklandscape=read.csv("~/Dropbox/kansasGIS/chikaskiadata.csv")
chiklandscape$combo=paste(chiklandscape$site1,chiklandscape$site2)
cimlandscape=read.csv("~/Dropbox/kansasGIS/cimarrondata.csv")
cimlandscape$combo=paste(cimlandscape$site1,cimlandscape$site2)
cololandscape=read.csv("~/Dropbox/kansasGIS/Coloradodata.csv")
cololandscape$combo=paste(cololandscape$site1,cololandscape$site2)
arklandscape=read.csv("~/Dropbox/kansasGIS/arkrattledata.csv")
arklandscape$combo=paste(arklandscape$site1,arklandscape$site2)
illlandscape=read.csv("~/Dropbox/kansasGIS/illinoisdata.csv")
illlandscape$combo=paste(illlandscape$site1,illlandscape$site2)
medlandscape=read.csv("~/Dropbox/kansasGIS/medicinedata.csv")
medlandscape$combo=paste(medlandscape$site1,medlandscape$site2)
nfnlandscape=read.csv("~/Dropbox/kansasGIS/NFNlandscapedata.csv")
nfnlandscape$combo=paste(nfnlandscape$site1,nfnlandscape$site2)
sfnlandscape=read.csv("~/Dropbox/kansasGIS/SFNlandscapedata.csv")
sfnlandscape$combo=paste(sfnlandscape$site1,sfnlandscape$site2)
saltlandscape=read.csv("~/Dropbox/kansasGIS/saltforkdata.csv")
saltlandscape$combo=paste(saltlandscape$site1,saltlandscape$site2)
wallandscape=read.csv("~/Dropbox/kansasGIS/walnutdata.csv")
wallandscape$combo=paste(wallandscape$site1,wallandscape$site2)


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

write.csv(sitehetlocs,"~/Dropbox/CSUEcrag/sitehetlocs_landcov.csv")

sitehetlocs=read.csv("~/Dropbox/CSUEcrag/sitehetlocs_landcov.csv")

streamtypedat=read.csv("~/Dropbox/rapture/landscape_manus/hets/KSDarterStreamType.csv")

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

lump=model.avg(dd,delta=1)

best_lm=lm(het~meta+upstream,data=sitehetlocs)
second_lm=lm(het~meta,data=sitehetlocs)
third_lm=lm(het~meta+fprop,data=sitehetlocs)
fourth_lm=lm(het~meta+wprop+upstream,data=sitehetlocs)
fifth_lm=lm(het~meta+dprop+upstream,data=sitehetlocs)
sixth_lm=lm(het~meta+wprop,data=sitehetlocs)

### KS only

sitehetlocs_KS=sitehetlocs[-which(sitehetlocs$meta=="Colorado" | sitehetlocs$meta=="SpringRiver"),]
sitehetlocs$type=str_replace(sitehetlocs$type,"ARTIFICIAL PATH","Perennial")
sitehetlocs$type=str_replace(sitehetlocs$type,"CANAL/DITCH","Perennial")


global_lm_KS=lm(het~type+meta+upstream+fprop+wprop+gprop+dprop+cprop,data=sitehetlocs_KS)
dd_KS=dredge(global_lm_KS)

best_lm=lm(het~meta+dprop,data=sitehetlocs_KS)

### KS + CO

sitehetlocs_KSCO=sitehetlocs[-which(sitehetlocs$meta=="SpringRiver" | sitehetlocs$site=="HPSWA" | sitehetlocs$site=="BSQ" | sitehetlocs$site=="HCR"),]

global_lm_KSCO=lm(het~type+meta+upstream+fprop+wprop+gprop+dprop+cprop,data=sitehetlocs_KSCO)
dd_KSCO=dredge(global_lm_KSCO)

best_lm=lm(het~meta+dprop,data=sitehetlocs_KSCO)
second_lm=lm(het~meta+dprop+wprop,data=sitehetlocs_KSCO)
third_lm=lm(het~meta+dprop+upstream,data=sitehetlocs_KSCO)


### no weird COLO

sitehetlocs_KSCOAR=sitehetlocs[-which(sitehetlocs$site=="HPSWA" | sitehetlocs$site=="BSQ" | sitehetlocs$site=="HCR"),]

global_lm_KSCOAR=lm(het~type+meta+upstream+fprop+gprop+dprop+cprop,data=sitehetlocs_KSCOAR)
dd_KSCOAR=dredge(global_lm_KSCOAR)

#best_lm=lm(het~meta+dprop,data=sitehetlocs_KSCOAR)
#second_lm=lm(het~meta+dprop+upstream,data=sitehetlocs_KSCOAR)
#third_lm=lm(het~meta+dprop+upstream+wprop,data=sitehetlocs_KSCOAR)
#fourth_lm=lm(het~meta+dprop+upstream+wprop+dprop,data=sitehetlocs_KSCOAR)
#fifth_lm=lm(het~meta+dprop+upstream+wprop+dprop,data=sitehetlocs_KSCOAR)
#sixth_lm=lm(het~meta+dprop+upstream+wprop+dprop,data=sitehetlocs_KSCOAR)

#### submetas

indsitesmeta=read.csv("~/Dropbox/rapture/ECRapture45/indsites_meta3.csv")


sitehetlocs_KSCOAR$submeta=indsitesmeta$meta[match(sitehetlocs_KSCOAR$site,indsitesmeta$site)]

global_lm_KSCOAR_submeta=lm(het~type+submeta+upstream+fprop++gprop+dprop+cprop,data=sitehetlocs_KSCOAR)
dd_KSCOAR_submeta=dredge(global_lm_KSCOAR_submeta)

best_lm=lm(het~submeta+dprop,data=sitehetlocs_KSCOAR)
second_lm=lm(het~submeta+dprop+upstream,data=sitehetlocs_KSCOAR)
third_lm=lm(het~submeta+dprop+fprop,data=sitehetlocs_KSCOAR)



### plus dam?

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

write.csv(full_modsel,"~/Desktop/ArkDarterManuscript/heterozygosity_modsel_fulltable_dam.csv")

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


### try a BMS? probably no? not immediately working with data... 

sitehetlocs4BMS=sitehetlocs_KSCOAR

head(sitehetlocs4BMS)

sitehetlocs4BMS$X.1=NULL
sitehetlocs4BMS$X=NULL
sitehetlocs4BMS$site=NULL
sitehetlocs4BMS$long=NULL
sitehetlocs4BMS$lat=NULL
sitehetlocs4BMS$col=NULL
sitehetlocs4BMS$meta=NULL

write.csv(sitehetlocs4BMS,"sitehetlocs4BMS.csv")

sitehetstan=data.frame((sitehetlocs4BMS$het-mean(sitehetlocs4BMS$het))/sd(sitehetlocs4BMS$het))
names(sitehetstan)[1]="Heterozygosity"
sitehetstan$StreamType=sitehetlocs4BMS$type
sitehetstan$Metapopulation=sitehetlocs4BMS$submeta
sitehetstan$Upstream=(sitehetlocs4BMS$upstream-mean(sitehetlocs4BMS$upstream))/sd(sitehetlocs4BMS$upstream)
sitehetstan$Forest=(sitehetlocs4BMS$fprop-mean(sitehetlocs4BMS$fprop))/sd(sitehetlocs4BMS$fprop)
sitehetstan$Grassland=(sitehetlocs4BMS$gprop-mean(sitehetlocs4BMS$gprop))/sd(sitehetlocs4BMS$gprop)
sitehetstan$Croplan=(sitehetlocs4BMS$cprop-mean(sitehetlocs4BMS$cprop))/sd(sitehetlocs4BMS$cprop)
sitehetstan$Developed=(sitehetlocs4BMS$dprop-mean(sitehetlocs4BMS$dprop))/sd(sitehetlocs4BMS$dprop)
sitehetstan$Dams=sitehetlocs4BMS$obstruct
sitehetstan$Reservoir=sitehetlocs4BMS$reservoir

install.packages("/Users/nerdbrained/Downloads/BMS_0.3.4.tar.gz",repo=NULL,type="source")

library(BMS)

sitehet_bms=bms(X.data=sitehetstan,iter=10000,mprior="uniform")
summary(sitehetstan_bms)
coef(sitehetstan_bms)
image(sitehetstan_bms,cex=1.14,cex.lab=1.2,col=c("blueviolet","cadetblue2"))