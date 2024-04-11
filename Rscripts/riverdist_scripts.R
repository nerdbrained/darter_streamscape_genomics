# scripts used for calculating river distance and stream/landscape characteristics along river paths

# lots of large external GIS datasets used and lots of fiddling with river networks needed to do this - contact nerdbrained@gmail.com if you want explanation/help with these. 

library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(maps)

dam=readOGR(dsn="~/Desktop/kansasGIS/KansasDarters",layer="Kansas_Dams")

stream=readOGR(dsn="~/Desktop/kansasGIS/KansasDarters",layer="KansasStreams")

x=c(-100.8,-96.9)
y=c(36.9,38.6)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

KSstreams_clipped <- gClip(stream,darterbox)

sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]

#plot(KSstreams_clipped,lwd=0.1)
points(indsites$long,indsites$lat,col=indsites$col,pch=19,cex=0.5,add=T)




latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))
streamUTM<- spTransform(stream,crs)
noind=indsites[,2:5]
uniquesites=unique(noind)
pts<-data.frame(sitelocs$x,sitelocs$y)
sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
ptsUTM<- spTransform(sppts,crs)

streamUTM$GNIS_Name[which.min(gDistance(ptsUTM[117,],streamUTM,byid=TRUE))]

ArkRiv=stream[which(stream$GNIS_Name == "Arkansas River" | stream$GNIS_Name == "Cow Creek" | stream$GNIS_Name == "Spring Creek" | stream$GNIS_Name == "Peace Creek" | stream$GNIS_Name == "Walnut Creek"),]
CimRiv=stream[which(stream$GNIS_Name == "Cimarron River" | stream$GNIS_Name == "Bluff Creek" | stream$GNIS_Name == "Cavalry Creek" | stream$GNIS_Name == "Crooked Creek" | stream$GNIS_Name == "Kiowa Creek" | stream$GNIS_Name == "Snake Creek" | stream$GNIS_Name == "Big Sandy Creek"),]
##is weird!
ChikRiv=stream[which(stream$GNIS_Name == "North Fork Chikaskia River" | stream$GNIS_Name == "Duck Creek" | stream$GNIS_Name == "Sandy Creek" | stream$GNIS_Name == "Spring Creek" | stream$GNIS_Name == "East Sand Creek" | stream$GNIS_Name == "Argonia Creek" | stream$GNIS_Name == "Wild Horse Creek" | stream$GNIS_Name == "Sand Creek"),]
##
MedRiv=stream[which(stream$GNIS_Name == "Medicine Lodge River" | stream$GNIS_Name == "Amber Creek" | stream$GNIS_Name == "Dog Creek" | stream$GNIS_Name == "Elm Creek" | stream$GNIS_Name == "Stolp Creek" | stream$GNIS_Name == "Antelope Creek" | stream$GNIS_Name == "North Branch Elm Creek" | stream$GNIS_Name == "North Elm Creek" | stream$GNIS_Name == "Crooked Creek" | stream$GNIS_Name == "Soldier Creek" | stream$GNIS_Name == "Turkey Creek" | stream$GNIS_Name == "East Branch South Elm Creek"),]
NFNRiv=stream[which(stream$GNIS_Name == "North Fork Ninnescah River" | stream$GNIS_Name == "Smarsh Creek" | stream$GNIS_Name == "Giefer Creek" | stream$GNIS_Name == "Goose Creek" | stream$GNIS_Name == "Red Rock Creek" | stream$GNIS_Name == "Silver Creek" | stream$GNIS_Name == "Wolf Creek"),]
SFNRiv=stream[which(stream$GNIS_Name == "South Fork Ninnescah River" | stream$GNIS_Name == "Coon Creek" | stream$GNIS_Name == "Natrona Creek" | stream$GNIS_Name == "Ninnescah River" | stream$GNIS_Name == "Painter Creek" | stream$GNIS_Name == "Sand Creek" | stream$GNIS_Name == "Spring Creek" | stream$GNIS_Name == "Smoots Creek"),]
SaltRiv=stream[which(stream$GNIS_Name == "Salt Fork Arkansas River" | stream$GNIS_Name == "Nescatunga Creek" | stream$GNIS_Name == "Indian Creek" | stream$GNIS_Name == "Mule Creek"),]
WalCrk=stream[which(stream$GNIS_Name == "Walnut Creek" | stream$GNIS_Name == "North Fork Walnut Creek"),]
RattCrk=stream[which(stream$GNIS_Name == "Rattlesnake Creek"),]
SlateCrk=stream[which(stream$GNIS_Name == "Slate Creek"),]


KSrast = raster(ncols=190,nrows=450,xmn=-101,xmx=-96.5,ymn=36.8,ymx=38.7)
ArkRast = rasterize(ArkRiv,KSrast)
CimRast = rasterize(CimRiv,KSrast)
ChikRast = rasterize(ChikRiv,KSrast)
MedRast = rasterize(MedRiv,KSrast)
NFNRast = rasterize(NFNRiv,KSrast)
SFNRast = rasterize(SFNRiv,KSrast)
SaltRast = rasterize(SaltRiv,KSrast)
WalRast = rasterize(WalCrk,KSrast)
RattRast = rasterize(RattCrk,KSrast)
SlateRast = rasterize(SlateCrk,KSrast)

plot(indsites$long,indsites$lat,col=indsites$col,pch=19,cex=1,xlim=c(-101,-96.5),ylim=c(36.8,38.7))

plot(ArkRast,add=T)
plot(CimRast,add=T)
plot(ChikRast,add=T)
plot(MedRast,add=T)
plot(NFNRast,add=T)
plot(SFNRast,add=T)
plot(SaltRast,add=T)
plot(WalRast,add=T)
plot(RattRast,add=T)
plot(SlateRast,add=T)


require(rgdal)

# The input file geodatabase
fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
summary(fc)

x=c(-101,-96)
y=c(36,39)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)

# View the feature class
plot(fc_clipped,lwd=0.2)

#cimarron 1 = [9]
#cimarron 2 = [51]
#cimarron 3 = [60]

#salt fork = [11]

#chikaskia = [13]

#medicine = [61]

#walnut = [40]

#rattle = [42]
#ark above Rattle = [37]
#furthest Lower Ark = [10]
#ark below rattle = [43]
#stray ark point? = [44]
#stray ark point? = [45]
#ark and slate = [46]

#NFNinnescah = [47]
#western SFN = [48]
#eastern SFN = [21]

############ produce files for riverdist
######## saltfork - done!

saltfork_watershed=fc_clipped[11]
plot(saltfork_watershed)

SaltFork_crop=crop(stream,saltfork_watershed)

SaltFork_redcrop=SaltFork_crop[which(SaltFork_crop$GNIS_Name == "Salt Fork Arkansas River" | SaltFork_crop$GNIS_Name == "Nescatunga Creek" | SaltFork_crop$GNIS_Name == "East Branch Nescatunga Creek" | SaltFork_crop$GNIS_Name == "Indian Creek" |  SaltFork_crop$GNIS_Name == "Mule Creek"),]


#df<-SpatialLinesDataFrame(SaltFork_crop, data.frame(id=row.names(SaltFork_crop),row.names=row.names(SaltFork_crop)))
writeOGR(SaltFork_redcrop, dsn="~/Desktop/kansasGIS/SaltFork_streams/" ,layer="SaltFork_redcrop",driver="ESRI Shapefile")


####### medicine - done!

medicine_watershed=fc_clipped[61]
plot(medicine_watershed)

Medicine_crop=crop(stream,medicine_watershed)

Medicine_redcrop=Medicine_crop[which(Medicine_crop$GNIS_Name == "Medicine Lodge River" | Medicine_crop$GNIS_Name == "Amber Creek" | Medicine_crop$GNIS_Name == "Dog Creek" | Medicine_crop$GNIS_Name == "Elm Creek" | Medicine_crop$GNIS_Name == "Stolp Creek" | Medicine_crop$GNIS_Name == "Antelope Creek" | Medicine_crop$GNIS_Name == "North Branch Elm Creek" | Medicine_crop$GNIS_Name == "North Elm Creek" | Medicine_crop$GNIS_Name == "Crooked Creek" | Medicine_crop$GNIS_Name == "Soldier Creek" | Medicine_crop$GNIS_Name == "Turkey Creek" | Medicine_crop$GNIS_Name == "East Branch South Elm Creek" |  Medicine_crop$GNIS_Name == "Little Bear Creek" |  Medicine_crop$GNIS_Name == "South Elm Creek" |  Medicine_crop$GNIS_Name == "Threemile Creek"|  Medicine_crop$GNIS_Name == "Short Creek" |Medicine_crop$OBJECTID == "485150" | Medicine_crop$OBJECTID == "431211" | Medicine_crop$OBJECTID == "147534" | Medicine_crop$OBJECTID == "287273" | Medicine_crop$OBJECTID == "494147" | Medicine_crop$OBJECTID == "48489" | Medicine_crop$OBJECTID == "79863"),]

pts<-data.frame(x=-98.552519,y=37.353309)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
ptsUTM<- spTransform(sppts,crs)

streamUTM$OBJECTID[which.min(gDistance(ptsUTM,streamUTM,byid=TRUE))]

streamUTM$GNIS_Name[which.min(gDistance(ptsUTM,streamUTM,byid=TRUE))]

writeOGR(Medicine_redcrop, dsn="~/Desktop/kansasGIS/Medicine_streams/" ,layer="Medicine_redcrop",driver="ESRI Shapefile")

####### chikaskia

chikaskia_watershed=fc_clipped[13]
plot(chikaskia_watershed)

Chikaskia_crop=crop(stream,chikaskia_watershed)

Chikaskia_redcrop=Chikaskia_crop[which(Chikaskia_crop$GNIS_Name == "North Fork Chikaskia River" | Chikaskia_crop$GNIS_Name == "Duck Creek" | Chikaskia_crop$GNIS_Name == "Sandy Creek" | Chikaskia_crop$GNIS_Name == "Spring Creek" | Chikaskia_crop$GNIS_Name == "East Sand Creek" | Chikaskia_crop$GNIS_Name == "Argonia Creek" | Chikaskia_crop$GNIS_Name == "Wild Horse Creek" | Chikaskia_crop$GNIS_Name == "Sand Creek" | Chikaskia_crop$GNIS_Name == "Rose Bud Creek"| Chikaskia_crop$OBJECTID == "363247" | Chikaskia_crop$OBJECTID == "327642" | Chikaskia_crop$OBJECTID == "399527"| Chikaskia_crop$OBJECTID == "179028" | Chikaskia_crop$OBJECTID == "512034" | Chikaskia_crop$OBJECTID == "201396" | Chikaskia_crop$OBJECTID == "530333" | Chikaskia_crop$OBJECTID == "7688" | Chikaskia_crop$OBJECTID == "21286" | Chikaskia_crop$OBJECTID == "552772" | Chikaskia_crop$OBJECTID == "7651"),]

#Missing so many!!

USstreams=readOGR(dsn="~/Desktop/kansasGIS/USA_Rivers_and_Streams", layer="USA_Rivers_and_Streams")
x=c(-104.83,-94.1)
y=c(36.08,39.52)
xy <- cbind(x,y)
S <- SpatialPoints(xy)
bbox(S)

ChikRiv=USstreams[which(USstreams$NAME == "Chikaskia River"),]
Chikproj=spTransform(ChikRiv,projection(nlcd_raster))
Chikbuff=buffer(Chikproj,width=100)

Chikcropproj=spTransform(Chikaskia_crop,projection(nlcd_raster))
Chikbuff=buffer(Chikproj,width=2000)

Chiklines <- intersect(Chikcropproj, Chikbuff)


Chiklinesadd <- spTransform(Chiklines,projection(Chikaskia_redcrop))

Chiklinesart=Chiklinesadd[which(Chiklinesadd$FCode=="55800"),]

Chiklinesperm=Chiklinesadd[which(Chiklinesadd$FCode=="46006"),]

Chiklinesall=Chiklinesadd[which(Chiklinesadd$FCode=="46006" | Chiklinesadd$FCode=="55800"),]



Chikaskia_redcrop2=rbind(Chikaskia_redcrop,Chiklinesall)

points(indsites$long,indsites$lat,col=indsites$col,pch=19,cex=0.5)
text(uniquesites$long,uniquesites$lat,labels=uniquesites$site,cex=0.3)

pts<-data.frame(x=uniquesites$long[which(uniquesites$site=="CHIK17")],y=uniquesites$lat[which(uniquesites$site=="CHIK17")])

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
ptsUTM<- spTransform(sppts,crs)

chikUTM$OBJECTID[which.min(gDistance(ptsUTM,chikUTM,byid=TRUE))]

chikUTM$GNIS_Name[which.min(gDistance(ptsUTM,chikUTM,byid=TRUE))]


writeOGR(Chikaskia_redcrop2, dsn="~/Desktop/kansasGIS/Chikaskia_streams/" ,layer="Chikaskia_redcrop",driver="ESRI Shapefile")


####### ninnescah DONE!!

nfn_watershed=fc_clipped[47]
sfn_watershed=fc_clipped[48]
nsfn_watershed=fc_clipped[21]

ninnescah_watershed=union(nfn_watershed,sfn_watershed)
ninnescah_watershed=union(ninnescah_watershed,nsfn_watershed)
ninnescah_watershed=gUnaryUnion(ninnescah_watershed)

plot(ninnescah_watershed)

Ninnescah_crop=crop(stream,ninnescah_watershed)

Ninnescah_redcrop=Ninnescah_crop[which(Ninnescah_crop$GNIS_Name == "North Fork Ninnescah River" | Ninnescah_crop$GNIS_Name == "Smarsh Creek" | Ninnescah_crop$GNIS_Name == "Giefer Creek" | Ninnescah_crop$GNIS_Name == "Goose Creek" | Ninnescah_crop$GNIS_Name == "Red Rock Creek" | Ninnescah_crop$GNIS_Name == "Silver Creek" | Ninnescah_crop$GNIS_Name == "Wolf Creek" | Ninnescah_crop$GNIS_Name == "South Fork Ninnescah River" | Ninnescah_crop$GNIS_Name == "Coon Creek" | Ninnescah_crop$GNIS_Name == "Natrona Creek" | Ninnescah_crop$GNIS_Name == "Ninnescah River" | Ninnescah_crop$GNIS_Name == "Painter Creek" | Ninnescah_crop$GNIS_Name == "Sand Creek" | Ninnescah_crop$GNIS_Name == "Spring Creek" | Ninnescah_crop$GNIS_Name == "Smoots Creek" | Ninnescah_crop$GNIS_Name == "Clearwater Creek" | Ninnescah_crop$OBJECTID == "356613" | Ninnescah_crop$OBJECTID == "259162" | Ninnescah_crop$OBJECTID == "92477" | Ninnescah_crop$OBJECTID == "223183" | Ninnescah_crop$OBJECTID == "473554" | Ninnescah_crop$OBJECTID == "403489"  | Ninnescah_crop$OBJECTID == "68643"  | Ninnescah_crop$OBJECTID == "518819" | Ninnescah_crop$OBJECTID == "194654" | Ninnescah_crop$OBJECTID == "496087" | Ninnescah_crop$OBJECTID == "410875" |  Ninnescah_crop$OBJECTID == "27970"| Ninnescah_crop$OBJECTID == "221491" | Ninnescah_crop$OBJECTID == "199099" | Ninnescah_crop$OBJECTID == "451396" | Ninnescah_crop$OBJECTID == "451106" | Ninnescah_crop$OBJECTID == "469074" | Ninnescah_crop$OBJECTID == "77506" | Ninnescah_crop$OBJECTID == "410887" | Ninnescah_crop$OBJECTID == "564130" | Ninnescah_crop$OBJECTID == "163248" |Ninnescah_crop$OBJECTID == "19027" | Ninnescah_crop$OBJECTID == "50580" | Ninnescah_crop$OBJECTID == "293978" | Ninnescah_crop$OBJECTID == "482757" | Ninnescah_crop$OBJECTID == "194655" |Ninnescah_crop$OBJECTID == "555089" | Ninnescah_crop$OBJECTID == "307420" | Ninnescah_crop$OBJECTID == "298482" | Ninnescah_crop$OBJECTID == "95648" | Ninnescah_crop$OBJECTID == "415405" | Ninnescah_crop$OBJECTID == "365579" | Ninnescah_crop$OBJECTID == "370048" | Ninnescah_crop$OBJECTID == "226049" | Ninnescah_crop$OBJECTID == "352183" | Ninnescah_crop$OBJECTID == "257772" | Ninnescah_crop$OBJECTID == "149709" | Ninnescah_crop$OBJECTID == "559562" | Ninnescah_crop$OBJECTID == "226042" | Ninnescah_crop$OBJECTID == "36943" | Ninnescah_crop$OBJECTID == "208184" | Ninnescah_crop$OBJECTID == "36963" | Ninnescah_crop$OBJECTID == "163247" | Ninnescah_crop$OBJECTID == "19021" | Ninnescah_crop$OBJECTID == "496362" | Ninnescah_crop$OBJECTID == "482774" | Ninnescah_crop$OBJECTID == "568646" | Ninnescah_crop$OBJECTID == "122530" | Ninnescah_crop$OBJECTID == "235122" | Ninnescah_crop$OBJECTID == "525125" | Ninnescah_crop$OBJECTID == "155611" | Ninnescah_crop$OBJECTID == "56844" | Ninnescah_crop$OBJECTID == "398581" | Ninnescah_crop$OBJECTID == "56845" | Ninnescah_crop$OBJECTID == "155611" | Ninnescah_crop$OBJECTID == "196275" | Ninnescah_crop$OBJECTID == "538704" | Ninnescah_crop$OBJECTID == "295584" | Ninnescah_crop$OBJECTID == "309064" | Ninnescah_crop$OBJECTID == "245631" | Ninnescah_crop$OBJECTID == "475282" | Ninnescah_crop$OBJECTID == "105901" | Ninnescah_crop$OBJECTID == "581445" | Ninnescah_crop$OBJECTID == "581579" | Ninnescah_crop$OBJECTID == "581613" | Ninnescah_crop$OBJECTID == "479963" | Ninnescah_crop$OBJECTID == "218711" | Ninnescah_crop$OBJECTID == "110564" | Ninnescah_crop$OBJECTID == "223166" | Ninnescah_crop$OBJECTID == "78877" | Ninnescah_crop$OBJECTID == "291019" | Ninnescah_crop$OBJECTID == "300161" | Ninnescah_crop$OBJECTID == "371747" | Ninnescah_crop$OBJECTID == "236868" | Ninnescah_crop$OBJECTID == "236511" | Ninnescah_crop$OBJECTID == "232188" | Ninnescah_crop$OBJECTID == "430492" | Ninnescah_crop$OBJECTID == "416983" | Ninnescah_crop$OBJECTID == "340536" | Ninnescah_crop$OBJECTID == "119651" | Ninnescah_crop$OBJECTID == "469087" | Ninnescah_crop$OBJECTID == "415396"),]

#Cheneys: 
CHN2 = "307420" + "149709"
*CHN3 = "298482" + "208184" + "163247" + "235122"
*CHN4 = "95648" + "19021" + "496362" + "482774"
CHN5 = "415405" + "36943"  + "36963"
CHN6 = "365579" + "226042"
CHN7 = "370048"
CHN8 = "226049" + "559562"
CHN9 = "352183"
*2008-24 = "257772" + "568646" + "122530"

#BWWAs
BWWA1 = "525125"
BWWA2 = "155611"
BWWA3 = "56844"
BWWA4 = "398581"
*BWWA5 = "56845" + "105901" + "581445" + "581579" + "581613" + "479963" + "78877" + "300161"
BWWA6 = "155611"
BWWA7 = "196275"
*BWWA8 = "538704" + "430492" + "416983" + "340536"
*BWWA9 = "295584" + "119651"
BWWA11 = "309064"
*BWWA12 = "245631" + "371747"
*KDOT1 = "475282" + "218711" + "110564" + "223166" + "291019"
#TNFNHWY50 = "473554" + "518819" (missing 1 link!) + "469087" + "415396"


writeOGR(Ninnescah_redcrop, dsn="~/Desktop/kansasGIS/Ninnescah_streams/" ,layer="Ninnescah_redcrop",driver="ESRI Shapefile")


#### cimarron DONE!
cimarron1=fc_clipped[9]
cimarron2=fc_clipped[51]
cimarron3=fc_clipped[60]

cimarron_watershed=union(cimarron1,cimarron2)
cimarron_watershed=union(cimarron_watershed,cimarron3)
cimarron_watershed=gUnaryUnion(cimarron_watershed)

Cimarron_crop=crop(stream,cimarron_watershed)

Cimarron_redcrop=Cimarron_crop[which(Cimarron_crop$GNIS_Name == "Cimarron River" | Cimarron_crop$GNIS_Name == "Bluff Creek" | Cimarron_crop$GNIS_Name == "Cavalry Creek" | Cimarron_crop$GNIS_Name == "Crooked Creek" | Cimarron_crop$GNIS_Name == "Kiowa Creek" | Cimarron_crop$GNIS_Name == "Snake Creek" | Cimarron_crop$GNIS_Name == "Big Sandy Creek" | Cimarron_crop$GNIS_Name == "Sand Creek" ),]

writeOGR(Cimarron_redcrop, dsn="~/Desktop/kansasGIS/Cimarron_streams/" ,layer="Cimarron_redcrop",driver="ESRI Shapefile")

#### Walnut - done!

walnut_watershed = fc_clipped[40]
Walnut_crop=crop(stream,walnut_watershed)

Walnut_redcrop=Walnut_crop[which(Walnut_crop$GNIS_Name == "Walnut Creek" | Walnut_crop$GNIS_Name == "North Fork Walnut Creek" | Walnut_crop$GNIS_Name == "Middle Fork Walnut Creek"),]

writeOGR(Walnut_redcrop, dsn="~/Desktop/kansasGIS/Walnut_streams/" ,layer="Walnut_redcrop",driver="ESRI Shapefile")

########  ark and rattle

rattle = fc_clipped[42]
upperark = fc_clipped[37]
lowerark = fc_clipped[10]
middleark = fc_clipped[43]
strayark1 = fc_clipped[44]
strayark2 = fc_clipped[45]
slate = fc_clipped[46]

arkrattle_watershed=union(rattle,upperark)
arkrattle_watershed=union(arkrattle_watershed,lowerark)
arkrattle_watershed=union(arkrattle_watershed,middleark)
arkrattle_watershed=union(arkrattle_watershed,slate)
arkrattle_watershed=union(arkrattle_watershed,strayark1)
arkrattle_watershed=union(arkrattle_watershed,strayark2)

x=c(-99,-96.9)
y=c(37,38.4)
xy <- cbind(x,y)
S <- SpatialPoints(xy)


tempbox=bbox(S)

tempclip <- gClip(arkrattle_watershed,tempbox)
arkrattle_watershed=gUnaryUnion(tempclip)

#Arkrattle_crop=crop(stream,arkrattle_watershed)

Arkrattle_redcrop=Arkrattle_crop[which(Arkrattle_crop$GNIS_Name == "Arkansas River" | Arkrattle_crop$GNIS_Name == "Cow Creek" | Arkrattle_crop$GNIS_Name == "Spring Creek" | Arkrattle_crop$GNIS_Name == "Peace Creek" | Arkrattle_crop$GNIS_Name == "Walnut Creek" | Arkrattle_crop$GNIS_Name == "Rattlesnake Creek" | Arkrattle_crop$GNIS_Name == "Slate Creek" | Arkrattle_crop$GNIS_Name == "North Branch State Creek" | Arkrattle_crop$GNIS_Name == "Salt Creek" | Arkrattle_crop$OBJECTID == "160368" | Arkrattle_crop$OBJECTID == "232215" | Arkrattle_crop$OBJECTID == "16224" | Arkrattle_crop$OBJECTID == "470801" | Arkrattle_crop$OBJECTID == "389795" | Arkrattle_crop$OBJECTID == "128665" | Arkrattle_crop$OBJECTID == "421467"),]



points(indsites$long,indsites$lat,col=indsites$col,pch=19,cex=0.5)
text(uniquesites$long,uniquesites$lat,labels=uniquesites$site,cex=0.5)

library(maptools)

midpts=getSpatialLinesMidPoints(Arkrattle_crop)


x=c(-98.6,-98.47)
y=c(38.12,38.25)
xy <- cbind(x,y)
S <- SpatialPoints(xy)


tempbox=bbox(S)

tempclip <- gClip(Arkrattle_crop,tempbox)

tempclip2 <- gClip(Arkrattle_redcrop,tempbox)

plot(tempclip)
plot(tempclip2,col="red",lwd=1.5,add=T)
text(uniquesites$long,uniquesites$lat,labels=uniquesites$site,cex=0.5)

text(midpts,labels=Arkrattle_crop$OBJECTID,cex=0.5)

pts<-data.frame(x=uniquesites$long[which(uniquesites$site=="QNWR4")],y=uniquesites$lat[which(uniquesites$site=="QNWR4")])




#pts<-data.frame(x=-98.335847,y=37.978925)


sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
ptsUTM<- spTransform(sppts,crs)

streamUTM$OBJECTID[which.min(gDistance(ptsUTM,streamUTM,byid=TRUE))]

streamUTM$GNIS_Name[which.min(gDistance(ptsUTM,streamUTM,byid=TRUE))]

streamUTM$ReachCode[which.min(gDistance(ptsUTM,streamUTM,byid=TRUE))]

#connectingQNWRs

bark=Arkrattle_crop[which(Arkrattle_crop$OBJECTID == "421467"),]@lines[[1]]

QNWR3pt=bark@Lines[[1]]@coords[51,] 

lin45=as.matrix(rbind(QNWR5pt,QNWR4pt))
splin45=spLines(lin45,crs=crs(Arkrattle_redcrop))

lin37=as.matrix(rbind(QNWR3pt,QNWR7pt))
splin37=spLines(lin37,crs=crs(Arkrattle_redcrop))

lin47=as.matrix(rbind(QNWR4pt,QNWR7pt))
splin47=spLines(lin47,crs=crs(Arkrattle_redcrop))

lin67=as.matrix(rbind(QNWR6pt,QNWR7pt))
splin67=spLines(lin67,crs=crs(Arkrattle_redcrop))

lincon7=as.matrix(rbind(conpt,QNWR7pt))
splincon7=spLines(lincon7,crs=crs(Arkrattle_redcrop))

nulin=rbind(splin45,splin37,splin47,splin67,splincon7)

OBJECTID = seq(1,5)
FCode= rep("46003",5)
Permanent_ = rep(NA,5)
FDate = rep(NA,5) 
Resolution = rep(NA,5)
GNIS_ID = rep(NA,5)
GNIS_Name = rep(NA,5)
LengthKM = rep(NA,5)
ReachCode = rep (NA,5)
FlowDir = rep(NA,5)
WBArea_Per = rep(NA,5)
FType = rep(NA,5)
MainPath = rep(NA,5)
InNetwork = rep(NA,5)
Visibility = rep(NA,5)
Shape_Leng = rep(NA,5)

nulindata=data.frame(OBJECTID,Permanent_,FDate,Resolution,GNIS_ID,GNIS_Name,LengthKM,ReachCode,FlowDir,WBArea_Per,FType,FCode,MainPath,InNetwork,Visibility,Shape_Leng)

nulindf <- SpatialLinesDataFrame(sl = nulin, data = nulindata, match.ID = FALSE)

Arkrattle_redcrop2=rbind(Arkrattle_redcrop,nulindf)

writeOGR(Arkrattle_redcrop2, dsn="~/Desktop/kansasGIS/Arkrattle_streams/" ,layer="Arkrattle_redcrop",driver="ESRI Shapefile")


#######


utmStr <- "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

library(riverdist)

MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/SaltFork_streams", layer="SaltFork_redcrop",reproject=utmStr)

#Fcode field is important (55800 = art path, 46003 = intermittent, 46006 = perennial, 46007 = ephemeral)

cleanup(MyRivernetwork)->SaltForkClean

salts=indsites[which(indsites$meta=="Salt Fork Arkansas River"),]
salts$ind=NULL
saltlocs=unique(saltlocs)[,3:4]

latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

crs <- CRS(sprintf(utmStr, 32))

saltpts<-data.frame(saltlocs$long,saltlocs$lat)
saltsppts<-SpatialPoints(saltpts,proj4string=CRS(latlong))
saltUTM<- spTransform(saltsppts,crs)
salties=coordinates(saltUTM)
saltiesfixed=salties[-1,]
saltiesfixed=saltiesfixed[-2,]

salt_riv <- xy2segvert(x=saltiesfixed[,1], y=saltiesfixed[,2], rivers=SaltForkClean)

zoomtoseg(seg=c(3), rivers=SaltForkClean)
points(salties[,1], salties[,2], pch=16, col="red")
riverpoints(seg=salt_riv$seg, vert=salt_riv$vert, rivers=SaltForkClean, pch=15, 
            col="blue")

riverpoints(seg=salt_riv$seg, vert=salt_riv$vert, rivers=SaltForkClean)->saltrivpoints

dmat <- riverdistancemat(salt_riv$seg,salt_riv$vert,SaltForkClean)
round(dmat)[1:7,1:7]

saltpair=salt_riv[1:2,]
saltpair$pair="TestPair"

homerange(unique=saltpair$pair, seg=saltpair$seg, vert=saltpair$vert, rivers=SaltForkClean) -> pairseg

pairseg[2]

###plotting the pairwise line    
for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0) 
                  lines(pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]), 
                    ],lwd=10)
            }
        }
        
        
        
###for a pair of points, extracting the pairwise river path with buffer and dams / stream type / landcover

library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(riverdist)
library(knitr)
library(FedData)

utmStr <- "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))

#load NLCD

fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
summary(fc)

x=c(-101,-96)
y=c(36,39)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)

#salt fork = [11]

saltfork_watershed=fc_clipped[11]

nlcd_raster <- get_nlcd(saltfork_watershed,label = 'categorical-extraction',year = 2016,extraction.dir = '.')

SaltFork_redcrop=readOGR(dsn="~/Desktop/kansasGIS/SaltFork_streams/" ,layer="SaltFork_redcrop")

saltfork_watershed_reproj=spTransform(saltfork_watershed, projection(nlcd_raster))
saltfork_river_reproj=spTransform(SaltFork_redcrop, projection(nlcd_raster))
saltfork_artpath=saltfork_river_reproj[which(saltfork_river_reproj$FCode=="55800"),]
saltfork_perennial=saltfork_river_reproj[which(saltfork_river_reproj$FCode=="46006"),]
saltfork_intermit=saltfork_river_reproj[which(saltfork_river_reproj$FCode=="46003"),]

dam=readOGR(dsn="~/Desktop/kansasGIS/KansasDarters",layer="Kansas_Dams")

dam_reproj=spTransform(dam, projection(nlcd_raster))


#MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/SaltFork_streams", layer="SaltFork_redcrop",reproject=utmStr)

#cleanup(MyRivernetwork)->SaltForkClean

load(file="/Users/nerdbrained/Desktop/kansasGIS/SaltForkClean.rda")

sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]


salts=indsites[which(indsites$meta=="Salt Fork Arkansas River"),]
salts$ind=NULL
saltsunique=unique(salts)

saltpts<-data.frame(saltsunique$long,saltsunique$lat)
saltsppts<-SpatialPoints(saltpts,proj4string=CRS(latlong))
saltUTM<- spTransform(saltsppts,crs)
saltpts_reproj<- spTransform(saltsppts,projection(nlcd_raster))


saltsitecombs=combn(saltsunique$site,m=2)
saltlongcombs=combn(saltsunique$long,m=2)
saltlatcombs=combn(saltsunique$lat,m=2)

landscapedata=data.frame(saltsitecombs[1,],saltsitecombs[2,],saltlongcombs[1,],saltlongcombs[2,],saltlatcombs[1,],saltlatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=SaltForkClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=SaltForkClean) -> pairseg

#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                 linebuff=buffer(lineseg,width=100)
                  linebuff=buffer(lineseg,width=500)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)



perbuff<-gIntersection(saltfork_perennial,buff1trans)
intbuff<-gIntersection(saltfork_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

landscapedata$riverdist[i]=pairseg$ranges$range

}




#####trying Arkrattle - died with unconnected river segs (remove first?)

fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
summary(fc)

x=c(-101,-96)
y=c(36,39)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)

rattle = fc_clipped[42]
upperark = fc_clipped[37]
lowerark = fc_clipped[10]
middleark = fc_clipped[43]
strayark1 = fc_clipped[44]
strayark2 = fc_clipped[45]
slate = fc_clipped[46]

arkrattle_watershed=union(rattle,upperark)
arkrattle_watershed=union(arkrattle_watershed,lowerark)
arkrattle_watershed=union(arkrattle_watershed,middleark)
arkrattle_watershed=union(arkrattle_watershed,slate)
arkrattle_watershed=union(arkrattle_watershed,strayark1)
arkrattle_watershed=union(arkrattle_watershed,strayark2)

x=c(-99,-96.9)
y=c(37,38.4)
xy <- cbind(x,y)
S <- SpatialPoints(xy)


tempbox=bbox(S)

tempclip <- gClip(arkrattle_watershed,tempbox)
arkrattle_watershed=gUnaryUnion(tempclip)

#remove previous nlcd .tif file in home directory before running this
nlcd_raster <- get_nlcd(arkrattle_watershed,label = 'categorical-extraction',year = 2016,extraction.dir = '.')

#writeOGR(Arkrattle_redcrop2, dsn="~/Desktop/kansasGIS/Arkrattle_streams/" ,layer="Arkrattle_redcrop",driver="ESRI Shapefile")

Arkrattle_redcrop2=readOGR(dsn="~/Desktop/kansasGIS/Arkrattle_streams/" ,layer="Arkrattle_redcrop")

arkrattle_watershed_reproj=spTransform(arkrattle_watershed, projection(nlcd_raster))
arkrattle_river_reproj=spTransform(Arkrattle_redcrop2, projection(nlcd_raster))
arkrattle_artpath=arkrattle_river_reproj[which(arkrattle_river_reproj$FCode=="55800"),]
arkrattle_perennial=arkrattle_river_reproj[which(arkrattle_river_reproj$FCode=="46006"),]
arkrattle_intermit=arkrattle_river_reproj[which(arkrattle_river_reproj$FCode=="46003"),]
arkrattle_canal=arkrattle_river_reproj[which(arkrattle_river_reproj$FCode=="33600"),]

arkrattle_allperennial=arkrattle_river_reproj[which(arkrattle_river_reproj$FCode=="46006" | arkrattle_river_reproj$FCode=="55800" | arkrattle_river_reproj$FCode=="33600"),]

dam=readOGR(dsn="~/Desktop/kansasGIS/KansasDarters",layer="Kansas_Dams")

dam_reproj=spTransform(dam, projection(nlcd_raster))

utmStr <- "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))


#MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Arkrattle_streams", layer="Arkrattle_redcrop",reproject=utmStr)

#cleanup(MyRivernetwork)->ArkrattleClean

load(file="/Users/nerdbrained/Desktop/kansasGIS/ArkrattleClean.rda")

sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]


arks=indsites[which(indsites$meta=="Lower Arkansas River" | indsites$meta=="Rattlesnake Creek"),]
arks$ind=NULL
arksunique=unique(arks)

arkpts<-data.frame(arksunique$long,arksunique$lat)
arksppts<-SpatialPoints(arkpts,proj4string=CRS(latlong))
arkUTM<- spTransform(arksppts,crs)
arkpts_reproj<- spTransform(arksppts,projection(nlcd_raster))


arksitecombs=combn(arksunique$site,m=2)
arklongcombs=combn(arksunique$long,m=2)
arklatcombs=combn(arksunique$lat,m=2)

landscapedata=data.frame(arksitecombs[1,],arksitecombs[2,],arklongcombs[1,],arklongcombs[2,],arklatcombs[1,],arklatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=ArkrattleClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=ArkrattleClean) -> pairseg


#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                 linebuff=buffer(lineseg,width=100)
                  linebuff=buffer(lineseg,width=100)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)



perbuff<-gIntersection(arkrattle_allperennial,buff1trans)
intbuff<-gIntersection(arkrattle_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

landscapedata$riverdist[i]=pairseg$ranges$range

}



#####trying Cimarron -- fixed KC160 problem (change to not create line if there's only 1 point)

fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
summary(fc)

x=c(-101,-96)
y=c(36,39)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)


cimarron1=fc_clipped[9]
cimarron2=fc_clipped[51]
cimarron3=fc_clipped[60]

cimarron_watershed=union(cimarron1,cimarron2)
cimarron_watershed=union(cimarron_watershed,cimarron3)
cimarron_watershed=gUnaryUnion(cimarron_watershed)

#remove previous nlcd .tif file in home directory before running this
nlcd_raster <- get_nlcd(cimarron_watershed,label = 'categorical-extraction',year = 2016,extraction.dir = '.')


Cimarron_redcrop=readOGR(dsn="~/Desktop/kansasGIS/Cimarron_streams/" ,layer="Cimarron_redcrop")

cimarron_watershed_reproj=spTransform(cimarron_watershed, projection(nlcd_raster))
cimarron_river_reproj=spTransform(Cimarron_redcrop, projection(nlcd_raster))
cimarron_artpath=cimarron_river_reproj[which(cimarron_river_reproj$FCode=="55800"),]
cimarron_perennial=cimarron_river_reproj[which(cimarron_river_reproj$FCode=="46006"),]
cimarron_intermit=cimarron_river_reproj[which(cimarron_river_reproj$FCode=="46003"),]
cimarron_canal=cimarron_river_reproj[which(cimarron_river_reproj$FCode=="33400"),]

cimarron_allperennial=cimarron_river_reproj[which(cimarron_river_reproj$FCode=="46006" | cimarron_river_reproj$FCode=="55800" | cimarron_river_reproj$FCode=="33400"),]

dam=readOGR(dsn="~/Desktop/kansasGIS/KansasDarters",layer="Kansas_Dams")

dam_reproj=spTransform(dam, projection(nlcd_raster))

utmStr <- "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))


#MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Cimarron_streams", layer="Cimarron_redcrop",reproject=utmStr)

#cleanup(MyRivernetwork)->CimarronClean

load(file="/Users/nerdbrained/Desktop/kansasGIS/CimarronClean.rda")

sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]


cims=indsites[which(indsites$meta=="Cimarron River"),]
cims$ind=NULL
cimsunique=unique(cims)

#cimsunique=cimsunique[-29,]

cimpts<-data.frame(cimsunique$long,cimsunique$lat)
cimsppts<-SpatialPoints(cimpts,proj4string=CRS(latlong))
cimUTM<- spTransform(cimsppts,crs)
cimpts_reproj<- spTransform(cimsppts,projection(nlcd_raster))


cimsitecombs=combn(cimsunique$site,m=2)
cimlongcombs=combn(cimsunique$long,m=2)
cimlatcombs=combn(cimsunique$lat,m=2)

landscapedata=data.frame(cimsitecombs[1,],cimsitecombs[2,],cimlongcombs[1,],cimlongcombs[2,],cimlatcombs[1,],cimlatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=CimarronClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=CimarronClean) -> pairseg


#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0 & sum(a) > 1) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                 linebuff=buffer(lineseg,width=100)
                  linebuff=buffer(lineseg,width=500)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)



perbuff<-gIntersection(cimarron_allperennial,buff1trans)
intbuff<-gIntersection(cimarron_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

landscapedata$riverdist[i]=pairseg$ranges$range

}





########## walnut


fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
summary(fc)

x=c(-101,-96)
y=c(36,39)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)


walnut_watershed=fc_clipped[40]

#remove previous nlcd .tif file in home directory before running this
nlcd_raster <- get_nlcd(walnut_watershed,label = 'categorical-extraction',year = 2016,extraction.dir = '.',force.redo=TRUE)


Walnut_redcrop=readOGR(dsn="~/Desktop/kansasGIS/Walnut_streams/" ,layer="Walnut_redcrop")

walnut_watershed_reproj=spTransform(walnut_watershed, projection(nlcd_raster))
walnut_river_reproj=spTransform(Walnut_redcrop, projection(nlcd_raster))
walnut_artpath=walnut_river_reproj[which(walnut_river_reproj$FCode=="55800"),]
walnut_perennial=walnut_river_reproj[which(walnut_river_reproj$FCode=="46006"),]
walnut_intermit=walnut_river_reproj[which(walnut_river_reproj$FCode=="46003"),]
walnut_canal=walnut_river_reproj[which(walnut_river_reproj$FCode=="33400"),]

walnut_allperennial=walnut_river_reproj[which(walnut_river_reproj$FCode=="46006" | walnut_river_reproj$FCode=="55800" ),]

dam=readOGR(dsn="~/Desktop/kansasGIS/KansasDarters",layer="Kansas_Dams")

dam_reproj=spTransform(dam, projection(nlcd_raster))

utmStr <- "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))


#MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Walnut_streams", layer="Walnut_redcrop",reproject=utmStr)

#cleanup(MyRivernetwork)->WalnutClean

load(file="/Users/nerdbrained/Desktop/kansasGIS/WalnutClean.rda")

sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]


wals=indsites[which(indsites$meta=="Walnut Creek"),]
wals$ind=NULL
walsunique=unique(wals)

walpts<-data.frame(walsunique$long,walsunique$lat)
walsppts<-SpatialPoints(walpts,proj4string=CRS(latlong))
walUTM<- spTransform(walsppts,crs)
walpts_reproj<- spTransform(walsppts,projection(nlcd_raster))


walsitecombs=combn(walsunique$site,m=2)
wallongcombs=combn(walsunique$long,m=2)
wallatcombs=combn(walsunique$lat,m=2)

landscapedata=data.frame(walsitecombs[1,],walsitecombs[2,],wallongcombs[1,],wallongcombs[2,],wallatcombs[1,],wallatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=WalnutClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=WalnutClean) -> pairseg


#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0 & sum(a) > 1) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                 linebuff=buffer(lineseg,width=100)
                  linebuff=buffer(lineseg,width=500)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)



perbuff<-gIntersection(walnut_allperennial,buff1trans)
intbuff<-gIntersection(walnut_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

landscapedata$riverdist[i]=pairseg$ranges$range

}


########## medicine lodge


fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
summary(fc)

x=c(-101,-96)
y=c(36,39)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)


medicine_watershed=fc_clipped[61]


#remove previous nlcd .tif file in home directory before running this
nlcd_raster <- get_nlcd(medicine_watershed,label = 'categorical-extraction',year = 2016,extraction.dir = '.')


Medicine_redcrop=readOGR(dsn="~/Desktop/kansasGIS/Medicine_streams/" ,layer="Medicine_redcrop")

medicine_watershed_reproj=spTransform(medicine_watershed, projection(nlcd_raster))
medicine_river_reproj=spTransform(Medicine_redcrop, projection(nlcd_raster))
medicine_artpath=medicine_river_reproj[which(medicine_river_reproj$FCode=="55800"),]
medicine_perennial=medicine_river_reproj[which(medicine_river_reproj$FCode=="46006"),]
medicine_intermit=medicine_river_reproj[which(medicine_river_reproj$FCode=="46003"),]
medicine_canal=medicine_river_reproj[which(medicine_river_reproj$FCode=="33400"),]

medicine_allperennial=medicine_river_reproj[which(medicine_river_reproj$FCode=="46006" | medicine_river_reproj$FCode=="55800" | medicine_river_reproj$FCode=="33400"),]

dam=readOGR(dsn="~/Desktop/kansasGIS/KansasDarters",layer="Kansas_Dams")

dam_reproj=spTransform(dam, projection(nlcd_raster))

utmStr <- "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))


#MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Medicine_streams", layer="Medicine_redcrop",reproject=utmStr)

#cleanup(MyRivernetwork)->MedicineClean

load(file="/Users/nerdbrained/Desktop/kansasGIS/MedicineClean.rda")

sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]


meds=indsites[which(indsites$meta=="Medicine Lodge River"),]
meds$ind=NULL
medsunique=unique(meds)

medpts<-data.frame(medsunique$long,medsunique$lat)
medsppts<-SpatialPoints(medpts,proj4string=CRS(latlong))
medUTM<- spTransform(medsppts,crs)
medpts_reproj<- spTransform(medsppts,projection(nlcd_raster))


medsitecombs=combn(medsunique$site,m=2)
medlongcombs=combn(medsunique$long,m=2)
medlatcombs=combn(medsunique$lat,m=2)

landscapedata=data.frame(medsitecombs[1,],medsitecombs[2,],medlongcombs[1,],medlongcombs[2,],medlatcombs[1,],medlatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=MedicineClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=MedicineClean) -> pairseg


#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0 & sum(a) > 1) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                  linebuff=buffer(lineseg,width=100)
                  linebuff=buffer(lineseg,width=500)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)



perbuff<-gIntersection(medicine_allperennial,buff1trans)
intbuff<-gIntersection(medicine_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

landscapedata$riverdist[i]=pairseg$ranges$range

}







###### chikaskia!!! remove small guys?


fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
summary(fc)

x=c(-101,-96)
y=c(36,39)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)


chikaskia_watershed=fc_clipped[13]

#remove previous nlcd .tif file in home directory before running this
nlcd_raster <- get_nlcd(chikaskia_watershed,label = 'categorical-extraction',year = 2016,extraction.dir = '.')


Chikaskia_redcrop=readOGR(dsn="~/Desktop/kansasGIS/Chikaskia_streams/" ,layer="Chikaskia_redcrop")

Chikaskia_redcropl=Chikaskia_redcrop[which(Chikaskia_redcrop$LengthKM > 0.3),]

chikaskia_watershed_reproj=spTransform(chikaskia_watershed, projection(nlcd_raster))
chikaskia_river_reproj=spTransform(Chikaskia_redcropl, projection(nlcd_raster))
chikaskia_artpath=chikaskia_river_reproj[which(chikaskia_river_reproj$FCode=="55800"),]
chikaskia_perennial=chikaskia_river_reproj[which(chikaskia_river_reproj$FCode=="46006"),]
chikaskia_intermit=chikaskia_river_reproj[which(chikaskia_river_reproj$FCode=="46003"),]
chikaskia_canal=chikaskia_river_reproj[which(chikaskia_river_reproj$FCode=="33400"),]

chikaskia_allperennial=chikaskia_river_reproj[which(chikaskia_river_reproj$FCode=="46006" | chikaskia_river_reproj$FCode=="55800" | chikaskia_river_reproj$FCode=="33400"),]

dam=readOGR(dsn="~/Desktop/kansasGIS/KansasDarters",layer="Kansas_Dams")

dam_reproj=spTransform(dam, projection(nlcd_raster))

utmStr <- "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))


#MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Chikaskia_streams", layer="Chikaskia_redcrop",reproject=utmStr)

#cleanup(MyRivernetwork)->ChikaskiaClean

load(file="/Users/nerdbrained/Desktop/kansasGIS/ChikaskiaClean.rda")

sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]


chiks=indsites[which(indsites$meta=="Chikaskia River"),]
chiks$ind=NULL
chiksunique=unique(chiks)

chikpts<-data.frame(chiksunique$long,chiksunique$lat)
chiksppts<-SpatialPoints(chikpts,proj4string=CRS(latlong))
chikUTM<- spTransform(chiksppts,crs)
chikpts_reproj<- spTransform(chiksppts,projection(nlcd_raster))


chiksitecombs=combn(chiksunique$site,m=2)
chiklongcombs=combn(chiksunique$long,m=2)
chiklatcombs=combn(chiksunique$lat,m=2)

landscapedata=data.frame(chiksitecombs[1,],chiksitecombs[2,],chiklongcombs[1,],chiklongcombs[2,],chiklatcombs[1,],chiklatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=ChikaskiaClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=ChikaskiaClean) -> pairseg


#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0 & sum(a) > 1) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                 linebuff=buffer(lineseg,width=100)
                  linebuff=buffer(lineseg,width=500)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)



perbuff<-gIntersection(chikaskia_allperennial,buff1trans)
intbuff<-gIntersection(chikaskia_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

if (length(pairseg$ranges$range) > 0) {landscapedata$riverdist[i]=pairseg$ranges$range}

}







#########ninnescah


fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
summary(fc)

x=c(-101,-96)
y=c(36,39)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)


NFN=fc_clipped[47]
wSFN = fc_clipped[48]
eSFN = fc_clipped[21]

ninnescah_watershed=union(NFN,wSFN)
ninnescah_watershed=union(ninnescah_watershed,eSFN)
ninnescah_watershed=gUnaryUnion(ninnescah_watershed)

SFN=union(wSFN,eSFN)
SFN=gUnaryUnion(SFN)


#remove previous nlcd .tif file in home directory before running this
nlcd_raster <- get_nlcd(ninnescah_watershed,label = 'categorical-extraction',year = 2016,extraction.dir = '.')


Ninnescah_redcrop=readOGR(dsn="~/Desktop/kansasGIS/Ninnescah_streams/" ,layer="Ninnescah_redcrop")

ninnescah_watershed_reproj=spTransform(ninnescah_watershed, projection(nlcd_raster))
NFN_watershed_reproj=spTransform(NFN, projection(nlcd_raster))
SFN_watershed_reproj=spTransform(SFN, projection(nlcd_raster))


ninnescah_river_reproj=spTransform(Ninnescah_redcrop, projection(nlcd_raster))


ninnescah_artpath=ninnescah_river_reproj[which(ninnescah_river_reproj$FCode=="55800"),]
ninnescah_perennial=ninnescah_river_reproj[which(ninnescah_river_reproj$FCode=="46006"),]
ninnescah_intermit=ninnescah_river_reproj[which(ninnescah_river_reproj$FCode=="46003"),]
ninnescah_canal=ninnescah_river_reproj[which(ninnescah_river_reproj$FCode=="33400"),]

ninnescah_allperennial=ninnescah_river_reproj[which(ninnescah_river_reproj$FCode=="46006" | ninnescah_river_reproj$FCode=="55800"),]


NFN_reproj=crop(ninnescah_river_reproj,NFN_watershed_reproj)

NFN_artpath=NFN_reproj[which(NFN_reproj$FCode=="55800"),]
NFN_perennial=NFN_reproj[which(NFN_reproj$FCode=="46006"),]
NFN_intermit=NFN_reproj[which(NFN_reproj$FCode=="46003"),]
NFN_canal=NFN_reproj[which(NFN_reproj$FCode=="33400"),]

NFN_allperennial=NFN_reproj[which(NFN_reproj$FCode=="46006" | NFN_reproj$FCode=="55800"),]

SFN_reproj=crop(ninnescah_river_reproj,SFN_watershed_reproj)

SFN_artpath=SFN_reproj[which(SFN_reproj$FCode=="55800"),]
SFN_perennial=SFN_reproj[which(SFN_reproj$FCode=="46006"),]
SFN_intermit=SFN_reproj[which(SFN_reproj$FCode=="46003"),]
SFN_canal=SFN_reproj[which(SFN_reproj$FCode=="33400"),]

SFN_allperennial=SFN_reproj[which(SFN_reproj$FCode=="46006" | SFN_reproj$FCode=="55800"),]


dam=readOGR(dsn="~/Desktop/kansasGIS/KansasDarters",layer="Kansas_Dams")

dam_reproj=spTransform(dam, projection(nlcd_raster))

utmStr <- "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))


MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Ninnescah_streams", layer="Ninnescah_redcrop",reproject=utmStr)

cleanup(MyRivernetwork)->NinnescahClean



NFN_redcrop=crop(Ninnescah_redcrop,NFN)
writeOGR(NFN_redcrop, dsn="~/Desktop/kansasGIS/Ninnescah_streams/" ,layer="NFN_redcrop",driver="ESRI Shapefile")
NFN


SFN_redcrop=crop(Ninnescah_redcrop,SFN)
writeOGR(SFN_redcrop, dsn="~/Desktop/kansasGIS/Ninnescah_streams/" ,layer="SFN_redcrop",driver="ESRI Shapefile")

MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Ninnescah_streams", layer="NFN_redcrop",reproject=utmStr)

cleanup(MyRivernetwork)->NFNClean

load(file="/Users/nerdbrained/Desktop/kansasGIS/NFNClean.rda")

MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Ninnescah_streams", layer="SFN_redcrop",reproject=utmStr)

cleanup(MyRivernetwork)->SFNClean

load(file="/Users/nerdbrained/Desktop/kansasGIS/SFNClean.rda")


sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]


#nins=indsites[which(indsites$meta=="North Fork Ninnescah River" | indsites$meta=="South Fork Ninnescah River" ),]
nins=indsites[which(indsites$meta=="North Fork Ninnescah River"),]
#nins=indsites[which(indsites$meta=="South Fork Ninnescah River"),]
nins$ind=NULL
ninsunique=unique(nins)

ninpts<-data.frame(ninsunique$long,ninsunique$lat)
ninsppts<-SpatialPoints(ninpts,proj4string=CRS(latlong))
ninUTM<- spTransform(ninsppts,crs)
ninpts_reproj<- spTransform(ninsppts,projection(nlcd_raster))


ninsitecombs=combn(ninsunique$site,m=2)
ninlongcombs=combn(ninsunique$long,m=2)
ninlatcombs=combn(ninsunique$lat,m=2)

landscapedata=data.frame(ninsitecombs[1,],ninsitecombs[2,],ninlongcombs[1,],ninlongcombs[2,],ninlatcombs[1,],ninlatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
#riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=NinnescahClean)
#homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=NinnescahClean) -> pairseg
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=NFNClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=NFNClean) -> pairseg
#riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=SFNClean)
#homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=SFNClean) -> pairseg


#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0 & sum(a) > 1) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                 linebuff=buffer(lineseg,width=100)
				  linebuff=buffer(lineseg,width=500)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)



#perbuff<-gIntersection(ninnescah_allperennial,buff1trans)
#intbuff<-gIntersection(ninnescah_intermit,buff1trans)
perbuff<-gIntersection(NFN_allperennial,buff1trans)
intbuff<-gIntersection(NFN_intermit,buff1trans)
#perbuff<-gIntersection(SFN_allperennial,buff1trans)
#intbuff<-gIntersection(SFN_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

if (length(pairseg$ranges$range) > 0) {landscapedata$riverdist[i]=pairseg$ranges$range}

}






###### finding colorados

x=c(-105,-102)
y=c(38,40)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_cropped <- crop(fc,darterbox)

# View the feature class
plot(fc_clipped,lwd=0.2,add=T)

##middle ark = [2] = 10190003 = Middle South Platte-Cherry Creek
###middle ark = [11] = 11020003 = Fountain
###middle ark = [12] = 11020004 = Chico
###middle ark = [17] = 11020009 = Upper Arkansas-John Martin Reservoir
#middle ark = [18] = 11020011 = Big Sandy
###middle ark = [19] = 11020012 = Rush


#connector 1 = [10] = 11020002
#connector 2 = [13] = 11020005


############ finding ozarks

x=c(-94.45,-94.1)
y=c(36.1,36.3)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_cropped <- crop(fc,darterbox)

#Illinois = 11110103




#### colo shapes

#leave out horse creek and Black Squirrel Creek

#mspcc=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/10190003",layer="NHDFlowline")
fount=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020003",layer="NHDFlowline")
#chico=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020004",layer="NHDFlowline")
uajmr=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020009",layer="NHDFlowline")
bigsand=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020011",layer="NHDFlowline")
rush=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020012",layer="NHDFlowline")
con1=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020002",layer="NHDFlowline")
con2=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020005",layer="NHDFlowline")

rush_red=rush[which(rush$GNIS_Name == "Rush Creek" | rush$GNIS_Name == "Middle Rush Creek" | rush$GNIS_Name == "South Rush Creek" | rush$GNIS_Name == "North Rush Creek"),]

fount_red=fount[which(fount$GNIS_Name == "Fountain Creek" | fount$GNIS_Name == "Jimmy Camp Creek" |  fount$OBJECTID == "16789" |  fount$OBJECTID == "12143"),]

bigsand_red=bigsand[which(bigsand$GNIS_Name == "Big Sandy Creek"),]

uajmr_red=uajmr[which(uajmr$GNIS_Name == "Arkansas River" | uajmr$GNIS_Name == "Wild Horse Creek" | uajmr$GNIS_Name == "Buffalo Creek" | uajmr$OBJECTID == "2669" | uajmr$OBJECTID == "4838" |  uajmr$OBJECTID == "3614" | uajmr$OBJECTID == "2668" | uajmr$OBJECTID == "6170" | uajmr$OBJECTID == "1037" | uajmr$OBJECTID == "6134"),]

con1_red=con1[which(con1$GNIS_Name == "Arkansas River"),]

con2_red=con2[which(con2$GNIS_Name == "Arkansas River"),]

points(indsites$long,indsites$lat,col=indsites$col,pch=19,cex=0.5)

coloall=bind(rush_red,fount_red,bigsand_red,uajmr_red,con1_red,con2_red)

writeOGR(coloall,dsn="~/Desktop/kansasGIS/Colorado_streams/" ,layer="Colorado_red",driver="ESRI Shapefile")



###### illinois


illinois=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11110103",layer="NHDFlowline")

#wilson OK
# LIBR ok
# PAHO ok
#TUSPR OK? not on stream
# SVGC OK
#HESPR OK!!


illinois_red=illinois[which(illinois$GNIS_Name == "Illinois River" | illinois$GNIS_Name == "Lick Branch" | illinois$GNIS_Name == "Hamestring Creek" | illinois$GNIS_Name == "Clear Creek" | illinois$GNIS_Name == "Wildcat Creek" | illinois$GNIS_Name == "Little Osage Creek" | illinois$GNIS_Name == "Osage Creek" | illinois$OBJECTID == "13797" | illinois$OBJECTID == "7156" | illinois$OBJECTID == "17194" | illinois$OBJECTID == "12703" | illinois$OBJECTID == "4127" | illinois$OBJECTID == "18157" | illinois$OBJECTID == "16746" | illinois$OBJECTID == "13867" | illinois$OBJECTID == "17324" | illinois$OBJECTID == "17721" | illinois$OBJECTID == "4882" | illinois$OBJECTID == "7055" | illinois$OBJECTID == "5343" | illinois$OBJECTID == "12055" | illinois$OBJECTID == "8072" | illinois$OBJECTID == "2527" | illinois$OBJECTID == "3842" | illinois$OBJECTID == "9787" | illinois$OBJECTID == "10484" | illinois$OBJECTID == "11905" | illinois$OBJECTID == "402" | illinois$OBJECTID == "3105" | illinois$OBJECTID == "3105"  | illinois$OBJECTID == "16677"  | illinois$OBJECTID == "10190" | illinois$OBJECTID == "515" | illinois$OBJECTID == "9179" | illinois$OBJECTID == "15076" | illinois$OBJECTID == "17882" | illinois$OBJECTID == "2074" | illinois$OBJECTID == "15306" | illinois$OBJECTID == "9301" | illinois$OBJECTID == "152" | illinois$OBJECTID == "2379" | illinois$OBJECTID == "6741" | illinois$OBJECTID == "6009" | illinois$OBJECTID == "3520" | illinois$OBJECTID == "11020" | illinois$OBJECTID == "2379" | illinois$OBJECTID == "17813" | illinois$OBJECTID == "11541" | illinois$OBJECTID == "14686" | illinois$OBJECTID == "11204" | illinois$OBJECTID == "16625" | illinois$OBJECTID == "14395" | illinois$OBJECTID == "11541" | illinois$OBJECTID == "10300" | illinois$OBJECTID == "17652" | illinois$OBJECTID == "12769" | illinois$OBJECTID == "16395" | illinois$OBJECTID == "1033" | illinois$OBJECTID == "67" | illinois$OBJECTID == "11541" | illinois$OBJECTID == "11144" | illinois$OBJECTID == "8186" | illinois$OBJECTID == "2745" | illinois$OBJECTID == "6015" | illinois$OBJECTID == "11250" | illinois$OBJECTID == "3521" | illinois$OBJECTID == "6370" | illinois$OBJECTID == "14839" | illinois$OBJECTID == "13520" | illinois$OBJECTID == "6466" | illinois$OBJECTID == "7594"),]

ozarks=indsites[which(indsites$meta == "Illinois River"),]

points(ozarks$long,ozarks$lat,col=ozarks$col,pch=19,cex=0.5)


library(maptools)


x=c(-94.275,-94.265)
y=c(36.25,36.265)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

tempbox=bbox(S)

tempclip <- crop(illinois,tempbox)

tempclip2 <- crop(illinois_red,tempbox)

plot(tempclip,lwd=0.2)
plot(tempclip2,col="red",lwd=1,add=T)
text(ozarks$long,ozarks$lat,labels=ozarks$site,cex=0.5)

midpts=getSpatialLinesMidPoints(tempclip)

text(midpts,labels=tempclip$OBJECTID,cex=0.5)

writeOGR(illinois_red,dsn="~/Desktop/kansasGIS/Illinois_streams/" ,layer="Illinois_red",driver="ESRI Shapefile")
writeOGR(Illinois_redcrop,dsn="~/Desktop/kansasGIS/Illinois_streams/" ,layer="Illinois_redcrop",driver="ESRI Shapefile")


####Illinois landscape calc


fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
summary(fc)

x=c(-94.5,-94.1)
y=c(36,36.3)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)


ill = fc_clipped[2]


#remove previous nlcd .tif file in home directory before running this
nlcd_raster <- get_nlcd(ill,label = 'categorical-extraction',year = 2016,extraction.dir = '.')


Illinois_red=readOGR(dsn="~/Desktop/kansasGIS/Illinois_streams/" ,layer="Illinois_red")

Illinois_redcrop=crop(Illinois_red,ill)

illinois_watershed_reproj=spTransform(ill, projection(nlcd_raster))

illinois_river_reproj=spTransform(Illinois_redcrop, projection(nlcd_raster))


illinois_artpath=illinois_river_reproj[which(illinois_river_reproj$FCode=="55800"),]
illinois_perennial=illinois_river_reproj[which(illinois_river_reproj$FCode=="46006"),]
illinois_intermit=illinois_river_reproj[which(illinois_river_reproj$FCode=="46003"),]
illinois_canal=illinois_river_reproj[which(illinois_river_reproj$FCode=="33400"),]

illinois_allperennial=illinois_river_reproj[which(illinois_river_reproj$FCode=="46006" | illinois_river_reproj$FCode=="46000" | illinois_river_reproj$FCode=="55800"),]



ill_event=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11110103",layer="NHDPointEventFC")
ill_dam=ill_event[which(ill_event$EventType == "57100"),]

dam_reproj=spTransform(ill_dam, projection(nlcd_raster))

utmStr <- "+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))


MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Illinois_streams", layer="Illinois_redcrop",reproject=utmStr)

cleanup(MyRivernetwork)->IllinoisClean





sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]


#nins=indsites[which(indsites$meta=="North Fork Ninnescah River" | indsites$meta=="South Fork Ninnescah River" ),]
#nins=indsites[which(indsites$meta=="North Fork Ninnescah River"),]
illinois=indsites[which(indsites$meta=="Illinois River"),]
illinois$ind=NULL
illsunique=unique(illinois)

illpts<-data.frame(illsunique$long,illsunique$lat)
illsppts<-SpatialPoints(illpts,proj4string=CRS(latlong))
illUTM<- spTransform(illsppts,crs)
illpts_reproj<- spTransform(illsppts,projection(nlcd_raster))


illsitecombs=combn(illsunique$site,m=2)
illlongcombs=combn(illsunique$long,m=2)
illlatcombs=combn(illsunique$lat,m=2)

landscapedata=data.frame(illsitecombs[1,],illsitecombs[2,],illlongcombs[1,],illlongcombs[2,],illlatcombs[1,],illlatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=IllinoisClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=IllinoisClean) -> pairseg


#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0 & sum(a) > 1) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                 linebuff=buffer(lineseg,width=100)
                  linebuff=buffer(lineseg,width=500)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)




perbuff<-gIntersection(illinois_allperennial,buff1trans)
intbuff<-gIntersection(illinois_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

if (length(pairseg$ranges$range) > 0) {landscapedata$riverdist[i]=pairseg$ranges$range}

}








#### colorado calc


fgdb <- "~/Desktop/kansasGIS/wbdhu8_a_us_september2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="WBDHU8")

# Determine the FC extent, projection, and attribute information
x=c(-105,-101)
y=c(37.5,39.5)
xy <- cbind(x,y)
S <- SpatialPoints(xy)

darterbox=bbox(S)

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

fc_clipped <- gClip(fc,darterbox)

###middle ark = [8] = 11020003 = Fountain
###middle ark = [14] = 11020009 = Upper Arkansas-John Martin Reservoir
#middle ark = [15] = 11020011 = Big Sandy
###middle ark = [16] = 11020012 = Rush


#connector 1 = [7] = 11020002
#connector 2 = [10] = 11020005

fountw= fc_clipped[8]
uajmrw= fc_clipped[14]
bigsandw= fc_clipped[15]
rushw= fc_clipped[16]
con1w= fc_clipped[7]
con2w= fc_clipped[10]

colorado_watershed=union(fountw,uajmrw)
colorado_watershed=union(colorado_watershed,bigsandw)
colorado_watershed=union(colorado_watershed,rushw)
colorado_watershed=union(colorado_watershed,con1w)
colorado_watershed=union(colorado_watershed,con2w)
colorado_watershed=gUnaryUnion(colorado_watershed)


#remove previous nlcd .tif file in home directory before running this
nlcd_raster <- get_nlcd(colorado_watershed,label = 'categorical-extraction',year = 2016,extraction.dir = '.')


Colorado_red=readOGR(dsn="~/Desktop/kansasGIS/Colorado_streams/" ,layer="Colorado_red")


#utmStr <- "+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
#crs <- CRS(sprintf(utmStr, 32))


Colorado_redcrop=crop(Colorado_red,colorado_watershed)

colorado_watershed_reproj=spTransform(colorado_watershed, projection(nlcd_raster))

colorado_river_reproj=spTransform(Colorado_redcrop, projection(nlcd_raster))


colorado_artpath=colorado_river_reproj[which(colorado_river_reproj$FCode=="55800"),]
colorado_perennial=colorado_river_reproj[which(colorado_river_reproj$FCode=="46006"),]
colorado_intermit=colorado_river_reproj[which(colorado_river_reproj$FCode=="46003"),]
colorado_canal=colorado_river_reproj[which(colorado_river_reproj$FCode=="33400"),]

colorado_allperennial=colorado_river_reproj[which(colorado_river_reproj$FCode=="46006" | colorado_river_reproj$FCode=="46000" | colorado_river_reproj$FCode=="55800" | colorado_river_reproj$FCode=="33400" | colorado_river_reproj$FCode=="33600"| colorado_river_reproj$FCode=="42801"),]



colo_event1=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020002",layer="NHDPointEventFC")
colo_event2=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020003",layer="NHDPointEventFC")
colo_event3=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020005",layer="NHDPointEventFC")
colo_event4=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020009",layer="NHDPointEventFC")
colo_event5=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020011",layer="NHDPointEventFC")
colo_event6=readOGR(dsn="~/Desktop/kansasGIS/COARshapes/11020012",layer="NHDPointEventFC")
colo_event=bind(colo_event1,colo_event2,colo_event3,colo_event4,colo_event5,colo_event6)
colo_dam=colo_event[which(colo_event$EventType == "57100"),]

dam_reproj=spTransform(colo_dam, projection(nlcd_raster))

utmStr <- "+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
latlong<-"+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
crs <- CRS(sprintf(utmStr, 32))


MyRivernetwork <- line2network(path="/Users/nerdbrained/Desktop/kansasGIS/Colorado_streams", layer="Colorado_red",reproject=utmStr)

cleanup(MyRivernetwork)->ColoradoClean




sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]

colos=indsites[which(indsites$meta=="Upper Arkansas River" | indsites$meta=="Middle Arkansas River" | indsites$meta=="Big Sandy / Rush Creeks"),]
colos$ind=NULL
colosunique=unique(colos)

colosunique=colosunique[-which(colosunique$site=="BSQ" | colosunique$site=="HCR"),]

colopts<-data.frame(colosunique$long,colosunique$lat)
colosppts<-SpatialPoints(colopts,proj4string=CRS(latlong))
coloUTM<- spTransform(colosppts,crs)
colopts_reproj<- spTransform(colosppts,projection(nlcd_raster))


colositecombs=combn(colosunique$site,m=2)
cololongcombs=combn(colosunique$long,m=2)
cololatcombs=combn(colosunique$lat,m=2)

landscapedata=data.frame(colositecombs[1,],colositecombs[2,],cololongcombs[1,],cololongcombs[2,],cololatcombs[1,],cololatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=ColoradoClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=ColoradoClean) -> pairseg


#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0 & sum(a) > 1) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                 linebuff=buffer(lineseg,width=100)
                  linebuff=buffer(lineseg,width=500)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)




perbuff<-gIntersection(colorado_allperennial,buff1trans)
intbuff<-gIntersection(colorado_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

if (length(pairseg$ranges$range) > 0) {landscapedata$riverdist[i]=pairseg$ranges$range}

}








##### all Ninnescahs

#########ninnescah


load(file="/Users/nerdbrained/Desktop/kansasGIS/NinnescahClean.rda")


sitelocs=read.csv("~/Desktop/rapture/ECRapture45/sitelocs.csv")
indsites=read.csv("~/Desktop/rapture/ECRapture45/indsites_meta2.csv",header=T)
indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]

metalist=read.csv("~/Desktop/rapture/ECRapture45/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
indsites$col<-metalist$V2[match(indsites$meta,metalist$V1)]


nins=indsites[which(indsites$meta=="North Fork Ninnescah River" | indsites$meta=="South Fork Ninnescah River" ),]
#nins=indsites[which(indsites$meta=="North Fork Ninnescah River"),]
#nins=indsites[which(indsites$meta=="South Fork Ninnescah River"),]
nins$ind=NULL
ninsunique=unique(nins)

ninpts<-data.frame(ninsunique$long,ninsunique$lat)
ninsppts<-SpatialPoints(ninpts,proj4string=CRS(latlong))
ninUTM<- spTransform(ninsppts,crs)
ninpts_reproj<- spTransform(ninsppts,projection(nlcd_raster))


ninsitecombs=combn(ninsunique$site,m=2)
ninlongcombs=combn(ninsunique$long,m=2)
ninlatcombs=combn(ninsunique$lat,m=2)

landscapedata=data.frame(ninsitecombs[1,],ninsitecombs[2,],ninlongcombs[1,],ninlongcombs[2,],ninlatcombs[1,],ninlatcombs[2,])

names(landscapedata)=c("site1","site2","long1","long2","lat1","lat2")

landscapedata$fprop=rep(0,nrow(landscapedata))
landscapedata$wprop=rep(0,nrow(landscapedata))
landscapedata$gprop=rep(0,nrow(landscapedata))
landscapedata$dprop=rep(0,nrow(landscapedata))
landscapedata$cprop=rep(0,nrow(landscapedata))
landscapedata$riverdist=rep(0,nrow(landscapedata))
landscapedata$perennial=rep(0,nrow(landscapedata))
landscapedata$intermit=rep(0,nrow(landscapedata))
landscapedata$dams=rep(0,nrow(landscapedata))


###iterate through points

for (i in 1:nrow(landscapedata)){
	
xs=c(landscapedata[i,3],landscapedata[i,4])
ys=c(landscapedata[i,5],landscapedata[i,6])

pts=data.frame(xs,ys)

sppts<-SpatialPoints(pts,proj4string=CRS(latlong))
UTMpts<- spTransform(sppts,crs)
convpts=coordinates(UTMpts)
riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=NinnescahClean)
homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=NinnescahClean) -> pairseg
#riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=NFNClean)
#homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=NFNClean) -> pairseg
#riv <- xy2segvert(x=convpts[,1], y=convpts[,2], rivers=SFNClean)
#homerange(unique=riv$pair, seg=riv$seg, vert=riv$vert, rivers=SFNClean) -> pairseg


#buff=buffer(UTMpts,width=100)
buff=buffer(UTMpts,width=500)
buff1=buffer(UTMpts,width=5)

for (j in 1:length(pairseg$subseg_n[[1]])) {
            a <- pairseg$subseg_n[[1]][[j]]
            n <- length(a)
            firsts <- c(1, (which(a[-n] != a[-1]) + 1))
            lasts <- c(which(a[-n] != a[-1]), n)
            denses <- a[firsts]
            for (k in 1:length(denses)) {
            	if (denses[k] > 0 & sum(a) > 1) {
                  linecoords<-pairseg$rivers$lines[[j]][(firsts[k]:lasts[k]),]
                  lineseg<-spLines(linecoords,crs=crs)
#                 linebuff=buffer(lineseg,width=100)
				  linebuff=buffer(lineseg,width=500)
                  linebuff1=buffer(lineseg,width=5)  
                  buff=union(buff,linebuff)
                  buff=gUnaryUnion(buff)
                  buff1=union(buff1,linebuff1)
                  buff1=gUnaryUnion(buff1)   }
            }
        }
        

bufftrans=spTransform(buff, projection(nlcd_raster))
buff1trans=spTransform(buff1, projection(nlcd_raster))
landcover <- extract(nlcd_raster, bufftrans)
landcoversum <- table(landcover)

f=sum(as.integer(landcoversum[names(landcoversum)==41]),as.integer(landcoversum[names(landcoversum)==42]),as.integer(landcoversum[names(landcoversum)==43]),as.integer(landcoversum[names(landcoversum)==44]))
w=sum(as.integer(landcoversum[names(landcoversum)==11]),as.integer(landcoversum[names(landcoversum)==90]),as.integer(landcoversum[names(landcoversum)==95]))
g=as.integer(landcoversum[names(landcoversum)==71])
###add shrub(52)? barren(31)? pasture(81)?
d=sum(as.integer(landcoversum[names(landcoversum)==21]),as.integer(landcoversum[names(landcoversum)==22]),as.integer(landcoversum[names(landcoversum)==23]),as.integer(landcoversum[names(landcoversum)==24]))
c=sum(as.integer(landcoversum[names(landcoversum)==81]),as.integer(landcoversum[names(landcoversum)==82]))

landscapedata$fprop[i]=f/(f+w+g+d+c)
landscapedata$wprop[i]=w/(f+w+g+d+c)
landscapedata$gprop[i]=g/(f+w+g+d+c)
landscapedata$dprop[i]=d/(f+w+g+d+c)
landscapedata$cprop[i]=c/(f+w+g+d+c)



perbuff<-gIntersection(ninnescah_allperennial,buff1trans)
intbuff<-gIntersection(ninnescah_intermit,buff1trans)
#perbuff<-gIntersection(NFN_allperennial,buff1trans)
#intbuff<-gIntersection(NFN_intermit,buff1trans)
#perbuff<-gIntersection(SFN_allperennial,buff1trans)
#intbuff<-gIntersection(SFN_intermit,buff1trans)

perlen<-NULL
intlen<-NULL

if(!is.null(perbuff)) perlen=(gLength(perbuff))
if(!is.null(intbuff)) intlen=(gLength(intbuff))

if(!is.null(perlen)) {landscapedata$perennial[i]=perlen}
if(!is.null(intlen)) {landscapedata$intermit[i]=intlen}
damcount=over(buff1trans,dam_reproj,fn=length)
if(!is.null(damcount$name)) {landscapedata$dams[i]=damcount$name}

if (length(pairseg$ranges$range) > 0) {landscapedata$riverdist[i]=pairseg$ranges$range}

}