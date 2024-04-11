library(MuMIn)

######## plot stream dist vs fst for each drainage

sitelist=read.table("metadata/sitelist.txt")
pwise.nei=read.table("analysis_outputs/allsites_neiFST")
rownames(pwise.nei)=sitelist$V1
colnames(pwise.nei)=sitelist$V1

###chikaskia
chiklandscape=read.csv("mapping/landscape_data/chikaskiadata.csv")

chiklandscape$fst=0
for (row in 1:nrow(chiklandscape)){
  chiklandscape$fst[row] = pwise.nei[chiklandscape$site1[row],chiklandscape$site2[row]]
}

chiklandscape$linfst=chiklandscape$fst/(1-chiklandscape$fst)
plot(chiklandscape$riverdist,chiklandscape$linfst,main="Chikaskia",xlab="River Distance",ylab="Fst/(1-Fst)")
riverlm=lm(linfst~riverdist,data=chiklandscape)
abline(riverlm)
summary(riverlm)

chiklandscape$meta="Chikaskia"

###cimarron
cimlandscape=read.csv("mapping/landscape_data/cimarrondata.csv")

cimlandscape$fst=0
for (row in 1:nrow(cimlandscape)){
  cimlandscape$fst[row] = pwise.nei[cimlandscape$site1[row],cimlandscape$site2[row]]
}

cimlandscape$linfst=cimlandscape$fst/(1-cimlandscape$fst)
plot(cimlandscape$riverdist,cimlandscape$linfst,main="Cimarron",xlab="River Distance",ylab="Fst/(1-Fst)")
riverlm=lm(linfst~riverdist,data=cimlandscape)
abline(riverlm)
summary(riverlm)

cimlandscape$meta="Cimarron"


###colorado
cololandscape=read.csv("mapping/landscape_data/Coloradodata.csv")

cololandscape$fst=0
for (row in 1:nrow(cololandscape)){
  cololandscape$fst[row] = pwise.nei[cololandscape$site1[row],cololandscape$site2[row]]
}

cololandscape$linfst=cololandscape$fst/(1-cololandscape$fst)
plot(cololandscape$riverdist,cololandscape$linfst,main="Colorado",xlab="River Distance",ylab="Fst/(1-Fst)")
riverlm=lm(linfst~riverdist,data=cololandscape)
abline(riverlm)
summary(riverlm)

cololandscape$meta="Colorado"


###arkrattle
arklandscape=read.csv("mapping/landscape_data/arkrattledata.csv")
arklandscape=arklandscape[-which(arklandscape$site1=="RATT2016"|arklandscape$site2=="RATT2016"),]

arklandscape$fst=0
for (row in 1:nrow(arklandscape)){
  arklandscape$fst[row] = pwise.nei[arklandscape$site1[row],arklandscape$site2[row]]
}

arklandscape$linfst=arklandscape$fst/(1-arklandscape$fst)
plot(arklandscape$riverdist,arklandscape$linfst,main="Lower Arkansas/Rattlesnake",xlab="River Distance",ylab="Fst/(1-Fst)")
riverlm=lm(linfst~riverdist,data=arklandscape)
abline(riverlm)
summary(riverlm)

## examine sites on Slate Creek and below the Arkansas/Slate confluence 

arklandscape$sitecomb=paste(arklandscape$site1,arklandscape$site2)
arklandscape_nfn=arklandscape[grep('SLATE|CNCSC|KAWN', arklandscape$sitecomb),]
arklandscape_nonfn=arklandscape[grep('SLATE|CNCSC|KAWN', arklandscape$sitecomb,invert=TRUE),]

plot(arklandscape_nfn$riverdist,arklandscape_nfn$linfst,main="Lower Arkansas/Rattlesnake",xlab="River Distance",ylab="Fst/(1-Fst)",col="red")
riverlm=lm(linfst~riverdist,data=arklandscape_nfn)
abline(riverlm,col="red")
summary(riverlm)

points(arklandscape_nonfn$riverdist,arklandscape_nonfn$linfst)
points(arklandscape_nfn$riverdist,arklandscape_nfn$linfst,col="red")
riverlm=lm(linfst~riverdist,data=arklandscape_nonfn)
abline(riverlm,col="black")
summary(riverlm)

legend(x=4.2e+05,y=0.25,legend=c("Ark","Ninn"),pch=c(1,1),col=c("black","red"))

## artificially high pairwise Fsts likely because of admixture with Ninnescah - remove these from streamscape analysis

arklandscape_nonfn$meta="LowerArk"
arklandscape_nonfn$sitecomb=NULL

arklandscape$meta="LowerArk"
arklandscape$sitecomb=NULL

###illinois
illlandscape=read.csv("mapping/landscape_data/illinoisdata.csv")

illlandscape$fst=0
for (row in 1:nrow(illlandscape)){
  illlandscape$fst[row] = pwise.nei[illlandscape$site1[row],illlandscape$site2[row]]
}

illlandscape$linfst=illlandscape$fst/(1-illlandscape$fst)
plot(illlandscape$riverdist,illlandscape$linfst,main="Illinois",xlab="River Distance",ylab="Fst/(1-Fst)",col="black")
riverlm=lm(linfst~riverdist,data=illlandscape)
abline(riverlm)
summary(riverlm)

illlandscape$meta="Illinois"

###medicine

medlandscape=read.csv("mapping/landscape_data/medicinedata.csv")

medlandscape$fst=0
for (row in 1:nrow(medlandscape)){
  medlandscape$fst[row] = pwise.nei[medlandscape$site1[row],medlandscape$site2[row]]
}

medlandscape$linfst=medlandscape$fst/(1-medlandscape$fst)
plot(medlandscape$riverdist,medlandscape$linfst,main="Medicine Lodge",xlab="River Distance",ylab="Fst/(1-Fst)",col="black")
riverlm=lm(linfst~riverdist,data=medlandscape)
abline(riverlm)
summary(riverlm)

medlandscape[order(-medlandscape$linfst),]

medlandscape$meta="Medicine"

###NFninnescah

nfnlandscape=read.csv("mapping/landscape_data/NFNlandscapedata.csv")

nfnlandscape$fst=0
for (row in 1:nrow(nfnlandscape)){
  nfnlandscape$fst[row] = pwise.nei[nfnlandscape$site1[row],nfnlandscape$site2[row]]
}

nfnlandscape$linfst=nfnlandscape$fst/(1-nfnlandscape$fst)
plot(nfnlandscape$riverdist,nfnlandscape$linfst)
riverlm=lm(linfst~riverdist,data=nfnlandscape)
abline(riverlm)
summary(riverlm)

nfnlandscape$sitecomb=paste(nfnlandscape$site1,nfnlandscape$site2)

## examine sites upstream of Cheney Reservoir Dam

#nfnlandscape_chn=nfnlandscape[grep("CHN2|CHN3|CHN4|CHN5|CHN6|CHN7|CHN8|CHN9|CHN10|2008-24|NFN2|TNFN1|NFNTS", nfnlandscape$sitecomb), ]
#nfnlandscape_nochn=nfnlandscape[grep("CHN2|CHN3|CHN4|CHN5|CHN6|CHN7|CHN8|CHN9|CHN10|2008-24|NFN2|TNFN1|NFNTS", nfnlandscape$sitecomb,invert=TRUE), ]

nfnlandscape_chn=nfnlandscape[grep("CHN2|CHN3|CHN4|CHN5|CHN6|CHN7|CHN8|CHN9|CHN10|2008-24", nfnlandscape$sitecomb), ]
nfnlandscape_nochn=nfnlandscape[grep("CHN2|CHN3|CHN4|CHN5|CHN6|CHN7|CHN8|CHN9|CHN10|2008-24", nfnlandscape$sitecomb,invert=TRUE), ]

col=c("#FFD92F","#FFD92F","#FFD92F","#8DA0CB","#8DA0CB","#FC8D62","#66C2A5","#E78AC3","#E5C494","#E5C494","#A6D854")

par(mai=c(1,1,1,1))

plot(nfnlandscape_chn$riverdist,nfnlandscape_chn$linfst,ylim=c(-0.01,0.13),main="North Fork Ninnescah",xlab="River Distance",ylab=expression("F"[ST]*"(/1-F"[ST]*")"),col="#999999",pch=19)
riverlm_chn=lm(linfst~riverdist,data=nfnlandscape_chn)
abline(riverlm_chn,col="#999999",lwd=3)
summary(riverlm_chn)

#nfnlandscape_nochn=nfnlandscape[grep("CHN", nfnlandscape$sitecomb,invert=TRUE), ]

points(nfnlandscape_nochn$riverdist,nfnlandscape_nochn$linfst,col="#56B4E9",pch=19)
riverlm_nochn=lm(linfst~riverdist,data=nfnlandscape_nochn)
abline(riverlm_nochn,col="#56B4E9",lwd=3)
summary(riverlm_nochn)
legend(x=60000,y=0.13,legend=c("Cheney Reservoir tributaries","All other Ninnescah"),pch=c(19,19),col=c("#999999","#56B4E9"))

#plot(USstreams,xlim=c(-97.95,-97.8),ylim=c(37.7,37.9))
#text(labels=allsites$site,x=allsites$long,y=allsites$lat)

#ninnhet=sitehetlocs[which(sitehetlocs$meta=="NFNinnescah"),]

#plot(ninnhet$long,ninnhet$lat,cex=ninnhet$het*5)

nfnlandscape_nochn$meta="NFNinnescah"
nfnlandscape_nochn$sitecomb=NULL

nfnlandscape$meta="NFNinnescah"
nfnlandscape$sitecomb=NULL

###SFninnescah

sfnlandscape=read.csv("mapping/landscape_data/SFNlandscapedata.csv")

sfnlandscape$fst=0
for (row in 1:nrow(sfnlandscape)){
  sfnlandscape$fst[row] = pwise.nei[sfnlandscape$site1[row],sfnlandscape$site2[row]]
}

sfnlandscape$linfst=sfnlandscape$fst/(1-sfnlandscape$fst)
plot(sfnlandscape$riverdist,sfnlandscape$linfst,main="South Fork Ninnescah",xlab="River Distance",ylab="Fst/(1-Fst)",col="black")
riverlm=lm(linfst~riverdist,data=sfnlandscape)
abline(riverlm)
summary(riverlm)

sfnlandscape[order(-sfnlandscape$linfst),]

sfnlandscape$meta="SFNinnescah"

###saltfork

saltlandscape=read.csv("mapping/landscape_data/saltforkdata.csv")

saltlandscape$fst=0
for (row in 1:nrow(saltlandscape)){
  saltlandscape$fst[row] = pwise.nei[saltlandscape$site1[row],saltlandscape$site2[row]]
}

saltlandscape$linfst=saltlandscape$fst/(1-saltlandscape$fst)
plot(saltlandscape$riverdist,saltlandscape$linfst,main="Salt Fork Arkansas River",xlab="River Distance",ylab="Fst/(1-Fst)",col="black")
riverlm=lm(linfst~riverdist,data=saltlandscape)
abline(riverlm)
summary(riverlm)

saltlandscape[order(-saltlandscape$linfst),]

saltlandscape$meta="SaltFork"
saltlandscape$streamsum=NULL
saltlandscape$over=NULL

###walnut

wallandscape=read.csv("mapping/landscape_data/walnutdata.csv")

wallandscape$fst=0
for (row in 1:nrow(wallandscape)){
  wallandscape$fst[row] = pwise.nei[wallandscape$site1[row],wallandscape$site2[row]]
}

wallandscape$linfst=wallandscape$fst/(1-wallandscape$fst)
plot(wallandscape$riverdist,wallandscape$linfst)
riverlm=lm(linfst~riverdist,data=wallandscape)
abline(riverlm)
summary(riverlm)

wallandscape$meta="Walnut"





###### streamscape analysis

indsitesmeta=read.csv("metadata/indsites_meta3.csv")

#colotest=cololandscape
#colotest$meta1=indsitesmeta$meta[match(colotest$site1,indsitesmeta$site)]
#colotest$meta2=indsitesmeta$meta[match(colotest$site2,indsitesmeta$site)]

full = rbind(chiklandscape,cimlandscape,cololandscape,arklandscape_nonfn,medlandscape,nfnlandscape,sfnlandscape,saltlandscape)
full$meta1=indsitesmeta$meta[match(full$site1,indsitesmeta$site)]
full$meta2=indsitesmeta$meta[match(full$site2,indsitesmeta$site)]

#prune to comparisons within metapopulations
full_metaprune1=full[which(full$meta1==full$meta2),]

#remove comparisons involving big sandy creek (very high Fst outlier)
full_metaprune1=full_metaprune1[-which(full_metaprune1$site1=="BSC" | full_metaprune1$site2=="BSC"),]
#full_metaprune2=full[which(full$meta1==full$meta2),]

#add information on dam locations
full_metaprune1$obstruct = ifelse(full_metaprune1$site1 == "CAV1" | full_metaprune1$site1 == "AMBER2" | full_metaprune1$site1 == "NBSC" | full_metaprune1$site1 == "NFN1" | full_metaprune1$site1 == "NFNINNSF" | full_metaprune1$site1 == "ZANG" | full_metaprune1$site1 == "SFNR4" | full_metaprune1$site1 == "TEASLY" | full_metaprune1$site1 == "SFNR3" | full_metaprune1$site2 == "CAV1" | full_metaprune1$site2 == "AMBER2" | full_metaprune1$site2 == "NBSC" | full_metaprune1$site2 == "NFN1" | full_metaprune1$site2 == "NFNINNSF" | full_metaprune1$site2 == "ZANG" | full_metaprune1$site2 == "SFNR4" | full_metaprune1$site2 == "TEASLY" | full_metaprune1$site2 == "SFNR3","dammed","undammed" )
#full_metaprune2$obstruct = ifelse(full_metaprune2$site1 == "CAV1|AMBER2|NBSC|NFN1|NFNINNSF|ZANG|SFNR4|TEASLY|SFNR3" | full_metaprune2$site2 == "CAV1|AMBER2|NBSC|NFN1|NFNINNSF|ZANG|SFNR4|TEASLY|SFNR3","dammed","undammed" )

#add information on reservoir
full_metaprune1$reservoir = ifelse(full_metaprune1$site1 == "CHN2" |full_metaprune1$site1 == "CHN3" |full_metaprune1$site1 == "CHN4" | full_metaprune1$site1 == "CHN5" | full_metaprune1$site1 == "CHN6" | full_metaprune1$site1 == "CHN7" | full_metaprune1$site1 == "CHN8" | full_metaprune1$site1 == "CHN9" | full_metaprune1$site1 == "CHN10" | full_metaprune1$site1 == "2008-24" | full_metaprune1$site2 == "CHN2" | full_metaprune1$site2 == "CHN3" | full_metaprune1$site2 == "CHN4" | full_metaprune1$site2 == "CHN5" | full_metaprune1$site2 == "CHN6" | full_metaprune1$site2 == "CHN7" | full_metaprune1$site2 == "CHN8" | full_metaprune1$site2 == "CHN9" | full_metaprune1$site2 == "CHN10" | full_metaprune1$site2 == "2008-24","reservoir","nores")
#full_metaprune2$reservoir = ifelse(full_metaprune2$site1 == "CHN2|CHN3|CHN4|CHN5|CHN6|CHN7|CHN8|CHN9|CHN10|2008-24" | full_metaprune2$site2 == "CHN2|CHN3|CHN4|CHN5|CHN6|CHN7|CHN8|CHN9|CHN10|2008-24","reservoir","nores")

#get proportion perennial and intermittent
full_metaprune1$pprop=full_metaprune1$perennial/(full_metaprune1$riverdist+5)
full_metaprune1$iprop=full_metaprune1$intermit/(full_metaprune1$riverdist+5)

#set global model
global_riverlm=lm(linfst~riverdist*meta2+fprop+gprop+dprop+cprop+iprop+obstruct+reservoir,data=full_metaprune1)
summary(global_riverlm)

#dredge
options(na.action = "na.fail")
dd_meta=dredge(global_riverlm)

best_riverlm=lm(linfst~cprop+iprop+fprop+riverdist*meta2+reservoir+obstruct,data=full_metaprune1)
second_riverlm=lm(linfst~cprop+iprop+fprop+dprop+riverdist*meta2+reservoir+obstruct,data=full_metaprune1)
third_riverlm=lm(linfst~cprop+iprop+fprop+gprop+riverdist*meta2+reservoir+obstruct,data=full_metaprune1)

best_riverlm_nometa=lm(linfst~cprop+iprop+fprop+riverdist+reservoir+obstruct,data=full_metaprune1)

summary(best_riverlm)

summary(best_riverlm_nometa)

landscape_full_modsel=data.frame(dd_meta$df,dd_meta$logLik,dd_meta$AICc,dd_meta$delta,dd_meta$weight)

write.csv(landscape_full_modsel,"~/Desktop/ArkDarterManuscript/landscape_modsel_fulltable.csv")

##### calculate random R2s and plot empirical Rs

randr2s=vector()

cololandscape_metaprune=cololandscape

cololandscape_metaprune$meta1=indsitesmeta$meta[match(cololandscape_metaprune$site1,indsitesmeta$site)]
cololandscape_metaprune$meta2=indsitesmeta$meta[match(cololandscape_metaprune$site2,indsitesmeta$site)]
cololandscape_metaprune=cololandscape_metaprune[which(cololandscape_metaprune$meta1==cololandscape_metaprune$meta2),]
cololandscape_metaprune$meta1=NULL
cololandscape_metaprune$meta2=NULL
cololandscape_metaprune=cololandscape_metaprune[-which(cololandscape_metaprune$site1=="BSC" | cololandscape_metaprune$site2=="BSC"),]

for(i in seq(1,100)){
  chik_rand=transform(chiklandscape,linfst=sample(linfst))
  cim_rand=transform(cimlandscape,linfst=sample(linfst))
  colo_rand=transform(cololandscape_metaprune,linfst=sample(linfst))
  ark_rand=transform(arklandscape_nonfn,linfst=sample(linfst))
  med_rand=transform(medlandscape,linfst=sample(linfst))
  nfn_rand=transform(nfnlandscape,linfst=sample(linfst))
  sfn_rand=transform(sfnlandscape,linfst=sample(linfst))
  salt_rand=transform(saltlandscape,linfst=sample(linfst))
  full_rand = rbind(chik_rand,cim_rand,colo_rand,ark_rand,med_rand,nfn_rand,sfn_rand,salt_rand)
  full_rand$meta1=indsitesmeta$meta[match(full_rand$site1,indsitesmeta$site)]
  full_rand$meta2=indsitesmeta$meta[match(full_rand$site2,indsitesmeta$site)]
  full_metaprune1_rand=full_rand[which(full_rand$meta1==full_rand$meta2),]
  full_metaprune1_rand$obstruct = ifelse(full_metaprune1_rand$site1 == "CAV1" | full_metaprune1_rand$site1 == "AMBER2" | full_metaprune1_rand$site1 == "NBSC" | full_metaprune1_rand$site1 == "NFN1" | full_metaprune1_rand$site1 == "NFNINNSF" | full_metaprune1_rand$site1 == "ZANG" | full_metaprune1_rand$site1 == "SFNR4" | full_metaprune1_rand$site1 == "TEASLY" | full_metaprune1_rand$site1 == "SFNR3" | full_metaprune1_rand$site2 == "CAV1" | full_metaprune1_rand$site2 == "AMBER2" | full_metaprune1_rand$site2 == "NBSC" | full_metaprune1_rand$site2 == "NFN1" | full_metaprune1_rand$site2 == "NFNINNSF" | full_metaprune1_rand$site2 == "ZANG" | full_metaprune1_rand$site2 == "SFNR4" | full_metaprune1_rand$site2 == "TEASLY" | full_metaprune1_rand$site2 == "SFNR3","dammed","undammed" )
  full_metaprune1_rand$reservoir = ifelse(full_metaprune1_rand$site1 == "CHN2" |full_metaprune1_rand$site1 == "CHN3" |full_metaprune1_rand$site1 == "CHN4" | full_metaprune1_rand$site1 == "CHN5" | full_metaprune1_rand$site1 == "CHN6" | full_metaprune1_rand$site1 == "CHN7" | full_metaprune1_rand$site1 == "CHN8" | full_metaprune1_rand$site1 == "CHN9" | full_metaprune1_rand$site1 == "CHN10" | full_metaprune1_rand$site1 == "2008-24" | full_metaprune1_rand$site2 == "CHN2" | full_metaprune1_rand$site2 == "CHN3" | full_metaprune1_rand$site2 == "CHN4" | full_metaprune1_rand$site2 == "CHN5" | full_metaprune1_rand$site2 == "CHN6" | full_metaprune1_rand$site2 == "CHN7" | full_metaprune1_rand$site2 == "CHN8" | full_metaprune1_rand$site2 == "CHN9" | full_metaprune1_rand$site2 == "CHN10" | full_metaprune1_rand$site2 == "2008-24","reservoir","nores")
  full_metaprune1_rand$pprop=full_metaprune1_rand$perennial/(full_metaprune1_rand$riverdist+5)
  full_metaprune1_rand$iprop=full_metaprune1_rand$intermit/(full_metaprune1_rand$riverdist+5)
  best_riverlm_rand=lm(linfst~cprop+iprop+fprop+riverdist*meta+reservoir+obstruct,data=full_metaprune1_rand)
  randr2s[i]=summary(best_riverlm_rand)$r.squared
}

realr2=summary(best_riverlm)$r.squared

plot(hist(randr2s),xlim=c(0,1),main="",xlab=expression(R^2))
lines(x=c(realr2,realr2),y=c(0,50),lty=2,lwd=3,col="red")

plot(full_metaprune1$riverdist,full_metaprune1$linfst)

plot(full_metaprune1_rand$riverdist,full_metaprune1_rand$linfst)


############ try with Ne instead of categorical metapop

effpops=read.csv("~/Desktop/ArkDarterManuscript/Ne_data.csv")

full_metaprune1$ne=effpops$NE[match(full_metaprune1$meta2,effpops$Dataset)]

global_riverlm_ne=lm(linfst~ne+riverdist+fprop+gprop+dprop+cprop+iprop+obstruct+reservoir,data=full_metaprune1)

dd_meta_ne=dredge(global_riverlm_ne)

summary(global_riverlm_ne)

######## both categorical and ne

global_riverlm_both=lm(linfst~riverdist*meta2+fprop+gprop+dprop+cprop+iprop+obstruct+reservoir+ne,data=full_metaprune1)

dd_meta_both=dredge(global_riverlm_both)

####### slopes/intercepts vs ne

effpops$intercepts=NA
effpops$slopes=NA

effpops$intercepts[1]=best_riverlm$coefficients["(Intercept)"]
effpops$slopes[1]=best_riverlm$coefficients["riverdist"]
effpops$intercepts[2]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Middle Arkansas River"]
effpops$slopes[2]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Middle Arkansas River"]
effpops$intercepts[3]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Salt Fork Arkansas River"]
effpops$slopes[3]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Salt Fork Arkansas River"]
effpops$intercepts[4]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Upper Arkansas River"]
effpops$slopes[4]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Upper Arkansas River"]
effpops$intercepts[5]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Lower Arkansas River"]
effpops$slopes[5]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Lower Arkansas River"]
effpops$intercepts[6]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Rattlesnake Creek"]
effpops$slopes[6]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Rattlesnake Creek"]
effpops$intercepts[7]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Cimarron River"]
effpops$slopes[7]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Cimarron River"]
effpops$intercepts[8]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Chikaskia River"]
effpops$slopes[8]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Chikaskia River"]
effpops$intercepts[9]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Medicine Lodge River"]
effpops$slopes[9]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Medicine Lodge River"]
effpops$intercepts[10]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2North Fork Ninnescah River"]
effpops$slopes[10]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2North Fork Ninnescah River"]
effpops$intercepts[11]=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2South Fork Ninnescah River"]
effpops$slopes[11]=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2South Fork Ninnescah River"]

plot(effpops$NE,effpops$intercepts)
plot(effpops$NE,effpops$slopes)

## distribution of FSTs
plot(density(full_metaprune1$linfst))
median(full_metaprune1$linfst)
mode(full_metaprune1$linfst)

##correlations btwn vars

cor(full_metaprune1[,c('riverdist','fprop','gprop','dprop','cprop','iprop')])

############################
#####PLOTTING IBD###########
############################

par(mfrow=c(1,1),mai=c(1,1,1,3))

plot(full_metaprune1$riverdist,full_metaprune1$linfst,xlab="Distance(m)",ylab=expression("F"[ST]*"(/1-F"[ST]*")"),col="gray",cex=0.5,pch=19)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Chikaskia River"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Chikaskia River"],col="#FC8D62",lwd=2)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Cimarron River"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Cimarron River"],col="#66C2A5",lwd=2)
abline(a=best_riverlm$coefficients["(Intercept)"],b=best_riverlm$coefficients["riverdist"],col="#FFD92F",lwd=2)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Lower Arkansas River"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Lower Arkansas River"],col="#8DA0CB",lwd=2)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Medicine Lodge River"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Medicine Lodge River"],col="#E78AC3",lwd=2)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Middle Arkansas River"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Middle Arkansas River"],col="#FFD92F",lwd=2,lty=2)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Upper Arkansas River"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Upper Arkansas River"],col="#FFD92F",lwd=2,lty=4)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2North Fork Ninnescah River"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2North Fork Ninnescah River"],col="#E5C494",lwd=2)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2South Fork Ninnescah River"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2South Fork Ninnescah River"],col="#E5C494",lwd=2,lty=2)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Salt Fork Arkansas River"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Salt Fork Arkansas River"],col="#A6D854",lwd=2)
abline(a=best_riverlm$coefficients["(Intercept)"]+best_riverlm$coefficients["meta2Rattlesnake Creek"],b=best_riverlm$coefficients["riverdist"]+best_riverlm$coefficients["riverdist:meta2Rattlesnake Creek"],col="#8DA0CB",lwd=2,lty=2)

legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,
       c("Big Sandy / Rush Creeks","Middle Arkansas","Upper Arkansas","Lower Arkansas","Rattlesnake","Chikasia","Cimarron","Medicine Lodge","North Fork Ninnescah","South Fork Ninnescah","Salt Fork"),col=c("#FFD92F","#FFD92F","#FFD92F","#8DA0CB","#8DA0CB","#FC8D62","#66C2A5","#E78AC3","#E5C494","#E5C494","#A6D854"), lwd=c(2,2,2,2,2,2,2,2,2,2,2), lty=c(1,2,4,1,2,1,1,1,1,2,1))

