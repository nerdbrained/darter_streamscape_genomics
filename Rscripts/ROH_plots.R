library(colorspace)

sitelocs=read.csv("metadata/sitelocs.csv")
indsites=read.csv("metadata/indsites_meta3.csv",header=T)
#indsites$long<-sitelocs$x[match(indsites$site,sitelocs$SITE.NAME)]
#indsites$lat<-sitelocs$y[match(indsites$site,sitelocs$SITE.NAME)]
#plot(x=NULL, y=NULL, xlim=c(-105,-94),ylim=c(36,40))
#points(indsites$long,indsites$lat,cex=0.3)
indsitesWGS=read.csv("metadata/indlist_WGS.csv",header=T)
#indsitesWGS$long<-sitelocs$x[match(indsitesWGS$site,sitelocs$SITE.NAME)]
#indsitesWGS$lat<-sitelocs$y[match(indsitesWGS$site,sitelocs$SITE.NAME)]
#points(indsitesWGS$long,indsitesWGS$lat,pch=4,col="red")

metalist=read.csv("metadata/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)
metalist$V3=c(1,2,1,2,1,2,1,2,1,2,1,2,1,2)
indsitesWGS$col<-metalist$V2[match(indsitesWGS$meta,metalist$V1)]
indsitesWGS$pch<-metalist$V3[match(indsitesWGS$meta,metalist$V1)]



#1 het / 100 kb allowed

darterlroh=read.table("analysis_outputs/darterlroh_100kb_fin.hom.indiv",header=T)
darterhet=read.table("analysis_outputs/darterhet.txt",header=T)

indsitesWGS$roh=darterlroh$NSEG
indsitesWGS$het=(darterhet$N.NM.-darterhet$O.HOM.)/darterhet$N.NM.


plot(x=NULL, y=NULL, xlim=c(0,0.3),ylim=c(0,60),xlab="Heterozygosity",ylab="ROH (≥100kb)",main="Genome-wide Heterozygosity vs ROH")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
points(indsitesWGS$het,indsitesWGS$roh,col=indsitesWGS$col,cex=2,pch=indsitesWGS$pch)
legend(0.2,55,legend=metalist$V1,col=metalist$V2,cex=1,pch=metalist$V3)

angsdhet=read.table("analysis_outputs/EcWGS_angsdhet.txt",header=T)

indsitesWGS$angsdhet=(angsdhet$N.NM.-angsdhet$O.HOM.)/angsdhet$N.NM.

plot(x=NULL, y=NULL, xlim=c(0,0.5),ylim=c(0,0.5),xlab="BBMAP Heterozygosity",ylab="ANGSD Heterozygosity",main="BBMAP Heterozygosity vs ANGSD Heterozygosity")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
points(indsitesWGS$het,indsitesWGS$angsdhet,col=indsitesWGS$col,cex=2,pch=indsitesWGS$pch)
legend(0.3,0.3,legend=metalist$V1,col=metalist$V2,cex=1,pch=metalist$V3)
lines(x=c(0,0.5),y=c(0,0.5),lty=2)

plot(darterhet$O.HOM.,angsdhet$O.HOM.)
plot(darterhet$N.NM.,angsdhet$N.NM.)

rong=c(seq(4,24),1,2,3)

#indsitesWGS$depth=depth$all[rong]

#plot(indsitesWGS$depth,angsdhet$O.HOM.)
#plot(indsitesWGS$depth,angsdhet$N.NM.)


#### ROHan theta + ROH

rohandat=read.csv("analysis_outputs/rohandat_100k.csv",header=T)

indsitesWGS$rohan_inROH<-rohandat$rohan_roh[match(indsitesWGS$ind,rohandat$ind)]

indsitesWGS$rohan_theta<-rohandat$rohan_theta[match(indsitesWGS$ind,rohandat$ind)]

indsitesWGS$plink_inROH=darterlroh$KB/6431

plot(indsitesWGS$angsdhet,indsitesWGS$rohan_theta,col=indsitesWGS$col,cex=2,pch=indsitesWGS$pch,xlab="Heterozygosity",ylab="ROHan Theta",cex.axis=1.2,cex.lab=1.2)
legend(0.3,0.003,legend=metalist$V1,col=metalist$V2,cex=1,pch=metalist$V3)

#plot(indsitesWGS$long,indsitesWGS$rohan_theta,col=indsitesWGS$col,cex=2,pch=indsitesWGS$pch,xlab="Longitude",ylab="ROHan Theta",cex.axis=1.2,cex.lab=1.2)
#legend(-104,0.0035,legend=metalist$V1,col=metalist$V2,cex=1,pch=metalist$V3)

#plot(indsitesWGS$long,indsitesWGS$angsdhet,col=indsitesWGS$col,cex=2,pch=indsitesWGS$pch,xlab="Longitude",ylab="Heterozygosity",cex.axis=1.2,cex.lab=1.2)
#legend(-104,0.35,legend=metalist$V1,col=metalist$V2,cex=1,pch=metalist$V3)

#plot(indsitesWGS$angsdhet,indsitesWGS$plink_inROH,col=indsitesWGS$col,cex=2,pch=indsitesWGS$pch,xlab = "Heterozygosity", ylab= "% of genome in ROH (PLINK)",cex.axis=1.2,cex.lab=1.2)
#legend(0.3,0.8,legend=metalist$V1,col=metalist$V2,cex=1,pch=metalist$V3)
       
plot(indsitesWGS$rohan_theta,indsitesWGS$rohan_inROH, col=indsitesWGS$col,cex=2,pch=indsitesWGS$pch,xlab = "ROHan Theta", ylab= "% of genome in ROH (ROHan)",cex.axis=1.2,cex.lab=1.2)
legend(0.003,0.4,legend=metalist$V1,col=metalist$V2,cex=1,pch=metalist$V3)


plot(indsitesWGS$plink_inROH,indsitesWGS$rohan_inROH, col=indsitesWGS$col,cex=2,pch=indsitesWGS$pch,xlab = "ROHan Theta", ylab= "% of genome in ROH (ROHan)",cex.axis=1.2,cex.lab=1.2)
legend(0.3,0.4,legend=metalist$V1,col=metalist$V2,cex=1,pch=metalist$V3)



cols=c("#5D00FF","#FF0000","#FF0000","#FF0000","#5D00FF","#FF8B00","#002EFF","#5D00FF","#00B9FF","#FF8B00","#00FF2E","#00FF2E","#00B9FF","#00B9FF","#FF8B00","#00B9FF","#5D00FF","#FF00DB","#5D00FF","#002EFF","#00B9FF","#E8FF00","#E8FF00","#00B9FF")

plot(indsitesWGS$angsdhet,indsitesWGS$plink_inROH,bg=cols,cex=2,pch=21,ylim=c(0,1.1),xlab="WGS Heterozygosity",ylab="% of genome in ROH (plink)",cex.axis=1.2,cex.lab=1.2)

plot(indsitesWGS$rohan_theta,indsitesWGS$rohan_inROH, bg=cols,cex=2,pch=21,ylim=c(0,1.1),xlab = "ROHan Theta", ylab= "% of genome in ROH (ROHan)",cex.axis=1.2,cex.lab=1.2)


par(mfrow=c(1,2),mai=c(0.9,0.9,0.2,0.2))


cols=c("#E5C494","#FC8D62","#FC8D62","#FC8D62","#E5C494","#66C2A5","#A6D854","#E5C494","#8DA0CB","#66C2A5","#E78AC3","#E78AC3","#8DA0CB","#8DA0CB","#FC8D62","#8DA0CB","#E5C494","#B3B3B3","#E5C494","#A6D854","#8DA0CB","#FFD92F","#FFD92F","#8DA0CB")
tcols=adjust_transparency(cols,alpha=0.8)

shapes=c(21,21,21,21,22,21,21,22,21,21,21,21,22,22,21,23,21,21,21,21,21,21,22,21)

sizes=(shapes/10.5)+((shapes-21)*0.3)

plot(indsitesWGS$angsdhet,indsitesWGS$plink_inROH,bg=tcols,cex=sizes,pch=shapes,ylim=c(0,1.1),xlab="WGS Heterozygosity",ylab="% of genome in ROH (plink)",cex.axis=1.2,cex.lab=1.2)

plot(indsitesWGS$rohan_theta,indsitesWGS$rohan_inROH, bg=tcols,cex=sizes,pch=shapes,ylim=c(0,1.1),xlab = "ROHan Theta", ylab= "% of genome in ROH (ROHan)",cex.axis=1.2,cex.lab=1.2)

wgsmetas=metalist$V1[-c(4,7)]


lcols=c("#FFD92F","#FC8D62","#66C2A5","#8DA0CB","#E78AC3","#E5C494","#8DA0CB","#A6D854","#E5C494","#B3B3B3","#FFD92F","#8DA0CB")
#lshapes=c(21,21,21,21,21,21,23,21,22,21,22,22)
lshapes=c(19,19,19,19,19,19,18,19,15,19,15,15)


legend(x=0.0022,y=1,legend=wgsmetas,pch=lshapes,col=lcols,bty="n")
