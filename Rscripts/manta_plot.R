coloindel=read.table("analysis_outputs/manta_indelsizes/colorado_indelsizes")
walindel=read.table("analysis_outputs/manta_indelsizes/walnut_indelsizes")
arkindel=read.table("analysis_outputs/manta_indelsizes/lowerark_indelsizes")
ninindel=read.table("analysis_outputs/manta_indelsizes/ninnescah_indelsizes")
chikindel=read.table("analysis_outputs/manta_indelsizes/chikaskia_indelsizes")
medindel=read.table("analysis_outputs/manta_indelsizes/medicine_indelsizes")
saltindel=read.table("analysis_outputs/manta_indelsizes/saltfork_indelsizes")
cimindel=read.table("analysis_outputs/manta_indelsizes/cimarron_indelsizes")
springindel=read.table("analysis_outputs/manta_indelsizes/spring_indelsizes")
nfnindel=read.table("analysis_outputs/manta_indelsizes/nfnin_indelsizes")
sfnindel=read.table("analysis_outputs/manta_indelsizes/sfnin_indelsizes")
larkindel=read.table("analysis_outputs/manta_indelsizes/lark_indelsizes")
rattleindel=read.table("analysis_outputs/manta_indelsizes/rattle_indelsizes")



metafsts=read.table("analysis_outputs/metafsts.txt",header=F)

fstvsindel=metafsts[metafsts$V1=="Colorado" | metafsts$V2=="Colorado" ,]

fstvsindel$nindel=rep(NA,nrow(fstvsindel))

fstvsindel$nindel[1]=nrow(chikindel)
fstvsindel$nindel[2]=nrow(cimindel)
fstvsindel$nindel[3]=nrow(larkindel)
fstvsindel$nindel[4]=nrow(medindel)
fstvsindel$nindel[5]=nrow(nfnindel)
fstvsindel$nindel[6]=nrow(rattleindel)
fstvsindel$nindel[7]=nrow(saltindel)
fstvsindel$nindel[8]=nrow(sfnindel)
fstvsindel$nindel[9]=nrow(springindel)
fstvsindel$nindel[10]=nrow(walindel)

coldat=c("Colorado","Colorado",0,0,0)
fstfin=rbind(fstvsindel,coldat)
fstfin$V2[1]="Chikaskia"
fstfin$V2[2]="Cimarron"

fstfin$nindel[11]=nrow(coloindel)

fstfin$col=c("#FF6D00","#FFDB00","#49FF00","#00FF24","#00FFFF","#0092FF","#0024FF","#4900FF","#B600FF","#FF006D","#FF00DB")
fstfin$nindel=as.integer(fstfin$nindel)
fstfin$V4=as.numeric(fstfin$V4)

#plot(fstfin$V3,fstfin$nindel)

plot(fstfin$V4,fstfin$nindel,xlab="FST relative to Colorado",ylab="# of indels",pch=19,col=fstfin$col)

#lm1=lm(nindel~V3,data=fstvsindel)
lm_fst=lm(nindel~V4,data=fstfin)


fstfin$divtime=c(2.81,2.81,1.37,2.81,2.81,0.94,2.81,2.81,5.69,0.62,0.35)

plot(fstfin$divtime,fstfin$nindel,xlab="Divergence time relative to Colorado (mya)",ylab="# of indels",pch=19,col=fstfin$col)

lm_div=lm(nindel~divtime,data=fstfin)


metafst=read.table("analysis_outputs/metapop_WCFST")

colo_ave=(metafst$V1+metafst$V7+metafst$V13)/3

0.2177414+0.2984260+0.1316401

fstfin$FST[1]=colo_ave[2]
fstfin$FST[2]=colo_ave[3]
fstfin$FST[3]=colo_ave[5]
fstfin$FST[4]=colo_ave[6]
fstfin$FST[5]=colo_ave[8]
fstfin$FST[6]=colo_ave[9]
fstfin$FST[7]=colo_ave[10]
fstfin$FST[8]=colo_ave[11]
fstfin$FST[9]=colo_ave[12]
fstfin$FST[10]=colo_ave[14]
fstfin$FST[11]=colo_ave[12]
fstfin$FST[11]=(0.2177414+0.2984260+0.1316401)/3

fstfin$col2=c("#FF0000","#FF8B00","#00B9FF","#00FF2E","#5D00FF","#00B9FF","#002EFF","#5D00FF","#FF00DB","#00B9FF","#E8FF00")

par(mai=c(1.1,1.1,1.1,1.1),xpd=FALSE)

plot(fstfin$FST,fstfin$nindel,xlab="FST relative to Colorado reference",ylab="# of indels relative to Colorado reference",pch=19,col=fstfin$col2,cex=1.8,cex.lab=1.5,cex.axis=1.5)

lm_fst=lm(nindel~FST,data=fstfin)

#get predicted y values using regression equation
newx <- seq(min(fstfin$FST), max(fstfin$nindel), length.out=100)
preds <- predict(lm_fst, newdata = data.frame(FST=newx), interval = 'confidence')

#add fitted regression line
abline(lm_fst,xlim,lty=2)

#add dashed lines for confidence bands
lines(newx, preds[ ,3], lty = 'dashed', col = 'blue')
lines(newx, preds[ ,2], lty = 'dashed', col = 'blue')

library(colorspace)

fstfin$col2=c("#FC8D62","#66C2A5","#8DA0CB","#E78AC3","#E5C494","#8DA0CB","#A6D854","#E5C494","#B3B3B3","#8DA0CB","#FFD92F")

fstfin$shape=c(21,21,21,21,22,23,21,21,21,22,21)

fstfin$tcols=adjust_transparency(fstfin$col2,alpha=0.8)

fstfin$sizes=(fstfin$shape/10.5)+((fstfin$shape-21)*0.3)


plot(fstfin$divtime,fstfin$nindel,ylim=c(0,15000),xlim=c(0,6),xlab="Divergence time relative to Colorado reference",ylab="# of indels relative to Colorado reference",pch=fstfin$shape,bg=fstfin$tcols,cex=fstfin$sizes,cex.lab=1.5,cex.axis=1.5)

lm_div=lm(nindel~divtime,data=fstfin)

abline(lm_div,xlim,lty=2)

plot(fstfin$FST,fstfin$nindel,ylim=c(0,15000),xlim=c(0,0.8),xlab="FST relative to Colorado reference",ylab="# of indels relative to Colorado reference",pch=fstfin$shape,bg=fstfin$tcols,cex=fstfin$sizes,cex.lab=1.5,cex.axis=1.5)


par(mfrow=c(1,2),mai=c(1,1,1,0))
plot(fstfin$divtime,fstfin$nindel,ylim=c(0,15000),xlim=c(0,6),xlab="Divergence time",ylab="# of indels",pch=fstfin$shape,bg=fstfin$tcols,cex=fstfin$sizes,cex.lab=1.5,cex.axis=1.5)
plot(fstfin$FST,fstfin$nindel,ylim=c(0,15000),xlim=c(0,0.8),xlab="FST",ylab="",pch=fstfin$shape,bg=fstfin$tcols,cex=fstfin$sizes,cex.lab=1.5,cex.axis=1.5)
