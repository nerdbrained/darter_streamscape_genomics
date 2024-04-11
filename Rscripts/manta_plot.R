# get # of indels (length of MANTA table)

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

#get FSTs

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

fstfin$nindel=as.integer(fstfin$nindel)

fstfin$divtime=c(2.81,2.81,1.37,2.81,2.81,0.94,2.81,2.81,5.69,0.62,0.35)

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

# plot indels vs FST and divergence time

library(colorspace)

fstfin$col2=c("#FC8D62","#66C2A5","#8DA0CB","#E78AC3","#E5C494","#8DA0CB","#A6D854","#E5C494","#B3B3B3","#8DA0CB","#FFD92F")

fstfin$shape=c(21,21,21,21,22,23,21,21,21,22,21)

fstfin$tcols=adjust_transparency(fstfin$col2,alpha=0.8)

fstfin$sizes=(fstfin$shape/10.5)+((fstfin$shape-21)*0.3)


par(mfrow=c(1,2),mai=c(1,1,1,0))
plot(fstfin$divtime,fstfin$nindel,ylim=c(0,15000),xlim=c(0,6),xlab="Divergence time",ylab="# of indels",pch=fstfin$shape,bg=fstfin$tcols,cex=fstfin$sizes,cex.lab=1.5,cex.axis=1.5)
plot(fstfin$FST,fstfin$nindel,ylim=c(0,15000),xlim=c(0,0.8),xlab="FST",ylab="",pch=fstfin$shape,bg=fstfin$tcols,cex=fstfin$sizes,cex.lab=1.5,cex.axis=1.5)
