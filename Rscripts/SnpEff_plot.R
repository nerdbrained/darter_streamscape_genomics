snpeff=read.csv("analysis_outputs/SnpEffdata.csv",header=TRUE)
indids=read.table("metadata/WGS_indnums.txt",header=FALSE)
snpeff$indids=indids

indsitesWGS=read.csv("metadata/indlist_WGS.csv",header=T)
indsitesWGS$ind=gsub("Ecr0","",indsitesWGS$ind)

snpeff$meta=indsitesWGS$meta

metalist=read.csv("metadata/metalist2.csv",header=F)
metalist$V2=rainbow(n=14)

snpeff$col=metalist$V2[match(snpeff$meta,metalist$V1)]

snpeff$nullHo=snpeff$null_het/(snpeff$null_homref+snpeff$null_homalt+snpeff$null_het)
snpeff$nullsites=snpeff$null_homref+snpeff$null_homalt+snpeff$null_het
snpeff$lowalleles=snpeff$low_het+2*snpeff$low_hom
snpeff$modalleles=snpeff$mod_het+2*snpeff$mod_hom
snpeff$highalleles=snpeff$high_het+2*snpeff$high_hom

#plot(snpeff$nullHo,snpeff$low_hom)
#plot(snpeff$nullHo,snpeff$low_het)
#plot(snpeff$nullHo,snpeff$mod_hom)
#plot(snpeff$nullHo,snpeff$mod_het)
#plot(snpeff$nullHo,snpeff$high_hom)
#plot(snpeff$nullHo,snpeff$high_het)

snpeff$low_homprop=snpeff$low_hom/snpeff$nullsites
snpeff$low_hetprop=snpeff$low_het/snpeff$nullsites
snpeff$mod_homprop=snpeff$mod_hom/snpeff$nullsites
snpeff$mod_hetprop=snpeff$mod_het/snpeff$nullsites
snpeff$high_homprop=snpeff$high_hom/snpeff$nullsites
snpeff$high_hetprop=snpeff$high_het/snpeff$nullsites

snpeff$low_alleleprop=snpeff$lowalleles/snpeff$nullsites
snpeff$mod_alleleprop=snpeff$modalleles/snpeff$nullsites
snpeff$high_alleleprop=snpeff$highalleles/snpeff$nullsites

snpeff$null_homprop=(snpeff$null_homref+snpeff$null_homalt)/snpeff$nullsites
snpeff$null_hetprop=snpeff$null_het/snpeff$nullsites

#plot(snpeff$nullHo,snpeff$low_homprop)
#plot(snpeff$nullHo,snpeff$low_hetprop)
#plot(snpeff$nullHo,snpeff$mod_homprop)
#plot(snpeff$nullHo,snpeff$mod_hetprop)
#plot(snpeff$nullHo,snpeff$high_homprop)
#plot(snpeff$nullHo,snpeff$high_hetprop)
#plot(snpeff$nullHo,snpeff$null_homprop)
#plot(snpeff$nullHo,snpeff$null_hetprop)

#plot(snpeff$nullHo,snpeff$low_alleleprop)
#plot(snpeff$nullHo,snpeff$mod_alleleprop)
#plot(snpeff$nullHo,snpeff$high_alleleprop)

par(mai=c(0.6,0.7,0.2,0.1),mfrow=c(3,2))
plot(snpeff$nullHo,snpeff$low_homprop,xlab="",ylab="Low Effect Homozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$low_hetprop,xlab="",ylab="Low Effect Heterozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$mod_homprop,xlab="",ylab="Moderate Effect Homozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$mod_hetprop,xlab="",ylab="Moderate Effect Heterozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$high_homprop,xlab="Heterozygosity",ylab="High Effect Homozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$high_hetprop,xlab="Heterozygosity",ylab="High Effect Heterozygotes",pch=19,col=snpeff$col,cex.lab=1.1)

par(mai=c(0.6,0.7,0.2,0.1),mfrow=c(3,2))
plot(snpeff$nullHo,snpeff$low_hom,xlab="",ylab="Low Effect Homozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$low_het,xlab="",ylab="Low Effect Heterozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$mod_hom,xlab="",ylab="Moderate Effect Homozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$mod_het,xlab="",ylab="Moderate Effect Heterozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$high_hom,xlab="Heterozygosity",ylab="High Effect Homozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,snpeff$high_het,xlab="Heterozygosity",ylab="High Effect Heterozygotes",pch=19,col=snpeff$col,cex.lab=1.1)



par(mai=c(0.6,0.7,0.2,0.1),mfrow=c(3,1))
plot(snpeff$nullHo,(2*snpeff$low_hom+snpeff$low_het)/snpeff$nullsites,xlab="",ylab="Low Effect Homozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,(2*snpeff$mod_hom+snpeff$mod_het)/snpeff$nullsites,xlab="",ylab="Moderate Effect Heterozygotes",pch=19,col=snpeff$col,cex.lab=1.1)
plot(snpeff$nullHo,(2*snpeff$high_hom+snpeff$high_het)/snpeff$nullsites,xlab="",ylab="High Effect Homozygotes",pch=19,col=snpeff$col,cex.lab=1.1)

snpeff$col=rohandat$col
snpeffnoSpring=snpeff[-18,]


snpeff$lowpersnp=(2*snpeff$low_hom+snpeff$low_het)/snpeff$nullsites
snpeff$highpersnp=(2*snpeff$high_hom+snpeff$high_het)/snpeff$nullsites
snpeff$modpersnp=(2*snpeff$mod_hom+snpeff$mod_het)/snpeff$nullsites


par(mai=c(0.6,0.7,0.2,0.1),mfrow=c(1,3))
plot(snpeff$nullHo,(2*snpeff$low_hom+snpeff$low_het)/snpeff$nullsites,xlab="",ylab="Low Effect Alleles / SNP",pch=19,col=snpeff$col,cex=2,cex.axis=1.2,cex.lab=1.5)
lowlm=lm(lowpersnp~nullHo,data=snpeff)
abline(lowlm,lty=2)
plot(snpeff$nullHo,(2*snpeff$mod_hom+snpeff$mod_het)/snpeff$nullsites,ylab="Moderate Effect Alleles / SNP",xlab="Heterozygosity",pch=19,col=snpeff$col,cex=2,cex.axis=1.2,cex.lab=1.5)
modlm=lm(modpersnp~nullHo,data=snpeff)
abline(modlm,lty=2)
plot(snpeff$nullHo,(2*snpeff$high_hom+snpeff$high_het)/snpeff$nullsites,ylab="High Effect Alleles / SNP",xlab="",pch=19,col=snpeff$col,cex=2,cex.axis=1.2,cex.lab=1.5)
highlm=lm(highpersnp~nullHo,data=snpeff)
abline(highlm,lty=2)

par(mai=c(0.6,0.7,0.2,0.1),mfrow=c(1,3))

cols=c("#E5C494","#FC8D62","#FC8D62","#FC8D62","#E5C494","#66C2A5","#A6D854","#E5C494","#8DA0CB","#66C2A5","#E78AC3","#E78AC3","#8DA0CB","#8DA0CB","#66C2A5CC","#8DA0CB","#E5C494","#B3B3B3","#E5C494","#A6D854","#8DA0CB","#FFD92F","#FFD92F","#8DA0CB")
tcols=adjust_transparency(cols,alpha=0.8)
shapes=c(21,21,21,21,22,21,21,22,21,21,21,21,22,22,21,23,21,21,21,21,21,21,22,21)
sizes=(shapes/10.5)+((shapes-21)*0.3)

cols_nospring=c("#E5C494","#FC8D62","#FC8D62","#FC8D62","#E5C494","#66C2A5","#A6D854","#E5C494","#8DA0CB","#66C2A5","#E78AC3","#E78AC3","#8DA0CB","#8DA0CB","#66C2A5CC","#8DA0CB","#E5C494","#E5C494","#A6D854","#8DA0CB","#FFD92F","#FFD92F","#8DA0CB")
tcols_nospring=adjust_transparency(cols_nospring,alpha=0.8)
shapes_nospring=c(21,21,21,21,22,21,21,22,21,21,21,21,22,22,21,23,21,21,21,21,21,22,21)
sizes_nospring=(shapes/10.5)+((shapes-21)*0.3)

snpeffnoSpring$cols=tcols_nospring
snpeffnoSpring$shapes=shapes_nospring

snpeff$cols=tcols
snpeff$shapes=shapes


plot(snpeff$nullHo,(2*snpeff$low_hom+snpeff$low_het)/snpeff$nullsites,xlab="",ylab="Low Effect Alleles / SNP",pch=shapes,bg=tcols,cex=2,cex.axis=1.2,cex.lab=1.5)
lowlm=lm(lowpersnp~nullHo,data=snpeff)
abline(lowlm,lty=2)
plot(snpeff$nullHo,(2*snpeff$mod_hom+snpeff$mod_het)/snpeff$nullsites,ylab="Moderate Effect Alleles / SNP",xlab="Heterozygosity",pch=shapes,bg=tcols,cex=2,cex.axis=1.2,cex.lab=1.5)
modlm=lm(modpersnp~nullHo,data=snpeff)
abline(modlm,lty=2)
plot(snpeff$nullHo,(2*snpeff$high_hom+snpeff$high_het)/snpeff$nullsites,ylab="High Effect Alleles / SNP",xlab="",pch=shapes,bg=tcols,cex=2,cex.axis=1.2,cex.lab=1.5)
highlm=lm(highpersnp~nullHo,data=snpeff)
abline(highlm,lty=2)


snpeff$metafactor=factor(c("South Fork Ninnescah","Chikaskia","Chikaskia","Chikaskia","North Fork Ninnescah","Cimarron","Salt Fork","North Fork Ninnescah","Lower Arkansas","Cimarron","Medicine Lodge","Medicine Lodge","Walnut","Walnut","Cimarron","Rattlesnake","South Fork Ninnescah","Spring","South Fork Ninnescah","Salt Fork","Lower Arkansas","Big Sandy / Rush","Upper Arkansas","Lower Arkansas"),levels=c("Big Sandy / Rush","Upper Arkansas","Walnut","Rattlesnake","Lower Arkansas","Salt Fork","Cimarron","Chikaskia","Medicine Lodge","North Fork Ninnescah","South Fork Ninnescah","Spring"))

par(mai=c(0.1,0.6,0.2,0.1),oma=c(0,2,0,0),mfrow=c(3,1))

plot(x=NULL,y=NULL,xlim=c(1,12),ylim=c(0.00005,0.00025),ylab="High Effect",xlab="",cex.axis=1,cex.lab=1.5,xaxt='n')
points(snpeff$metafactor,(2*snpeff$high_hom+snpeff$high_het)/snpeff$nullsites,pch=shapes,bg=tcols,cex=2)

plot(x=NULL,y=NULL,xlim=c(1,12),ylim=c(0.001,0.0105),ylab="Moderate Effect",xlab="",cex.axis=1,cex.lab=1.5,xaxt='n')
points(snpeff$metafactor,(2*snpeff$mod_hom+snpeff$mod_het)/snpeff$nullsites,pch=shapes,bg=tcols,cex=2)

plot(x=NULL,y=NULL,xlim=c(1,12),ylim=c(0.004,0.017),ylab="Low Effect",xlab="",cex.axis=1,cex.lab=1.5,xaxt='n')
points(snpeff$metafactor,(2*snpeff$low_hom+snpeff$low_het)/snpeff$nullsites,pch=shapes,bg=tcols,cex=2)

mtext('Alleles/SNP',at=.5,side=2,outer=T,cex=1)
#mtext(c("Big Sandy / Rush","Upper Arkansas","Walnut","Rattlesnake","Lower Arkansas","Salt Fork","Cimarron","Chikaskia","Medicine Lodge","North Fork Ninnescah","South Fork Ninnescah","Spring"),at=seq(1,12)/12,side=1,srt=45,outer=T,xpd=NA)
