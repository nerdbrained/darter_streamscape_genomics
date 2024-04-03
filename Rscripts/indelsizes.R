library("ggplot2")
#install.packages("ggridges")
library("ggridges")

chik=read.table("analysis_outputs/manta_indelsizes/chikaskia_indelsizes")
cim=read.table("analysis_outputs/manta_indelsizes/cimarron_indelsizes")
col=read.table("analysis_outputs/manta_indelsizes/colorado_indelsizes")
lark=read.table("analysis_outputs/manta_indelsizes/lark_indelsizes")
lower=read.table("analysis_outputs/manta_indelsizes/lowerark_indelsizes")
med=read.table("analysis_outputs/manta_indelsizes/medicine_indelsizes")
nfnin=read.table("analysis_outputs/manta_indelsizes/nfnin_indelsizes")
ninnescah=read.table("analysis_outputs/manta_indelsizes/ninnescah_indelsizes")
rattle=read.table("analysis_outputs/manta_indelsizes/rattle_indelsizes")
saltfork=read.table("analysis_outputs/manta_indelsizes/saltfork_indelsizes")
sfnin=read.table("analysis_outputs/manta_indelsizes/sfnin_indelsizes")
spring=read.table("analysis_outputs/manta_indelsizes/spring_indelsizes")
walnut=read.table("analysis_outputs/manta_indelsizes/walnut_indelsizes")

allsizes=c(chik,cim,col,lark,med,nfnin,rattle,saltfork,sfnin,spring,walnut)
allpops=c(rep("Chikaskia",nrow(chik)),rep("Cimarron",nrow(cim)),rep("Colorado",nrow(col)),rep("Lower Arkansas",nrow(lark)),rep("Medicine Lodge",nrow(med)),rep("North Fork Ninnescah",nrow(nfnin)),rep("Rattlesnake",nrow(rattle)),rep("Salt Fork",nrow(saltfork)),rep("South Fork Ninnescah",nrow(sfnin)),rep("Spring",nrow(spring)),rep("Walnut",nrow(walnut)))
allindels=data.frame(allpops,unlist(allsizes))
names(allindels)=c("Metapopulation","IndelSizes")

ggplot(allindels, aes(x = IndelSizes, y = Metapopulation, group = Metapopulation)) + 
  geom_density_ridges(fill = "#00AFBB")

allindels$AbsoluteIndelSizes=abs(allindels$IndelSizes)

ggplot(allindels, aes(x = AbsoluteIndelSizes, y = Metapopulation, group = Metapopulation)) + 
  geom_density_ridges(fill = "#00AFBB")
