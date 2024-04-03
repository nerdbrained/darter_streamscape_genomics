rhc=read.csv("~/Desktop/rapture/wgs/Ec_ROH/rohancov.csv")

par(mfrow=c(3,1),mai=c(0.7,0.7,0.1,0.1))

plot(rhc$all,rhc$rohan_theta,ylab="ROHan theta",xlab="Depth")
rlm=lm(rohan_theta~all,data=rhc)
summary(rlm)

plot(rhc$all,rhc$rohan_unclass,ylab="ROHan % unclassified",xlab="Depth")
rlm=lm(rohan_unclass~all,data=rhc)
summary(rlm)

plot(rhc$rohan_theta,rhc$rohan_unclass,ylab="ROHan % unclassified",xlab="ROHan theta")
rlm=lm(rohan_unclass~all,data=rhc)
summary(rlm)

