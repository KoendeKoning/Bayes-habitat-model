# GIS libraries
library(raster)
library(rgdal)
library(sp)

setwd("C:/Users/konin056/Documents/Sensing Clues/Paper 1/Data")
flist <- list.files()
flist <- flist[substr(flist, nchar(flist)-2,  nchar(flist))=="tif"]

PPB <- flist[substr(flist,12,19)!="HSRaster"]
posterior <- PPB[1]
prior <- PPB[3]
bnaive <- PPB[2]

#flist <- flist[!flist %in% c(posterior, prior, bnaive)]
flist <- flist[substr(flist,12,22)=="HSRaster ID"]

RASTER.posterior <- raster(posterior)
RASTER.prior <- raster(prior)
RASTER.bnaive <- raster(bnaive)

newfactor <- ifelse(RASTER.posterior[]>RASTER.posterior@data@max/4*3,
                    "very suitable",ifelse(RASTER.posterior[]>RASTER.posterior@data@max/4*2,
                                           "suitable",ifelse(RASTER.posterior[]>RASTER.posterior@data@max/4*1,
                                                             "unsuitable","very unsuitable")))
newfactor <- factor(newfactor,levels=c("very unsuitable","unsuitable","suitable","very suitable"))
RASTER.posterior[] <- newfactor

newfactor <- ifelse(RASTER.prior[]>RASTER.prior@data@max/4*3,
                    "very suitable",ifelse(RASTER.prior[]>RASTER.prior@data@max/4*2,
                                           "suitable",ifelse(RASTER.prior[]>RASTER.prior@data@max/4*1,
                                                             "unsuitable","very unsuitable")))
newfactor <- factor(newfactor,levels=c("very unsuitable","unsuitable","suitable","very suitable"))
RASTER.prior[] <- newfactor

newfactor <- ifelse(RASTER.bnaive[]>RASTER.bnaive@data@max/4*3,
                    "very suitable",ifelse(RASTER.bnaive[]>RASTER.bnaive@data@max/4*2,
                                           "suitable",ifelse(RASTER.bnaive[]>RASTER.bnaive@data@max/4*1,
                                                             "unsuitable","very unsuitable")))
newfactor <- factor(newfactor,levels=c("very unsuitable","unsuitable","suitable","very suitable"))
RASTER.bnaive[] <- newfactor

summary(RASTER.bnaive[]==RASTER.prior[])
summary(RASTER.bnaive[]==RASTER.posterior[])
sum(RASTER.bnaive[]==RASTER.posterior[], na.rm=TRUE)

#############
# show maps #
#############

library(leaflet)
# naive bayes
map <- leaflet()
map <- addTiles(map)
map <- addRasterImage(map, RASTER.bnaive, colors = c("red", "orange", "light green", "dark green"), opacity = 0.6)
map
# scientific prior
map <- leaflet()
map <- addTiles(map)
map <- addRasterImage(map, RASTER.prior, colors = c("red", "orange", "light green", "dark green"), opacity = 0.6)
map
# posterior
map <- leaflet()
map <- addTiles(map)
map <- addRasterImage(map, RASTER.posterior, colors = c("red", "orange", "light green", "dark green"), opacity = 0.6)
map

#############

DATA <- data.frame(fileNAME = flist)
for (i in flist) {
  index <- which(flist==i)
  
  raster <- raster(i)
  newraster <- raster > raster@data@max/2
  newfactor <- ifelse(raster[]>raster@data@max/4*3,
                      "very suitable",ifelse(raster[]>raster@data@max/4*2,
                                             "suitable",ifelse(raster[]>raster@data@max/4*1,
                                                               "unsuitable","very unsuitable")))
  newfactor <- factor(newfactor,levels=c("very unsuitable","unsuitable","suitable","very suitable"))
  newraster[] <- newfactor
  
  name <- substr(i,12,nchar(i)-4)
  params <- strsplit(name," ")[[1]][2:3]
  
  #CP <- as.numeric(substr(params[1],3,nchar(params[1])))
  Nobs <- as.numeric(substr(params[2],5,nchar(params[2])))
  
  #DATA$CP[index] <- CP
  DATA$Nobs[index] <- Nobs
  
  DATA$posteriorscore[index] <- sum(newraster[]==RASTER.posterior[], na.rm=TRUE)/ncell(raster)
  DATA$priorscore[index] <- sum(newraster[]==RASTER.prior[], na.rm=TRUE)/ncell(raster)
  DATA$bnaivescore[index] <- sum(newraster[]==RASTER.bnaive[], na.rm=TRUE)/ncell(raster)
  
  DATA$posteriorTP[index] <- sum(newraster[]==RASTER.posterior[]&newraster[]>2, na.rm=TRUE)/sum(newraster[]>2, na.rm=TRUE)
  DATA$priorTP[index] <- sum(newraster[]==RASTER.prior[]&newraster[]>2, na.rm=TRUE)/sum(newraster[]>2, na.rm=TRUE)
  DATA$bnaiveTP[index] <- sum(newraster[]==RASTER.bnaive[]&newraster[]>2, na.rm=TRUE)/sum(newraster[]>2, na.rm=TRUE)
  #plot(newraster, col=c("red","orange","light green", "dark green"),
  #     main=substr(i,12,nchar(i)-4))
  
  #plot(raster, main=substr(i,12,nchar(i)-4))
}

DATA


#plot(posteriorscore~CP,DATA, col="red",ylim=c(0,1), ylab="score", main="Score on the prior and posterior as function of CP")
#points(priorscore~CP,DATA, col="blue")
#lines(smooth.spline(DATA$CP, DATA$posteriorscore, df=5), col="red",lwd=2)
#lines(smooth.spline(DATA$CP, DATA$priorscore, df=5), col="blue",lwd=2)
#legend("topright",c("Prior", "Posterior"), col=c("blue","red"), lwd=2)

plot(posteriorscore~Nobs,DATA, col="red",ylim=c(0,1), ylab="score", main="Score on the prior and posterior as function of Nobs")
points(priorscore~Nobs,DATA, col="blue")
lines(smooth.spline(DATA$Nobs, DATA$posteriorscore, df=5), col="red",lwd=2)
lines(smooth.spline(DATA$Nobs, DATA$priorscore, df=5), col="blue",lwd=2)
legend("topright",c("Prior", "Posterior"), col=c("blue","red"), lwd=2)

#summary(lm(posteriorscore~Nobs+CP,DATA))
#summary(lm(priorscore~Nobs+CP,DATA))

#plot(posteriorTP~CP,DATA, col="red",ylim=c(0,1), ylab="TPR", main="True positive rate on the prior and posterior as function of CP")
#points(priorTP~CP,DATA, col="blue")
#lines(smooth.spline(DATA$CP, DATA$posteriorTP, df=5), col="red",lwd=2)
#lines(smooth.spline(DATA$CP, DATA$priorTP, df=5), col="blue",lwd=2)
#legend("topright",c("Prior", "Posterior"), col=c("blue","red"), lwd=2)

plot(posteriorTP~Nobs,DATA, col="red",ylim=c(0,1), ylab="TPR", main="True positive rate on the prior and posterior as function of Nobs")
points(priorTP~Nobs,DATA, col="blue")
lines(smooth.spline(DATA$Nobs, DATA$posteriorTP, df=5), col="red",lwd=2)
lines(smooth.spline(DATA$Nobs, DATA$priorTP, df=5), col="blue",lwd=2)
legend("topright",c("Prior", "Posterior"), col=c("blue","red"), lwd=2)


getNOBS <- function(x) {
  name <- substr(x,12,nchar(x)-4)
  params <- strsplit(name," ")[[1]][2:3]
  #CP <- as.numeric(substr(params[1],3,nchar(params[1])))
  Nobs <- as.numeric(substr(params[2],5,nchar(params[2])))
  return(Nobs)
}
NOBS <- sapply(flist,getNOBS)

#raster <- raster("2020-07-06 HSRaster CP248 Nobs4.tif")
#raster <- raster("2020-07-06 HSRaster CP4 Nobs90.tif")
#plot(0,0, xlab="FPR", ylab="TPR",col="white",xlim=c(0,1), ylim=c(0,1))
DF.TPR <- NULL
DF.FPR <- NULL

FPRlist <- 1:sum(RASTER.posterior[]<3, na.rm=TRUE)/sum(RASTER.posterior[]<3, na.rm=TRUE)
FPRlist <- unique(round(log(FPRlist),1))
FPRlist <- FPRlist[FPRlist>-10.1]
#FPRlist <- FPRlist[1:length(FPRlist)%%1000==1]
for (i in flist) {
  index <- which(flist==i)
  
  raster <- raster(i)
  
DF <- data.frame(model=raster[],target=RASTER.posterior[])
DF <- DF[!is.na(DF$model)&!is.na(DF$model),]
DF <- DF[order(DF$model, decreasing=TRUE),]
DF$TPR <- cumsum(DF$target>2)/sum(DF$target>2)
DF$FPR <- cumsum(DF$target<3)/sum(DF$target<3)
DF$logFPR <- round(log(DF$FPR),1)
#FPRlist <- unique(DF$FPR)[1:length(unique(DF$FPR))%%1000==1]
#DF <- DF[(1:nrow(DF))%%1000==1,]
DF <- aggregate(TPR~logFPR, DF[DF$logFPR%in%FPRlist,], "max")
DF$FPR <- exp(DF$logFPR)

DF.TPR <- cbind(DF.TPR,DF$TPR)
DF.FPR <- cbind(DF.FPR,DF$FPR)
}
plot(0,0, xlab="log(FPR)", ylab="TPR",col="white",xlim=c(-10,0), ylim=c(0,1))
colours <- colorRampPalette(c("red","yellow","green", "dark green"))
colours <- colours(max(NOBS))
for (i in flist) {
  name <- substr(i,12,nchar(i)-4)
  params <- strsplit(name," ")[[1]][2:3]
  
  #CP <- as.numeric(substr(params[1],3,nchar(params[1])))
  Nobs <- as.numeric(substr(params[2],5,nchar(params[2])))
  
  index <- which(flist==i)
  #red <- sqrt(Nobs/max(DATA$Nobs))
  #green <- sqrt(1-Nobs/max(DATA$Nobs))
  #lines(log(DF.FPR[,index]),DF.TPR[,index], col=rgb(red,green,0))
  lines(log(DF.FPR[,index]),DF.TPR[,index], col=colours[Nobs])
}
#TPRmean <- apply(DF.TPR,1,mean)
#FPRmean <- apply(DF.FPR,1,mean)
#lines(log(FPRmean),TPRmean, lty=2, lwd=2)
lines(seq(-10,0,0.01), exp(seq(-10,0,0.01)), lwd=2, lty=2)

# score of the scientific prior:
DF <- data.frame(model=RASTER.prior[],target=RASTER.posterior[])
DF <- DF[!is.na(DF$model)&!is.na(DF$model),]
DF <- DF[order(DF$model, decreasing=TRUE),]
DF$TPR <- cumsum(DF$target>2)/sum(DF$target>2)
DF$FPR <- cumsum(DF$target<3)/sum(DF$target<3)
DF <- DF[(1:nrow(DF))%%1000==1,]
lines(TPR~log(FPR), DF, lwd=2, col="blue")

# score of the bayesian naive model:
DF <- data.frame(model=RASTER.bnaive[],target=RASTER.posterior[])
DF <- DF[!is.na(DF$model)&!is.na(DF$model),]
DF <- DF[order(DF$model, decreasing=TRUE),]
DF$TPR <- cumsum(DF$target>2)/sum(DF$target>2)
DF$FPR <- cumsum(DF$target<3)/sum(DF$target<3)
DF <- DF[(1:nrow(DF))%%1000==1,]
lines(TPR~log(FPR), DF, lwd=2, col="purple")

# mean group score as function of Nobs:

# visualise clusters:
hist(NOBS, breaks=100, col="blue")
#abline(v=14,col="red")
#abline(v=38,col="red")
#abline(v=51,col="red")
#abline(v=75,col="red")
#abline(v=100,col="red")
abline(v=18,col="red")
#abline(v=42,col="red")
abline(v=51,col="red")
abline(v=75,col="red")
abline(v=94,col="red")
CLUSTER <- ifelse(NOBS<18,"<18",
                  ifelse(NOBS<51,"18-50",
                         ifelse(NOBS<75,"51-74",
                                #ifelse(NOBS<75,"51-74",
                                       ifelse(NOBS<94,"75-93","93<"))))#)
CLUSTER <- factor(CLUSTER, levels=c("<18","18-50","51-74","75-93","93<"))

colours <- colorRampPalette(c("red","yellow","green", "dark green"))
red <- c(1,1,.8,0,0)
green <- c(0,.5,1,1,0.5)
#colours <- colours(5)
colours <- rgb(red,green,0)
plot(0,0, xlab="log(FPR)", ylab="TPR",main="ROC curves of habitat suitability models",col="white",xlim=c(-6,0), ylim=c(0,1))
for (i in levels(CLUSTER)) {
  index <- which(CLUSTER==i)
  
  level <- which(levels(CLUSTER)==i)
  colour <- colours[level]
  
  DFsub.TPR <- DF.TPR[,index]
  DFsub.FPR <- DF.FPR[,index]

  TPRmean <- apply(DFsub.TPR,1,median)
  FPRmean <- apply(DFsub.FPR,1,mean)
  
  # new:
  TPRlower <- apply(DFsub.TPR,1,FUN=quantile, .25,na.rm=TRUE)
  TPRupper <- apply(DFsub.TPR,1,FUN=quantile, .75,na.rm=TRUE)
  polygon(c(log(FPRmean),rev(log(FPRmean))) ,c(TPRupper,rev(TPRlower)), col=rgb(red,green,0,alpha=.2)[level], border=rgb(red,green,0,alpha=.2)[level])
  
  lines(log(FPRmean),TPRmean, lwd=2, col=colour)
  
  print("Summary of cluster:")
  print(i)
  print("TPR at 100 sq km incorrect")
  print(TPRmean[round(log(FPRmean),1)==-3.5])
  print("TPR at 10 sq km incorrect")
  print(TPRmean[round(log(FPRmean),1)==-5.8])
  #lines(log(DF.FPR),DF.TPR[,index], col=rgb(red,green,0))
}

# score of the scientific prior:
DF <- data.frame(model=RASTER.prior[],target=RASTER.posterior[])
DF <- DF[!is.na(DF$model)&!is.na(DF$model),]
DF <- DF[order(DF$model, decreasing=TRUE),]
DF$TPR <- cumsum(DF$target>2)/sum(DF$target>2)
DF$FPR <- cumsum(DF$target<3)/sum(DF$target<3)
DF <- DF[(1:nrow(DF))%%1000==1,]
lines(TPR~log(FPR), DF, lwd=4, col="blue")
print("Summary of science prior:")
print("TPR at 100 sq km incorrect")
print(DF$TPR[round(log(DF$FPR),2)==-3.46])
print("TPR at 10 sq km incorrect")
print(DF$TPR[round(log(DF$FPR),1)==-5.6])

# score of the bayesian naive model:
DF <- data.frame(model=RASTER.bnaive[],target=RASTER.posterior[])
DF <- DF[!is.na(DF$model)&!is.na(DF$model),]
DF <- DF[order(DF$model, decreasing=TRUE),]
DF$TPR <- cumsum(DF$target>2)/sum(DF$target>2)
DF$FPR <- cumsum(DF$target<3)/sum(DF$target<3)
DF <- DF[(1:nrow(DF))%%1000==1,]
lines(TPR~log(FPR), DF, lwd=4, col="purple")
print("Summary of naive bayes:")
print("TPR at 100 sq km incorrect")
print(DF$TPR[round(log(DF$FPR),2)==-3.47])
print("TPR at 10 sq km incorrect")
print(DF$TPR[round(log(DF$FPR),1)==-5.7])

lines(seq(-10,0,0.01), exp(seq(-10,0,0.01)), lwd=2, lty=2)
legend("bottomright",c(levels(CLUSTER),"Prior model", "Naive Bayes' model"),lty=1,lwd=c(2,2,2,2,2,4,4),col=c(colours,"blue","purple"), title="Number of observations")

# lowest score:
plot(0,0, xlab="log(FPR)", ylab="TPR",col="white",main="ROC curves of worst performing habitat suitability models", xlim=c(-6,0), ylim=c(0,1))
for (i in levels(CLUSTER)) {
  index <- which(CLUSTER==i)
  
  level <- which(levels(CLUSTER)==i)
  colour <- colours[level]
  
  DFsub.TPR <- DF.TPR[,index]
  DFsub.FPR <- DF.FPR[,index]
  
  #TPR <- unique(vapply(DFsub.FPR,2,FUN=function(x){unique(x)}))
  #DF <- NULL
  #for (j in 1:ncol(DFsub.FPR)) {
  #  temp <- aggregate(x=DFsub.TPR[,j],by=list(DFsub.FPR[,j]),FUN=max)
  #  names(temp) <- c("FPR","TPR")
  #  DF <- rbind(DF,temp)
  #}
  #DF <- DF[order(DF$FPR),]
  #DF$logFPR <- round(log(DF$FPR),1)
  #lines(aggregate(TPR~logFPR,DF,mean), lwd=2, col=colour)
  
  TPRmin <- apply(DFsub.TPR,1,min)
  FPRmin <- apply(DFsub.FPR,1,min)
  lines(log(FPRmin),TPRmin, lwd=2, col=colour)
  
  print("Summary of cluster:")
  print(i)
  print("TPR at 100 sq km incorrect")
  print(TPRmin[round(log(FPRmin),1)==-3.5])
  print("TPR at 10 sq km incorrect")
  print(TPRmin[round(log(FPRmin),1)==-5.8])
  #lines(log(DF.FPR),DF.TPR[,index], col=rgb(red,green,0))
}
# score of the scientific prior:
DF <- data.frame(model=RASTER.prior[],target=RASTER.posterior[])
DF <- DF[!is.na(DF$model)&!is.na(DF$model),]
DF <- DF[order(DF$model, decreasing=TRUE),]
DF$TPR <- cumsum(DF$target>2)/sum(DF$target>2)
DF$FPR <- cumsum(DF$target<3)/sum(DF$target<3)
DF <- DF[(1:nrow(DF))%%1000==1,]
lines(TPR~log(FPR), DF, lwd=4, col="blue")
lines(seq(-10,0,0.01), exp(seq(-10,0,0.01)), lwd=2, lty=2)
legend("topleft",c(levels(CLUSTER),"Prior model"),lty=1,lwd=c(2,2,2,2,2,4),col=c(colours,"blue"), title="Number of observations")

# how well do the the models perform with a specificity of 100%?
DF.spec100 <- data.frame(cluster=CLUSTER,TPR=DF.TPR[1,],Nobs=NOBS)
table <- aggregate(TPR~cluster,DF.spec100, summary)
table[,-1] <- round(table[,-1], 3)
write.table(table, "Summary table.csv", sep=";")

########################################################################
# NOW MAKE ROC curves on the VERY suitable bear habitat classification #
########################################################################

DF2.TPR <- NULL
DF2.FPR <- NULL

FPRlist <- 1:sum(RASTER.posterior[]<3, na.rm=TRUE)/sum(RASTER.posterior[]<3, na.rm=TRUE)
FPRlist <- unique(round(log(FPRlist),1))
FPRlist <- FPRlist[FPRlist>-10.1]
#FPRlist <- FPRlist[1:length(FPRlist)%%1000==1]
for (i in flist) {
  index <- which(flist==i)
  
  raster <- raster(i)
  
  DF2 <- data.frame(model=raster[],target=RASTER.posterior[])
  DF2 <- DF2[!is.na(DF2$model)&!is.na(DF2$model),]
  DF2 <- DF2[order(DF2$model, decreasing=TRUE),]
  DF2$TPR <- cumsum(DF2$target==4)/sum(DF2$target==4)
  DF2$FPR <- cumsum(DF2$target<4)/sum(DF2$target<4)
  DF2$logFPR <- round(log(DF2$FPR),1)
  #FPRlist <- unique(DF2$FPR)[1:length(unique(DF2$FPR))%%1000==1]
  #DF2 <- DF2[(1:nrow(DF2))%%1000==1,]
  DF2 <- aggregate(TPR~logFPR, DF2[DF2$logFPR%in%FPRlist,], "max")
  DF2$FPR <- exp(DF2$logFPR)
  
  DF2.TPR <- cbind(DF2.TPR,DF2$TPR)
  DF2.FPR <- cbind(DF2.FPR,DF2$FPR)
}
plot(0,0, xlab="log(FPR)", ylab="TPR",col="white",xlim=c(-10,0), ylim=c(0,1))
colours <- colorRampPalette(c("red","yellow","green", "dark green"))
colours <- colours(max(NOBS))
for (i in flist) {
  name <- substr(i,12,nchar(i)-4)
  params <- strsplit(name," ")[[1]][2:3]
  
  #CP <- as.numeric(substr(params[1],3,nchar(params[1])))
  Nobs <- as.numeric(substr(params[2],5,nchar(params[2])))
  
  index <- which(flist==i)
  #red <- sqrt(Nobs/max(DATA$Nobs))
  #green <- sqrt(1-Nobs/max(DATA$Nobs))
  #lines(log(DF2.FPR[,index]),DF2.TPR[,index], col=rgb(red,green,0))
  lines(log(DF2.FPR[,index]),DF2.TPR[,index], col=colours[Nobs])
}
#TPRmean <- apply(DF2.TPR,1,mean)
#FPRmean <- apply(DF2.FPR,1,mean)
#lines(log(FPRmean),TPRmean, lty=2, lwd=2)
lines(seq(-10,0,0.01), exp(seq(-10,0,0.01)), lwd=2, lty=2)

# score of the scientific prior:
DF2 <- data.frame(model=RASTER.prior[],target=RASTER.posterior[])
DF2 <- DF2[!is.na(DF2$model)&!is.na(DF2$model),]
DF2 <- DF2[order(DF2$model, decreasing=TRUE),]
DF2$TPR <- cumsum(DF2$target==4)/sum(DF2$target==4)
DF2$FPR <- cumsum(DF2$target<4)/sum(DF2$target<4)
DF2 <- DF2[(1:nrow(DF2))%%1000==1,]
lines(TPR~log(FPR), DF2, lwd=2, col="blue")

# score of the bayesian naive model:
DF2 <- data.frame(model=RASTER.bnaive[],target=RASTER.posterior[])
DF2 <- DF2[!is.na(DF2$model)&!is.na(DF2$model),]
DF2 <- DF2[order(DF2$model, decreasing=TRUE),]
DF2$TPR <- cumsum(DF2$target==4)/sum(DF2$target==4)
DF2$FPR <- cumsum(DF2$target<4)/sum(DF2$target<4)
DF2 <- DF2[(1:nrow(DF2))%%1000==1,]
lines(TPR~log(FPR), DF2, lwd=2, col="purple")

# mean group score as function of Nobs:
CLUSTER <- ifelse(NOBS<18,"<18",
                  ifelse(NOBS<51,"18-50",
                         ifelse(NOBS<75,"51-74",
                                #ifelse(NOBS<75,"51-74",
                                ifelse(NOBS<94,"75-93","93<"))))#)
CLUSTER <- factor(CLUSTER, levels=c("<18","18-50","51-74","75-93","93<"))

colours <- colorRampPalette(c("red","yellow","green", "dark green"))
red <- c(1,1,.8,0,0)
green <- c(0,.5,1,1,0.5)
#colours <- colours(5)
colours <- rgb(red,green,0)
plot(0,0, xlab="log(FPR)", ylab="TPR",main="ROC curves of habitat suitability models",col="white",xlim=c(-6,0), ylim=c(0,1))
for (i in levels(CLUSTER)) {
  index <- which(CLUSTER==i)
  
  level <- which(levels(CLUSTER)==i)
  colour <- colours[level]
  
  DF2sub.TPR <- DF2.TPR[,index]
  DF2sub.FPR <- DF2.FPR[,index]
  
  TPRmean <- apply(DF2sub.TPR,1,median)
  FPRmean <- apply(DF2sub.FPR,1,mean)
  
  # new:
  TPRlower <- apply(DF2sub.TPR,1,FUN=quantile, .25,na.rm=TRUE)
  TPRupper <- apply(DF2sub.TPR,1,FUN=quantile, .75,na.rm=TRUE)
  polygon(c(log(FPRmean),rev(log(FPRmean))) ,c(TPRupper,rev(TPRlower)), col=rgb(red,green,0,alpha=.2)[level], border=rgb(red,green,0,alpha=.2)[level])
  
  lines(log(FPRmean),TPRmean, lwd=2, col=colour)
  
  print("Summary of cluster:")
  print(i)
  print("TPR at 1 sq km incorrect")
  print(TPRmean[round(log(FPRmean),1)==-7.5])
  print("TPR at 10 sq km incorrect")
  print(TPRmean[round(log(FPRmean),1)==-5.2])
  #lines(log(DF2.FPR),DF2.TPR[,index], col=rgb(red,green,0))
}

# score of the scientific prior:
DF2 <- data.frame(model=RASTER.prior[],target=RASTER.posterior[])
DF2 <- DF2[!is.na(DF2$model)&!is.na(DF2$model),]
DF2 <- DF2[order(DF2$model, decreasing=TRUE),]
DF2$TPR <- cumsum(DF2$target==4)/sum(DF2$target==4)
DF2$FPR <- cumsum(DF2$target<4)/sum(DF2$target<4)
DF2 <- DF2[(1:nrow(DF2))%%1000==1,]
lines(TPR~log(FPR), DF2, lwd=4, col="blue")
print("Summary of science prior:")
print("TPR at 1 sq km incorrect")
print(DF2$TPR[round(log(DF2$FPR),1)==-6.9])
print("TPR at 10 sq km incorrect")
print(DF2$TPR[round(log(DF2$FPR),1)==-5.1])

# score of the bayesian naive model:
DF2 <- data.frame(model=RASTER.bnaive[],target=RASTER.posterior[])
DF2 <- DF2[!is.na(DF2$model)&!is.na(DF2$model),]
DF2 <- DF2[order(DF2$model, decreasing=TRUE),]
DF2$TPR <- cumsum(DF2$target==4)/sum(DF2$target==4)
DF2$FPR <- cumsum(DF2$target<4)/sum(DF2$target<4)
DF2 <- DF2[(1:nrow(DF2))%%1000==1,]
lines(TPR~log(FPR), DF2, lwd=4, col="purple")
print("Summary of naive bayes:")
print("TPR at 1 sq km incorrect")
print(DF2$TPR[round(log(DF2$FPR),1)==-7.5])
print("TPR at 10 sq km incorrect")
print(DF2$TPR[round(log(DF2$FPR),1)==-5.2])

lines(seq(-10,0,0.01), exp(seq(-10,0,0.01)), lwd=2, lty=2)
legend("bottomright",c(levels(CLUSTER),"Prior model", "Naive Bayes' model"),lty=1,lwd=c(2,2,2,2,2,4,4),col=c(colours,"blue","purple"), title="Number of observations")

##################################
# Nobs as function of timestamps #
##################################
# focus sparql endpoint
library(httr)
library(jsonlite)
library(SPARQL)
#------------------------
# login to collect data
#------------------------

#username = "kdkoning"
#password = "*****"
# a more secure way:
library(getPass)

URL <- "https://focus.sensingclues.org/"
url.login <- paste0(URL,"api/user/login")

#json_body <- jsonlite::toJSON(list(username = username,password=getPass()), auto_unbox = TRUE)
# we set up an authenticated session
#rl <- POST(url.login, body = json_body, encode = "raw",content_type_json())
rl <- POST(url.login, 
           body = jsonlite::toJSON(list(username = getPass("Enter your Cluey username:"),
                                        password=getPass()), 
                                   auto_unbox = TRUE), 
           encode = "raw",
           content_type_json())

#----------
# filters
#----------

# concepts
q <- "  PREFIX skos:<http://www.w3.org/2004/02/skos/core#>
          SELECT DISTINCT ?Concept ?prefLabel
          WHERE
          { ?Concept ?x skos:Concept .
          { ?Concept skos:prefLabel ?prefLabel .
          FILTER regex(?prefLabel, 'Bear')}
          }
        "
focus.p <- "https://focus.sensingclues.org/v1/graphs/sparql?query="
ontology.lst <- content(GET(paste0(focus.p,URLencode(q, reserved=TRUE))))
names <- unlist(ontology.lst$head)
NROW <- length(ontology.lst$results$bindings)
NCOL <- length(names)
ontology.tbl <- matrix(nrow=NROW, ncol=NCOL)
for (a in 1:NROW) {
  for (b in 1:NCOL) {
    ontology.tbl[a,b] <- ontology.lst$results$bindings[[a]][[names[b]]]$value
  }
}
ontology.tbl <- data.frame(ontology.tbl)
names(ontology.tbl) <- names
# location
bounds <- list(north=90,east=40,south=30,west=0)

#-------------------
# sample timerange 
#------------------

count <- 2500
df.dateNobs <- data.frame(Tstart=rep(NA,count), 
                          Tend=rep(NA,count), 
                          Date.start=rep(NA,count), 
                          Date.End=rep(NA,count), 
                          DeltaT=rep(NA,count), 
                          Nobs=rep(NA,count))
for (i in 1:count) {
  starttime <- sample(0:250,1)
  endtime <- starttime + sample(0:400,1)
  TIMERANGE <- c(starttime,endtime)
  
  df.dateNobs$Tstart[i] <- starttime
  df.dateNobs$Tend[i] <- endtime
  df.dateNobs$DeltaT[i] <- endtime-starttime
    
  # date
  print(TIMERANGE)
  input <- list(DateRange=c(Sys.Date()-TIMERANGE[2],Sys.Date()-TIMERANGE[1]))
  print(input)
  
  df.dateNobs$Date.start[i] <- input$DateRange[1]
  df.dateNobs$Date.End[i] <- input$DateRange[2]
  
  #----------
  # data collection
  #----------
  q <- paste0('
{"query":
		{"and-query":
			{"queries":
				[{"range-constraint-query":{"constraint-name":"Concepts","range-operator":"EQ","value":["',ontology.tbl$Concept[4],'"],"range-option":[]}}
				,{"custom-constraint-query":{"constraint-name":"Incidents","point":[],"box":[{"south":',bounds$south,',"west":',bounds$west,',"north":',bounds$north,',"east":',bounds$east,'}],"circle":[],"polygon":[]}}
				,{"range-constraint-query":{"constraint-name":"Date","range-operator":"GE","value":["',input$DateRange[1],'"],"range-option":[]}}
				,{"range-constraint-query":{"constraint-name":"Date","range-operator":"LE","value":["',input$DateRange[2],'"],"range-option":[]}}
				]
			}
		 }
}')
  url.incidents <- paste0(URL,"v1/values/IncidentLocations?options=all&start=1")
  incidentDATA.L <- POST(url.incidents, body=q, encode="raw", content_type_json())
  #incidentDATA.L <- POST(url.incidents, encode="raw", content_type_json())
  incidentDATA <- content(incidentDATA.L)
  Nobs <- length(incidentDATA[[1]]$tuple)
  
  df.dateNobs$Nobs[i] <- Nobs
}

plot(Nobs~Date.start, df.dateNobs)
plot(Nobs~Date.End, df.dateNobs)
plot(Nobs~DeltaT, df.dateNobs)
hist(df.dateNobs$Nobs, breaks=100, col="blue")
abline(v=18,col="red")
#abline(v=42,col="red")
abline(v=51,col="red")
abline(v=75,col="red")
abline(v=94,col="red")


#######
# AUC #
#######

deltaFPR <- FPRmean-c(0,FPRmean[-length(FPRmean)])
AUCmean <- sum(deltaFPR*TPRmean)
getAUC <- function(x) {
  index <- which(flist==x)
  deltaFPR <- DF.FPR[,index]-c(0,DF.FPR[-nrow(DF.FPR),index])
  AUC <- sum(deltaFPR*DF.TPR[,index])
  return(AUC)
}
AUC <- sapply(flist,getAUC)
plot(0,0, xlab="FPR", ylab="TPR",col="white",xlim=c(0,1), ylim=c(0,1))
for (i in flist) {
  index <- which(flist==i)
  if (AUC[index]>AUCmean) {
    lines(DF.FPR[,index],DF.TPR[,index], col="red")
    } else {
    lines(DF.FPR[,index],DF.TPR[,index], col="blue")
  }
}
lines(FPRmean,TPRmean, lty=2, lwd=2)

#lines(FPRmean,predict(lm(TPRmean~poly(FPRmean,20))), col="red")
getNOBS <- function(x) {
  name <- substr(x,12,nchar(x)-4)
  params <- strsplit(name," ")[[1]][2:3]
  #CP <- as.numeric(substr(params[1],3,nchar(params[1])))
  Nobs <- as.numeric(substr(params[2],5,nchar(params[2])))
  return(Nobs)
}
NOBS <- sapply(flist,getNOBS)
max(NOBS[AUC<AUCmean])


plot(0,0, xlab="FPR", ylab="TPR",col="white",xlim=c(0,1), ylim=c(0,1), main="colours indicate confidence parameter")
for (i in flist) {
  index <- which(flist==i)
  
  raster <- raster(i)
  
  DF <- data.frame(model=raster[],target=RASTER.posterior[])
  DF <- DF[!is.na(DF$model)&!is.na(DF$model),]
  DF <- DF[order(DF$model, decreasing=TRUE),]
  DF$TPR <- cumsum(DF$target>2)/sum(DF$target>2)
  DF$FPR <- cumsum(DF$target<3)/sum(DF$target<3)
  DF <- DF[(1:nrow(DF))%%1000==1,]
  
  name <- substr(i,12,nchar(i)-4)
  params <- strsplit(name," ")[[1]][2:3]
  
  CP <- as.numeric(substr(params[1],3,nchar(params[1])))
  Nobs <- as.numeric(substr(params[2],5,nchar(params[2])))
  
  #plot(TPR~FPR, DF, type="l")
  lines(TPR~FPR, DF, col=rgb(CP/max(DATA$CP),1-CP/max(DATA$CP),0))
}




