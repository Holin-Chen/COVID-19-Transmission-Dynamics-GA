require(Hmisc)

epicurve <- function(slct,tmvec){
  tm <- (0:max(tmvec))
  tmsel <- tmvec[slct]
  epic <- array(NA,dim=c(length(tm),2))
  for(k.tm in 1:length(tm)){
    sel <- which(tmsel==tm[k.tm])
    epic[k.tm,] <- c(tm[k.tm],length(sel))
  }
  return(epic)
}

reprnum <- function(pmat){
  csum <- rep(NA,dim(pmat)[2])
  for(k.col in 1:dim(pmat)[2]) csum[k.col] <- sum(pmat[,k.col])
  return(csum)
}

rbytime <- function(slct,pmat,tmvec){
  rbycol <- reprnum(pmat)[slct]
  tmsel <- tmvec[slct];
  tm <- unique(tmvec)
  rbyt <- array(NA,dim=c(length(tm),5))
  for(k.tm in 1:length(tm)){
    sel <- which(tmsel==tm[k.tm])
    rbyt[k.tm,1] <- tm[k.tm]
    rbyt[k.tm,2:4] <- quantile(rbycol[sel],c(0.025,0.5,0.975))
    rbyt[k.tm,5] <- mean(rbycol[sel],na.rm = T)
  }
  return(rbyt)
}

# r.est <- rbytime(rep(TRUE,length(timedata)),pmpmat,timedata)
#this is the code you can truncate 5 recent days of Rt;
#r.est[(length(r.est[,1])-4):length(r.est[,1]),2:4] <- NA

# pdf(paste("./",ver,"/output/","Rgraph_",k.county,".pdf",sep=""))
# par(mfrow=c(2,1),mar=c(2,4,6,0))
# epic <- epicurve(rep(TRUE,length(timedata)),timedata)
# barplot(epic[,2],names.arg=epic[,1],xaxt="n",ylab="nr cases",main=paste0(k.county))
# par(mar=c(8,4,0,0))
# errbar(r.est[,1],r.est[,3],r.est[,4],r.est[,2],
#        xlab="",ylab="Reff",xaxt = "n",ylim=c(0,10))
# axis(1, at=0:(length(seq(min(timedata),max(timedata),by=1))-1),
#      labels=as.Date(min(time):max(time),origin="1970-01-01"),las=2)
# abline(h=1,lty=2)
# mtext(side=1, text="date", line=6)
# dev.off()

out.res <- matrix(NA, nrow = 0, ncol = 6)
for(k in unique(unitdata)){
  r.est <- rbytime(unitdata==k,pmpmat,timedata)
  out.res <- rbind(out.res, cbind(r.est[,1]+min(time),r.est[,2:5],rep(counties[k],length(r.est[,1]))))
  pdf(paste("./output/","Rgraph_",counties[k],".pdf",sep=""))
  par(mfrow=c(2,1),mar=c(2,4,6,0))
  epic <- epicurve(unitdata==k,timedata)
  barplot(epic[,2],names.arg=epic[,1],xaxt="n",ylab="nr cases",main=paste0(counties[k],", n=",sum(unitdata==k)))
  par(mar=c(8,4,0,0))
  errbar(r.est[,1],r.est[,3],r.est[,4],r.est[,2],
         xlab="",ylab="Reff",xaxt = "n",ylim=c(0,10))
  axis(1, at=0:(length(seq(min(timedata),max(timedata),by=1))-1),
       labels=as.Date(min(time):max(time),origin="1970-01-01"),las=2)
  abline(h=1,lty=2)
  mtext(side=1, text="date", line=6)
  dev.off()
}
out.res <- data.frame(out.res)
names(out.res) <- c("date onset","q2.5","median","q97.5","mean","county")
out.res$`date onset` <- as.numeric(as.character(out.res$`date onset`))
out.res$q2.5 <- as.numeric(as.character(out.res$q2.5))
out.res$q97.5 <- as.numeric(as.character(out.res$q97.5))
out.res$median <- as.numeric(as.character(out.res$median))
out.res$mean <- as.numeric(as.character(out.res$mean))

save(out.res,file = paste("./output/res",ind.county,".rda",sep=""))


