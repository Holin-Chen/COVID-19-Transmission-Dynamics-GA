#use the serial interval distribution for right-censoring correction;
prop.obs <- c()
r.est.adj <- r.est
for (i in 1:nrow(r.est)){
  prop.obs[i] <- pgamma(r.est[nrow(r.est)]-r.est[i]+0.5,shape = pmgenpars[2],scale = pmgenpars[3])
  r.est.adj[i,2:4] <- r.est[i,2:4] / prop.obs[i]
}

pdf(paste("./",ver,"/output/","Rgraph_adjust_",k.county,".pdf",sep=""))
par(mfrow=c(3,1),mar=c(2,4,6,0))
epic <- epicurve(rep(TRUE,length(timedata)),timedata)
barplot(epic[,2],names.arg=epic[,1],xaxt="n",ylab="nr cases",main=paste0(k.county))
axis(1, at=0:(length(seq(min(timedata),max(timedata),by=1))-1)*1.2+0.75,
     labels=as.Date(min(time):max(time),origin="1970-01-01"),las=2)
errbar(r.est[,1],r.est[,3],r.est[,4],r.est[,2],
       xlab="",ylab="Unadjusted Reff",xaxt = "n",ylim=c(0,10))
abline(h=1,lty=2)
par(mar=c(8,4,0,0))
errbar(r.est.adj[,1],r.est.adj[,3],r.est.adj[,4],r.est.adj[,2],
       xlab="",ylab="Adjusted Reff",xaxt = "n",ylim=c(0,10))
axis(1, at=0:(length(seq(min(timedata),max(timedata),by=1))-1),
     labels=as.Date(min(time):max(time),origin="1970-01-01"),las=2)
abline(h=1,lty=2)
mtext(side=1, text="date", line=6)
dev.off()
