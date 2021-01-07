graphname <- paste("./output/",ver,".output",ind.county,".pdf",sep="")

mcpost <- thinning(burnin(posttrail,burn),thin)
pmpos <- which(unlist(mcpost)==min(unlist(mcpost)))[1]
#pmpmat <- pmtrail[[pmpos]]
pmpmat <- pmpv

pdf(graphname,width=10,height=5)
par(mfrow=c(1,1))
plot(1:length(unlist(mcpost)),unlist(mcpost),type="l",
     xlab="iteration",ylab="log posterior",
     main=paste(100*acc/(acc+rej),"% accepted",sep=""))
dev.off()
