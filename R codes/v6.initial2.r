cat("Initialize Markov chain; ")
# Initialize Markov Chain;
pmprior <- unitmask
rm(unitmask)
# pmgenpars <- c(6.0,1) # just tried this
#pmgenpars <- c(0,3.158971,1.516190) # Andrew estimated from traced links using China data;
#pmgenpars <- c(0,1.423395,4.528192)
pmgenpars <- c(0,2.001668,2.493639)
sigth <- rep(0,length(invtransfpar(pmgenpars)))
# adjust sigp to achieve good acceptance rate
# sigp <- 1.5
sigp <- 0.25;
pmppars <- matrix(1,ncol=length(timedata),nrow=length(timedata))*pmprior
newtypes <- mctype(typedata,freqtype)
oldtypes <- newtypes
# typemat <- mctypemat(oldtypes)
typemat <- typemask
rm(typemask)
updtype <- 0
tu <- updtype
oldtheta <- invtransfpar(pmgenpars)
linklist <- makelinklist(oldtheta,timedata)*typemat
ltppars <- matrix(c(oldtheta,rep(0.5,length(oldtheta))),nrow=3) #oldpmat <- out.data[[1]],
oldpmat <- matrix(NA,nrow=length(timedata),ncol=length(timedata))
# for (i in 1:length(timedata)){
#   oldpmat[i,]<-initvec2(i,timedata,
#                         typemat[1:length(timedata)],invtransfpar(pmgenpars))
# }
oldpmat <- sapply(1:length(timedata),
  function(x) initvec(x,timedata,typemat[1:length(timedata)],
                       invtransfpar(pmgenpars)))
# oldpmat <- normalize(matrix(1,nrow=length(timedata),ncol=length(timedata)))
oldlogpost <- logpost(timedata,oldtheta,linklist,oldpmat)
logoldpost <- -2*oldlogpost
pmpost <- logoldpost

posttrail <- list(pmpost)
thetatrail <- list(oldtheta)
#pmtrail <- list(oldpmat)
typetrail <- list(oldtypes)

rej <- 0
acc <- 0
sk <- 1
iterskip <- 1

# system.time(btest(sk))
