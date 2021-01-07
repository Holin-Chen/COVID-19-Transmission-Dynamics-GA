cat("MCMC functions\n")
#   MCMC;
##  Set up MCMC;
### Parameter bookkeeping;
logit <- function(x){
  exp(x)/(1+exp(x))
}
invlogit <- function(x){
  x[which(x==0)] <- 10^(-16)
  x[which(x==1)] <- 1-10^(-16)
  log(x/(1-x))
}

# this new admlink is much faster.
admlink <- function(i,dat,span){
  link <- as.numeric(dat[i]-dat>=span[1] & dat[i]-dat<span[2] &
                     (1:length(dat))<i)
  return(link)
}

transfpar <- function(par) c(par[1],exp(par[2:3]))
invtransfpar <- function(par) c(par[1],log(par[2:3]))

makelinklist <- function(par,tdata){
  mat <- sapply(1:length(tdata),
         function(x) admlink(x,timedata,
           c(-par[1],length(probtab(transfpar(par)))-1-par[1])))
  return(t(mat))
}

nupdate <- function(ll){
  vec <- apply(ll,1,function(x) length(x[x==1]))
  return(vec)
}

### Distribution;
normpdf <- function(x,m,s){
  dnorm(x,mean=m,sd=s)
}

gammapdf <- function(x,r,l){
  dgamma(x,shape=r,scale=l)
}

dirchpdf <- function(p,u){
  z <- prod(gamma(u))/gamma(sum(u))
  return(prod(p^(u-1))/z)
}

linkdirchpdf <- function(p,u,link){
  prob <- p[link==1]
  value <- u[link==1]
  if (length(prob)==0) {return(1)}
  return(dirchpdf(prob,value))
}

normalize <- function(pvec){
  if (sum(pvec)==0) {return(pvec)}
  return(pvec/sum(pvec))
}

### Random sample generators
gammarnd <- function(theta){
  rgamma(1,shape=theta[1],scale=theta[2])
}

gammamixrnd <- function(theta){
  ifelse(rbinom(1,1,theta[1]),gammarnd(exp(theta[2],exp(theta[3]))),
                              gammarnd(exp(theta[4],exp(theta[5]))))
}

dirchrand <- function(u){
  vec<-sapply(1:length(u),function(x) rgamma(1,shape=u[x],scale=1))
  vec<-normalize(vec)
  return(vec)
}

genuni <- function(lw,up){
  runif(1,min=lw,max=up)
}

### Likelihood functions;
ilik <- function(k,tmdat,theta,pvec){
  sum(probij(k,c(1:length(tmdat)),transfpar(theta),tmdat)*pvec)
}

mlvec <- function(k,tmdat,tpvec,theta){
  tmp <- probij(k,c(1:length(tmdat)),transfpar(theta),tmdat)*tpvec
  Max <- max(tmp)
  posmax <- which(tmp==Max)
  nul <- rep(0,length(tmdat))
  tmp <- nul
  tmp[posmax] <- 1
  if (Max==0) {return(nul)} else {return(normalize(tmp))}
}

initvec <- function(k,tmdat,tpvec,theta){
  tmp <- probij(k,c(1:length(tmdat)),transfpar(theta),tmdat)*tpvec
  Max <- max(tmp)
  posmax <- which(tmp==Max)
  nul <- rep(0,length(tmdat))
  tmp <- nul
  tmp[posmax] <- 1
  if (Max==0) {return(nul)} else {
    return(normalize(tmp+rgamma(1,shape=4,scale=0.004)))}
}

### Priors

thetaprior <- function(transftheta){
  tmp <- sapply(1:length(ltppars[,1]),
    function(x) normpdf(transftheta[x],ltppars[x,1],ltppars[x,2]))
  return(prod(tmp))
}

pmatprior <- function(pmat,link){
  tmp[i] <- sapply(1:length(pmppars),
    function(x) linkdirchpdf(pmat[x],pmppars[x],link[x]))
  return(prod(tmp))
}

typeprior <- function(typepvec){
  dirchpdf(typepvec,tppriorpars)
}

### Posterior
post <- function(dat,transftheta,link,pmat){
  tmp <- sapply(1:length(pmat[,1]),
    function(x) ilik(x,dat,transftheta,link[x,]*pmprior[x,]*pmat[x,]))
  return(thetaprior(transftheta)*prod(tmp[which(tmp!=0)]))
}

logpost <- function(dat,transftheta,link,pmat){
  tmp <- sapply(1:length(pmat[,1]),
    function(x) ilik(x,dat,transftheta,link[x,]*pmprior[x,]*pmat[x,]))
  return(log(thetaprior(transftheta))+sum(log(tmp[which(tmp!=0)])))
}
### Generate candidate parameter values
dthcand <- function(){
  tmp <- sapply(1:length(ltppars[,1]),
    function(x) ifelse(sigth[x]==0,0,rnorm(1,0,sigth[x])))
  return(tmp)
}

candtheta <- function(){
  oldtheta+dthcand()
}

dpcand <- function(){
  matrix(rnorm(length(pmppars[1,])^2,0,sigp),nrow=length(pmppars[1,]))
}

candpmat <- function(){
  t(apply(logit(invlogit(oldpmat)+dpcand())*linklist,1,normalize))
}

typecand <- function(){
  tmp <- sapply(1:length(sigtp),function(x) rnorm(1,0,sigtp[x]))
  return(tmp)
}

candtpmat <- function(){
  normalize(logit(invlogit(oldvec)+typecand()))
}

upd.theta <- function(sigth){
  if (sum(sigth)>0) {
    newtheta <- candtheta()
    linklist <- makelinklist(newtheta,timedata)*typemat
  } else{
    newtheta <- oldtheta
  }
  return(newtheta)
}

upd.typemat <- function(updtype,typemat){
  if (updtype!=0){
    if (tu==0){
      newtypes <- mctype1(oldtypes,freqtype)
      typemat <- mctypemat(newtypes)
      tu <- updtype
    } else{
      tu <- tu-1
    }
  }
  return(typemat)
}

upd.pmat <- function(sigp){
  if(sigp!=0){
    newpmat <- candpmat()
  }else{
    newpmat <- oldpmat
  }
  return(newpmat)
}

### Adaptive rejection sampling (Metropolis-Hastings)
btest <- function(sk){
  newtheta <- upd.theta(sigth)
  typemat <- upd.typemat(updtype,typemat)
  newpmat <- upd.pmat(sigp)
  newlogpost <- logpost(timedata,newtheta,linklist*typemat,newpmat)
  while(!is.finite(newlogpost)){
    newtheta <- upd.theta(sigth)
    typemat <- upd.typemat(updtype,typemat)
    newpmat <- upd.pmat(sigp)
    newlogpost <- logpost(timedata,newtheta,linklist*typemat,newpmat)
  }
  logratio <- newlogpost - oldlogpost
  # cat(" ",logratio," ")
  if (exp(logratio)>=genuni(0,1)){
    oldlogpost <<- newlogpost
    logoldpost <<- -2*oldlogpost
    if (updtype!=0){
      oldtypes <<- newtypes
    }
    oldtheta <<- newtheta
    oldpmat <<- newpmat
    acc <<- acc+1
  } else{rej <<- rej+1}
  if (sk==iterskip){
    inc()
    sk <- 1
  } else{sk <- sk+1}
  return(sk)
}

inc <- function(){
  posttrail[[length(posttrail)+1]] <<- logoldpost
  #pmtrail[[length(pmtrail)+1]] <<- oldpmat
  if (updtype!=0){
    typetrail[[length(typetrail)+1]] <<- oldtypes
  }
  if (sum(sigth)>0){
    thetatrail[[length(thetatrail)+1]] <<- oldtheta
  }
}

gentimes <- function(r){
  oldperc <- 0;
  progbar(0,r);
  for (j in 1:r){
    sk <- btest(sk)
    if (pmpost>logoldpost){
      pmpost <<- logoldpost
      pmth <<- oldtheta
      pmpv <<- oldpmat
      pmtp <<- oldtypes
    }
    newperc <- round(100*j/r);
    if(newperc - oldperc >= 2){
      progbar(j,r);
      oldperc <- newperc;
    }
  }
  cat("\n")
}

burnin <- function(mclist,intro){
  choose <- seq(from=intro,to=length(mclist),by=1)
  return(mclist[choose])
}

thinning <- function(mclist,thinskip){
  choose <- seq(from=1,to=length(mclist),by=thinskip)
  return(mclist[choose])
}

progbar <- function(j,r){
  prgri <- round(100*j/r);
  star <- round(50*j/r); spce <- 50 - star;
  bsp <- ifelse(prgri<10,2,3);
  if(j!=0) cat(rep("\b",52+bsp),sep="");
  cat("|",sep="");
  cat(rep("*",star),sep="");
  cat(rep(" ",spce),sep="");
  cat("|",sep="");
  cat(prgri,"%",sep="");
}
