cat("Type matrix functions; ")
# Virus type: probabilities
typecount <- function(tpdata){
  matrix(c(sort(unique(tpdata)),as.vector(table(tpdata))),nrow=2,byrow=TRUE)
}

normalize <- function(pvec){
  if (sum(pvec)==0) {return(pvec)}
  return(pvec/sum(pvec))
}

# Virus type
rndtype <- function(typepvec){ # generate random type
  sum(rmultinom(1,1,typepvec)*types)
}

mctype <- function(tpdata,tpvec){ # substitute missing type with random type
  tmp <- c()
  for (k in 1:length(tpdata)){
    if (is.na(tpdata[k])){
      tmp[k] <- rndtype(tpvec)
    } else {
      tmp[k] <- tpdata[k]
    }
  }
  return(tmp)
}

mctype1 <- function(oldtp,tpdata,tpvec){ # substitute 1 missing by random type
  tmp <- oldtp
  untyped <- which(is.na(tpdata))
  tmp[untyped[sample(1:length(untyped),1)]] <- rndtype(tpvec)
  return(tmp)
}

mctypemat <- function(mctp){
  typemat <- matrix(NA,ncol=length(mctp),nrow=length(mctp))
  for (i in 1:length(mctp)){
    for (j in 1:length(mctp)){
      typemat[i,j] <- ifelse((mctp[i]==1 & mctp[j]==1),1,0)
    }
  }
  return(typemat)
}

