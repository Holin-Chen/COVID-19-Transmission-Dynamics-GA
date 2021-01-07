cat("Likelihood functions; ");
# Likelihood Function;
# The parameters may have different numbers depend on the model we are
# going to use;
# par5 is (p,r1,lambda1,r2,lambda2) for mixed model and par2 is (r,lambda)
# for simple model;
# Construct likelihood kernel
## Generation interval kernel
# probability transfer from i to j, the interval is +-1day;
# shifted gamma distribution: allow for negative serial intervals
ptij <- function(ti,tj,dt,tshft,a,b){
  pgamma(ti-tj+dt+tshft,shape=a,scale=b)-pgamma(ti-tj-dt+tshft,shape=a,scale=b)
}

logit<-function(x){
  exp(x)/(1-exp(x))
}

# serial interval pdf;
serintpdf <- function(x,tshft,r,lambda){
  dgamma(x+tshft,shape=r,scale=lambda)
}
unifpdf <- function(x,u1,u2){
  dunif(x,min=u1,max=u2)
}

#serial interval cdf;
serintcdf <- function(x,tshft,r,lambda){
  pgamma(x+tshft,shape=r,scale=lambda)
}

maxgenint <- function(par){ # only positive roots
  f <- function(x){1-serintcdf(exp(x),par[1],par[2],par[3])-10^-3}
  return(c(-par[1],exp(uniroot(f, c(-10^8,10^8))$root)))
}

## Likelihood: product over all terms
ppij <- function(i,j,par,data){
  ifelse(i!=j,ptnegij(data[i],data[j],1,par[1],par[2],par[3]),0)
}

# Generate table with probabilities for generation interval kernel
# Maximum admissable generation interval: 99.9 percentile of interval
# distribution
probtab <- function(par){
  j <- 0
  stop <- FALSE
  prvect <- c()
  while(stop!=TRUE){
    new <- ptij(j,0,1,par[1],par[2],par[3])
    prvect[j+1] <- new
    if(sum(prvect)>(0.999999*2) | j>100 | new==0) {stop <- TRUE}
    j <- j+1
  }
  return(c(prvect,0))
}

# Generate vector of time differences (time intervals) for all possible
# contacts: 
# replace inadmissable ones with -2 (including self, this is the outer
# Replace). See next line for reason why not -1.

diffs <- function(i,vec,rng){
  diff_t <- vec[i]-vec
  diff_t[which(diff_t<rng[1] | diff_t>rng[2])] <- -2
  diff_t[i] <- -2
  return(diff_t)
}

# Probability of j infecting i
# two parameters
probvec <- function(i,par,data,maxint){
  serintpdf(diffs(i,data,maxint),par[1],par[2],par[3])
}
probunif <- function(i,par,data){
  unifpdf(data[i]-data,par[1],par[2])
}

# probij <- function(i,j,par,data){
#   c(probunif(i,c(0,tswuhan),timedata)[j==1], # for Wuhan, uniform distribution
#     probvec(i,par,data,maxgenint(par))[j>1])
# }

probij <- function(i,j,par,data){
  probvec(i,par,data,maxgenint(par))[j]
}

# j can be vector, i can only be a number.
probijnp <- function(i,j,par,data){
  pvec<-probij(i,(1:length(data)),par,data)
  return(pvec[j]/(1-pvec[j])*prod(1-pvec))
}
