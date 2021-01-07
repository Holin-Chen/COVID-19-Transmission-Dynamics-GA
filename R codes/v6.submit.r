# nCoV Transmission;
# Transmission analysis with linking kernel;
# Global Setting
setwd("C:\\Users\\holin\\Desktop\\Transmission Dynamics Team Work\\nCoV_GA\\nCoV_GA\\v6\\")

ver <- "v6"
main_dir <- paste("~/stat/nCoV_GA/",sep="")
dat_dir <- paste("~/stat/nCoV_GA/","data/",sep="")
mcmc_dir <- paste("./",sep="")

# define type matrix
source(paste(mcmc_dir,ver,'.type.r',sep=""))
#likelihood function
source(paste(mcmc_dir,ver,'.likelihood.r',sep=""))
# mcmc function
source(paste(mcmc_dir,ver,'.mcmc.r',sep=""))
#select counties; value range from 1-19;
ind.county <- 7
#read data
source(paste(mcmc_dir,ver,'.data.r',sep=""))

# initialize everything
source(paste(mcmc_dir,ver,'.initial.r',sep=""))
source(paste(mcmc_dir,ver,'.initial2.r',sep=""))

sigp <- 0.15

# set a timer;
ptm <- proc.time()
# Iterations
iterskip <- 1
burn <- 500; thin <- 10;
numiter <- 600;
cat("Generating samples...\n");
gentimes(numiter)

# running time;
runtime <- proc.time() - ptm
runtime/3600


# Post processing MCMC
source(paste(mcmc_dir,ver,'.post.r',sep=""))
out.data <- list(pmpmat, unitdata, timedata, time, counties)
save(out.data,file = paste("./output/pmpmat",ind.county,".rda",sep=""))

# Graph output
source(paste(mcmc_dir,ver,'.output.r',sep=""))

#not using the right censor correction;
#source(paste(mcmc_dir,ver,'.right.censor.r',sep=""))

