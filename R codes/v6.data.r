cat("Load data; ")

library(tidyr)
library(reshape2)
# Read Data
ver="v6";
datapath <- "~/stat/nCoV_GA/data/";

raw.full <- read.csv("./data/pui_emory_extract_202007130845_imputed.csv",stringsAsFactors = F)
#raw.full <- read.csv("./data/pui_emory_extract_202006290845_imputed.csv",stringsAsFactors = F)
label.full <- read.csv("./data/pui_data_dict_202005300846.csv",stringsAsFactors = F)

#load counties list;
load(paste0("./counties3.RDA"))
counties3$counties[6] <- "NEWTON, ROCKDALE"

#select variables for the analysis
raw.sub <- raw.full[,c("QARESPONSEID","Q174631","Q174646","Q174652","Q175852","Q174655","Q174656","Q174664","Q174755","Q174833",
                       "Q175985","Q175987","Q178250","Q178251","Q178254","Q178270","Q178274","Q178275","Q178280")]

#clean county;
raw.sub$Q175852[which(raw.sub$Q175852=="" & raw.sub$Q174652!="")]<-raw.sub$Q174652[which(raw.sub$Q175852=="" & raw.sub$Q174652!="")]
raw.sub<-raw.sub[,-which(names(raw.sub)=="Q174652")]

#select only confirmed cases;
raw.sub <- raw.sub[which(raw.sub$Q174755=="Yes"),]

#clean close contact with known cases;
Numextract <- function(string){
  new.string <- unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
  new.string <- new.string[which(nchar(new.string)==6)]
  return(paste(new.string,collapse = ","))
}
raw.sub$Q175987 <- unlist(lapply(raw.sub$Q175987,FUN=Numextract))

#clean outbreak ID;
raw.sub$Q178280 <- gsub(" ","",raw.sub$Q178280)

#create a days between onset date and reported date;
raw.sub$days.delay <- as.numeric(as.Date(raw.sub$Q174631,"%m/%d/%Y")) - as.numeric(as.Date(raw.sub$Q174656,"%m/%d/%Y"))

#cases without any date information and will be dropped for analysis;
raw.sub <- raw.sub[-which(raw.sub$Q174656==""),]

#select the information for analysis;
raw.GA <- raw.sub[,c("QARESPONSEID","Q174656","Q174646","Q175852","Q174655",
                     "Q174833","Q175987","Q178280","days.delay")]
names(raw.GA) <- c("ID","date.onset","gender","county","age",
                   "zip","contact","outbreak","days.delay")
raw.GA$days.delay[which(raw.GA$days.delay<0 | raw.GA$days.delay>100)] <- NA
rm(raw.sub)

k.state <- "Georgia"
raw.GA$date.onset <- as.numeric(as.Date(raw.GA$date.onset,"%m/%d/%Y"))
raw.GA$state <- k.state

#drop symptom onset date before 01/01/2020 and after 07/13/2020;
#subset time period;
raw.GA <- raw.GA[-which(raw.GA$date.onset<as.numeric(as.Date("01/01/2020","%m/%d/%Y")) |
                         raw.GA$date.onset>as.numeric(as.Date("07/13/2020","%m/%d/%Y"))),]

k.county <- unlist(strsplit(counties3$counties[ind.county],", "))
#subset for four counties;
raw.GA <- raw.GA[which(raw.GA$county %in% k.county),]
if (any(is.na(raw.GA$date.onset))){
  raw.GA <- raw.GA[-which(is.na(raw.GA$date.onset)),]
}

ID <- raw.GA$ID
time <- raw.GA$date.onset
state <- raw.GA$state
county <- raw.GA$county
gender <- raw.GA$gender
age <- raw.GA$age
outbreak <- raw.GA$outbreak
delay <- raw.GA$days.delay

tm <- time - min(time,na.rm=TRUE);

counties <- unique(county);
unit <- rep(NA,length(counties));
for(k in 1:length(counties)){
  unit[county==counties[k]] <- k;
}
type <- unit

ind <- order(tm);
ID <- ID[ind]
state <- state[ind]
county <- county[ind]
gender <- gender[ind]
age <- age[ind]
delay <- delay[ind]
timedata <- tm[ind];
unitdata <- unit[ind];
outbreakdata <- outbreak[ind];
typedata <- type[ind]

types <- typecount(typedata)[1,]

freqtype <- normalize(typecount(typedata)[2,])

typemask <- matrix(NA,ncol=length(typedata),nrow=length(typedata))
for (i in 1:length(typedata)){
  for (j in 1:length(typedata)){
    typemask[i,j] <- ifelse((typedata[i]==typedata[j]),1,0)
 }
}
unitmask <- matrix(1,ncol=length(unitdata),nrow=length(unitdata))

cont.mat <- matrix(0.01,nrow=length(time),ncol=length(time))
for (i in 1:length(time)){
  tmp0 <- as.numeric(unlist(strsplit(raw.GA$contact[i],",")))
  cont.mat[i,which(raw.GA$ID %in% tmp0)] <- 1
  cont.mat[which(raw.GA$ID %in% tmp0),i] <- 1
}
contmask <- cont.mat[ind,ind]

#outbreak mask;
outbreakdata[which(outbreakdata=="")] <- "0000000"
outbreakmask <- matrix(0.01,ncol=length(outbreakdata),nrow=length(outbreakdata))
for (i in 1:length(outbreakdata)){
  for (j in 1:length(outbreakdata)){
    if(outbreakdata[i]==outbreakdata[j]) outbreakmask[i,j] <- 1;
  }
}

unitmask <- unitmask * contmask * outbreakmask
rm(contmask); rm(outbreakmask); rm(cont.mat);

