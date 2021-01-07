library(MASS)
ver="v5";

#raw.full <- read.csv("./data/pui_emory_extract_202005300845.csv",stringsAsFactors = F)
#raw.full <- read.csv("./data/pui_emory_extract_202006290845.csv",stringsAsFactors = F)
raw.full <- read.csv("./data/pui_emory_extract_202007130845.csv",stringsAsFactors = F)


label.full <- read.csv("./data/pui_data_dict_202005300846.csv",stringsAsFactors = F)

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

#clean implausible dates 
raw.sub$Q174631[as.numeric(as.Date(raw.sub$Q174631,"%m/%d/%Y"))<as.numeric(as.Date("01/01/2020","%m/%d/%Y")) |
                  as.numeric(as.Date(raw.sub$Q174631,"%m/%d/%Y"))>as.numeric(as.Date("08/01/2020","%m/%d/%Y"))] <- ""
raw.sub$Q175985[as.numeric(as.Date(raw.sub$Q175985,"%m/%d/%Y"))<as.numeric(as.Date("01/01/2020","%m/%d/%Y")) |
                  as.numeric(as.Date(raw.sub$Q175985,"%m/%d/%Y"))>as.numeric(as.Date("08/01/2020","%m/%d/%Y"))] <- ""
raw.sub$Q174656[as.numeric(as.Date(raw.sub$Q174656,"%m/%d/%Y"))<as.numeric(as.Date("01/01/2020","%m/%d/%Y")) |
                  as.numeric(as.Date(raw.sub$Q174656,"%m/%d/%Y"))>as.numeric(as.Date("08/01/2020","%m/%d/%Y"))] <- ""

only.confirmed.date <- which(raw.sub$Q174631!="" & raw.sub$Q175985=="" & raw.sub$Q174656=="")
with.confirmed.date <- which(raw.sub$Q174631!="")
only.specimen.date <- which(raw.sub$Q175985!="" & raw.sub$Q174656=="")
with.specimen.date <- which(raw.sub$Q175985!="")

raw.sub$days.delay.report <- NA
raw.sub$days.delay.specimen <- NA
#create a days between onset date and reported date;
raw.sub$days.delay.report[with.confirmed.date] <- as.numeric(as.Date(raw.sub$Q174631[with.confirmed.date],"%m/%d/%Y")) - as.numeric(as.Date(raw.sub$Q174656[with.confirmed.date],"%m/%d/%Y"))
raw.sub$days.delay.specimen[with.specimen.date] <- as.numeric(as.Date(raw.sub$Q175985[with.specimen.date],"%m/%d/%Y")) - as.numeric(as.Date(raw.sub$Q174656[with.specimen.date],"%m/%d/%Y"))
#impute missing symptom onset dateby conducting an possion distribution based on confirmed date or first specimen collective date
raw.sub$days.delay.report[raw.sub$days.delay.report < 0] <- NA
raw.sub$days.delay.specimen[raw.sub$days.delay.specimen < 0] <- NA
raw.sub$days.report <- as.numeric(as.Date(raw.sub$Q174631,"%m/%d/%Y"))
raw.sub$days.specimen <- as.numeric(as.Date(raw.sub$Q175985,"%m/%d/%Y"))
raw.sub$days.onset <- as.numeric(as.Date(raw.sub$Q174656,"%m/%d/%Y"))

#impute for cases with first speciemen collective dates
delay.specimen.reg <- glm.nb(data=raw.sub, days.delay.specimen ~ days.specimen)
#summary(delay.specimen.reg)
#extract the intercept, coefficicent
intercept <- summary(delay.specimen.reg)$coefficients[1]
beta <- summary(delay.specimen.reg)$coefficients[2]
raw.sub$imputing.onset.date <- NA
raw.sub$imputing.delay.specimen <- NA
for(i in only.specimen.date){
  if (is.na(raw.sub$days.onset[i])){
    raw.sub$imputing.delay.specimen[i] <- rnbinom(n = 1, size = delay.specimen.reg$theta, mu = exp(intercept+beta*raw.sub$days.specimen[i]))
    raw.sub$imputing.onset.date[i] <- raw.sub$days.specimen[i] - raw.sub$imputing.delay.specimen[i]
  }
}

par(mfrow=c(2,1))
plot(raw.sub$days.report,raw.sub$days.delay.specimen,xlim=range(raw.sub$days.report,na.rm=T),ylim=c(0,100),main="Observed")
plot(raw.sub$days.report,raw.sub$imputing.delay.specimen,xlim=range(raw.sub$days.report,na.rm=T),ylim=c(0,100),main="Imputed")

mat.delay.specimen <- matrix(NA,ncol=4,nrow=length(unique(raw.sub$days.report)))
for (i in 1:length(unique(raw.sub$days.report))){
  mat.delay.specimen[i,1] <- unique(raw.sub$days.report)[i]
  mat.delay.specimen[i,2:4] <- quantile(raw.sub$days.delay.specimen[which(raw.sub$days.report==unique(raw.sub$days.report)[i])],c(0.025,0.5,0.975),na.rm=T)
}

mat.delay.specimen.imputed <- matrix(NA,ncol=4,nrow=length(unique(raw.sub$days.report)))
for (i in 1:length(unique(raw.sub$days.report))){
  mat.delay.specimen.imputed[i,1] <- unique(raw.sub$days.report)[i]
  mat.delay.specimen.imputed[i,2:4] <- quantile(raw.sub$imputing.delay.specimen[which(raw.sub$days.report==unique(raw.sub$days.report)[i])],c(0.025,0.5,0.975),na.rm=T)
}

par(mar=c(6,4,3,0))
errbar(mat.delay.specimen[,1],mat.delay.specimen[,3],mat.delay.specimen[,4],mat.delay.specimen[,2],
       xlab="",ylab="delay",xaxt = "n",ylim=c(0,30))
title(main="delay for specimen collected (observed)")
axis(1, at=seq(min(raw.sub$days.report,na.rm=T),max(raw.sub$days.report,na.rm=T),by=1),
     labels=as.Date(min(raw.sub$days.report,na.rm=T):max(raw.sub$days.report,na.rm=T),origin="1970-01-01"),las=2)

errbar(mat.delay.specimen.imputed[,1],mat.delay.specimen.imputed[,3],mat.delay.specimen.imputed[,4],mat.delay.specimen.imputed[,2],
       xlab="",ylab="delay",xaxt = "n",ylim=c(0,30))
title(main="delay for specimen collected (imputed)")
axis(1, at=seq(min(raw.sub$days.report,na.rm=T),max(raw.sub$days.report,na.rm=T),by=1),
     labels=as.Date(min(raw.sub$days.report,na.rm=T):max(raw.sub$days.report,na.rm=T),origin="1970-01-01"),las=2)
# par(mfrow=c(2,1))
# plot(raw.sub$days.report,raw.sub$days.delay.specimen,xlim=range(raw.sub$days.report,na.rm=T),ylim=c(0,100),main="Observed")
# plot(raw.sub$days.report,raw.sub$imputing.delay.specimen,xlim=range(raw.sub$days.report,na.rm=T),ylim=c(0,100),main="Imputed")
# 
# mat.delay.specimen <- matrix(NA,ncol=4,nrow=length(unique(raw.sub$days.report)))
# for (i in 1:length(unique(raw.sub$days.report))){
#   mat.delay.specimen[i,1] <- unique(raw.sub$days.report)[i]
#   mat.delay.specimen[i,2:4] <- quantile(raw.sub$days.delay.specimen[which(raw.sub$days.report==unique(raw.sub$days.report)[i])],c(0.025,0.5,0.975),na.rm=T)
# }
# 
# mat.delay.specimen.imputed <- matrix(NA,ncol=4,nrow=length(unique(raw.sub$days.report)))
# for (i in 1:length(unique(raw.sub$days.report))){
#   mat.delay.specimen.imputed[i,1] <- unique(raw.sub$days.report)[i]
#   mat.delay.specimen.imputed[i,2:4] <- quantile(raw.sub$imputing.delay.specimen[which(raw.sub$days.report==unique(raw.sub$days.report)[i])],c(0.025,0.5,0.975),na.rm=T)
# }
# 
# par(mar=c(6,4,3,0))
# errbar(mat.delay.specimen[,1],mat.delay.specimen[,3],mat.delay.specimen[,4],mat.delay.specimen[,2],
#        xlab="",ylab="delay",xaxt = "n",ylim=c(0,30))
# title(main="delay for specimen collected (observed)")
# axis(1, at=seq(min(raw.sub$days.report,na.rm=T),max(raw.sub$days.report,na.rm=T),by=1),
#      labels=as.Date(min(raw.sub$days.report,na.rm=T):max(raw.sub$days.report,na.rm=T),origin="1970-01-01"),las=2)
# 
# errbar(mat.delay.specimen.imputed[,1],mat.delay.specimen.imputed[,3],mat.delay.specimen.imputed[,4],mat.delay.specimen.imputed[,2],
#        xlab="",ylab="delay",xaxt = "n",ylim=c(0,30))
# title(main="delay for specimen collected (imputed)")
# axis(1, at=seq(min(raw.sub$days.report,na.rm=T),max(raw.sub$days.report,na.rm=T),by=1),
#      labels=as.Date(min(raw.sub$days.report,na.rm=T):max(raw.sub$days.report,na.rm=T),origin="1970-01-01"),las=2)


#impute for cases without first speciemen collective dates but with confirmed date
delay.report.reg <- glm.nb(data=raw.sub, days.delay.report ~ days.report)
#summary(delay.report.reg)
#extract the intercept, coefficicent
intercept2 <- summary(delay.report.reg)$coefficients[1]
beta2 <- summary(delay.report.reg)$coefficients[2]
raw.sub$imputing.delay.report <- NA
for(i in only.confirmed.date){
  if (is.na(raw.sub$days.onset[i])){
    raw.sub$imputing.delay.report[i] <- rnbinom(n = 1, size = delay.report.reg$theta, mu = exp(intercept2+beta2*raw.sub$days.report[i]))
    raw.sub$imputing.onset.date[i] <- raw.sub$days.report[i] - raw.sub$imputing.delay.report[i]
  }
}

# par(mfrow=c(2,1))
# plot(raw.sub$days.report,raw.sub$days.delay.report,xlim=range(raw.sub$days.report,na.rm=T),ylim=c(0,100),main="Observed")
# plot(raw.sub$days.report,raw.sub$imputing.delay.report,xlim=range(raw.sub$days.report,na.rm=T),ylim=c(0,100),main="Imputed")
# 
# mat.delay.report <- matrix(NA,ncol=4,nrow=length(unique(raw.sub$days.report)))
# for (i in 1:length(unique(raw.sub$days.report))){
#   mat.delay.report[i,1] <- unique(raw.sub$days.report)[i]
#   mat.delay.report[i,2:4] <- quantile(raw.sub$days.delay.report[which(raw.sub$days.report==unique(raw.sub$days.report)[i])],c(0.025,0.5,0.975),na.rm=T)
# }
# 
# mat.delay.report.imputed <- matrix(NA,ncol=4,nrow=length(unique(raw.sub$days.report)))
# for (i in 1:length(unique(raw.sub$days.report))){
#   mat.delay.report.imputed[i,1] <- unique(raw.sub$days.report)[i]
#   mat.delay.report.imputed[i,2:4] <- quantile(raw.sub$imputing.delay.report[which(raw.sub$days.report==unique(raw.sub$days.report)[i])],c(0.025,0.5,0.975),na.rm=T)
# }
# 
# par(mar=c(6,4,3,0))
# errbar(mat.delay.report[,1],mat.delay.report[,3],mat.delay.report[,4],mat.delay.report[,2],
#        xlab="",ylab="delay",xaxt = "n",ylim=c(0,30))
# title(main="delay for report (observed)")
# axis(1, at=seq(min(raw.sub$days.report,na.rm=T),max(raw.sub$days.report,na.rm=T),by=1),
#      labels=as.Date(min(raw.sub$days.report,na.rm=T):max(raw.sub$days.report,na.rm=T),origin="1970-01-01"),las=2)
# 
# errbar(mat.delay.report.imputed[,1],mat.delay.report.imputed[,3],mat.delay.report.imputed[,4],mat.delay.report.imputed[,2],
#        xlab="",ylab="delay",xaxt = "n",ylim=c(0,30))
# title(main="delay for report (imputed)")
# axis(1, at=seq(min(raw.sub$days.report,na.rm=T),max(raw.sub$days.report,na.rm=T),by=1),
#      labels=as.Date(min(raw.sub$days.report,na.rm=T):max(raw.sub$days.report,na.rm=T),origin="1970-01-01"),las=2)

raw.sub$Q174656[which(raw.sub$Q174656=="")] <- as.character(format(as.Date(raw.sub$imputing.onset.date[which(raw.sub$Q174656=="")],origin="1970-01-01"), "%m/%d/%Y"))
out.raw.full <- merge(raw.full,raw.sub[,c("QARESPONSEID","Q174656")],by="QARESPONSEID",all.x = T)
out.raw.full$Q174656 <- out.raw.full$Q174656.x
out.raw.full$Q174656[which(out.raw.full$Q174656.x=="" & out.raw.full$Q174656.y!="")] <- out.raw.full$Q174656.y[which(out.raw.full$Q174656.x=="" & out.raw.full$Q174656.y!="")]
names(out.raw.full)[which(names(out.raw.full)=="Q174656.x")] <- "Q174656.original"
names(out.raw.full)[which(names(out.raw.full)=="Q174656.y")] <- "Q174656.afterImputation"


#View(out.raw.full[which(out.raw.full$Q174755=="Yes"),c("QARESPONSEID","Q174656","Q174631","Q175985","Q174656.original","Q174656.afterImputation")])
#out.raw.full <- read.csv("./data/pui_emory_extract_202005300845_imputed.csv",stringsAsFactors = F)
#write.csv(out.raw.full,file="./data/pui_emory_extract_202005300845_imputed.csv", row.names=FALSE)
write.csv(out.raw.full,file="./data/pui_emory_extract_202007130845_imputed.csv", row.names=FALSE)
