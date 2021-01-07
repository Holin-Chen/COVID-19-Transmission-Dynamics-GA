# Library
library(ggplot2)
library(hrbrthemes)
library(EnvStats)

load("./v6/SI.data.rda")

Metro.ATL <- c("FULTON", "DEKALB", "GWINNETT", "COBB", "CLAYTON")

gamma.dat <- data.frame(rbind(
  c("total",length(SI.data$t_interval),mean(SI.data$t_interval),var(SI.data$t_interval)),
  c("hosp+",length(SI.data$t_interval[SI.data$hospitalized_1=="Yes"]),mean(SI.data$t_interval[SI.data$hospitalized_1=="Yes"]),var(SI.data$t_interval[SI.data$hospitalized_1=="Yes"])),
  c("hosp-",length(SI.data$t_interval[SI.data$hospitalized_1=="No"]),mean(SI.data$t_interval[SI.data$hospitalized_1=="No"]),var(SI.data$t_interval[SI.data$hospitalized_1=="No"])),
  c("vent+",length(SI.data$t_interval[SI.data$ventilator_1=="Yes"]),mean(SI.data$t_interval[SI.data$ventilator_1=="Yes"]),var(SI.data$t_interval[SI.data$ventilator_1=="Yes"])),
  c("vent-",length(SI.data$t_interval[SI.data$ventilator_1=="No"]),mean(SI.data$t_interval[SI.data$ventilator_1=="No"]),var(SI.data$t_interval[SI.data$ventilator_1=="No"])),
  c("death+",length(SI.data$t_interval[SI.data$death_1=="Yes"]),mean(SI.data$t_interval[SI.data$death_1=="Yes"]),var(SI.data$t_interval[SI.data$death_1=="Yes"])),
  c("death-",length(SI.data$t_interval[SI.data$death_1=="No"]),mean(SI.data$t_interval[SI.data$death_1=="No"]),var(SI.data$t_interval[SI.data$death_1=="No"])),
  c("fever+",length(SI.data$t_interval[SI.data$fever_1=="Yes"]),mean(SI.data$t_interval[SI.data$fever_1=="Yes"]),var(SI.data$t_interval[SI.data$fever_1=="Yes"])),
  c("fever-",length(SI.data$t_interval[SI.data$fever_1=="No"]),mean(SI.data$t_interval[SI.data$fever_1=="No"]),var(SI.data$t_interval[SI.data$fever_1=="No"])),
  c("cough+",length(SI.data$t_interval[SI.data$cough_1=="Yes"]),mean(SI.data$t_interval[SI.data$cough_1=="Yes"]),var(SI.data$t_interval[SI.data$cough_1=="Yes"])),
  c("cough-",length(SI.data$t_interval[SI.data$cough_1=="No"]),mean(SI.data$t_interval[SI.data$cough_1=="No"]),var(SI.data$t_interval[SI.data$cough_1=="No"])),
  c("short_breath+",length(SI.data$t_interval[SI.data$short.breath_1=="Yes"]),mean(SI.data$t_interval[SI.data$short.breath_1=="Yes"]),var(SI.data$t_interval[SI.data$short.breath_1=="Yes"])),
  c("short_breath-",length(SI.data$t_interval[SI.data$short.breath_1=="No"]),mean(SI.data$t_interval[SI.data$short.breath_1=="No"]),var(SI.data$t_interval[SI.data$short.breath_1=="No"])),
  c("diarrhea+",length(SI.data$t_interval[SI.data$diarrhea_1=="Yes"]),mean(SI.data$t_interval[SI.data$diarrhea_1=="Yes"]),var(SI.data$t_interval[SI.data$diarrhea_1=="Yes"])),
  c("diarrhea-",length(SI.data$t_interval[SI.data$diarrhea_1=="No"]),mean(SI.data$t_interval[SI.data$diarrhea_1=="No"]),var(SI.data$t_interval[SI.data$diarrhea_1=="No"])),
  c("xray+",length(SI.data$t_interval[SI.data$abnormal.chest.xray_1=="Yes"]),mean(SI.data$t_interval[SI.data$abnormal.chest.xray_1=="Yes"]),var(SI.data$t_interval[SI.data$abnormal.chest.xray_1=="Yes"])),
  c("xray-",length(SI.data$t_interval[SI.data$abnormal.chest.xray_1=="No"]),mean(SI.data$t_interval[SI.data$abnormal.chest.xray_1=="No"]),var(SI.data$t_interval[SI.data$abnormal.chest.xray_1=="No"])),
  c("55+",length(SI.data$t_interval[!is.na(SI.data$age_1) & SI.data$age_1>55]),mean(SI.data$t_interval[!is.na(SI.data$age_1) & SI.data$age_1>55],na.rm=T),var(SI.data$t_interval[!is.na(SI.data$age_1) & SI.data$age_1>55])),
  c("55-",length(SI.data$t_interval[!is.na(SI.data$age_1) & SI.data$age_1<=55]),mean(SI.data$t_interval[!is.na(SI.data$age_1) & SI.data$age_1<=55],na.rm=T),var(SI.data$t_interval[!is.na(SI.data$age_1) & SI.data$age_1<=55])),
  c("male",length(SI.data$t_interval[SI.data$gender_1=="Male"]),mean(SI.data$t_interval[SI.data$gender_1=="Male"]),var(SI.data$t_interval[SI.data$gender_1=="Male"])),
  c("female",length(SI.data$t_interval[SI.data$gender_1=="Female"]),mean(SI.data$t_interval[SI.data$gender_1=="Female"]),var(SI.data$t_interval[SI.data$gender_1=="Female"])),
  c("black",length(SI.data$t_interval[!is.na(SI.data$race_1) & SI.data$race_1=="BLACK"]),mean(SI.data$t_interval[!is.na(SI.data$race_1) & SI.data$race_1=="BLACK"],na.rm=T),var(SI.data$t_interval[!is.na(SI.data$race_1) & SI.data$race_1=="BLACK"])),
  c("white",length(SI.data$t_interval[!is.na(SI.data$race_1) & SI.data$race_1=="WHITE"]),mean(SI.data$t_interval[!is.na(SI.data$race_1) & SI.data$race_1=="WHITE"],na.rm=T),var(SI.data$t_interval[!is.na(SI.data$race_1) & SI.data$race_1=="WHITE"])),
  c("metroATL",length(SI.data$t_interval[SI.data$county_1 %in% Metro.ATL]),mean(SI.data$t_interval[SI.data$county_1 %in% Metro.ATL]),var(SI.data$t_interval[SI.data$county_1 %in% Metro.ATL])),
  c("rural",length(SI.data$t_interval[!SI.data$county_1 %in% Metro.ATL]),mean(SI.data$t_interval[!SI.data$county_1 %in% Metro.ATL]),var(SI.data$t_interval[!SI.data$county_1 %in% Metro.ATL]))
))

names(gamma.dat) <-c("parent.group","n","mean","var")
gamma.dat$n <- as.numeric(as.character(gamma.dat$n))
gamma.dat$mean <- as.numeric(as.character(gamma.dat$mean))
gamma.dat$var <- as.numeric(as.character(gamma.dat$var))
gamma.dat$shape <- (gamma.dat$mean^2)/gamma.dat$var
gamma.dat$scale <- gamma.dat$var/gamma.dat$mean

write.csv(gamma.dat,file=paste0("./v4/output/param_table_gamma_",Sys.Date(),".csv"))

chisq.test(table(SI.data$hospitalized_1[which(SI.data$hospitalized_1 %in% c("Yes","No") & SI.data$hospitalized_2 %in% c("Yes","No"))],
                 SI.data$hospitalized_2[which(SI.data$hospitalized_1 %in% c("Yes","No") & SI.data$hospitalized_2 %in% c("Yes","No"))]))
#secondary case tend to have higher prob to be hospitalized given primary cases were hospitalized (<0.001);

fisher.test(table(SI.data$ventilator_1[which(SI.data$ventilator_1 %in% c("Yes","No") & SI.data$ventilator_2 %in% c("Yes","No"))],
                  SI.data$ventilator_2[which(SI.data$ventilator_1 %in% c("Yes","No") & SI.data$ventilator_2 %in% c("Yes","No"))]))

fisher.test(table(SI.data$death_1[which(SI.data$death_1 %in% c("Yes","No") & SI.data$death_2 %in% c("Yes","No"))],
                  SI.data$death_2[which(SI.data$death_1 %in% c("Yes","No") & SI.data$death_2 %in% c("Yes","No"))]))

chisq.test(table(SI.data$fever_1[which(SI.data$fever_1 %in% c("Yes","No") & SI.data$fever_2 %in% c("Yes","No"))],
                 SI.data$fever_2[which(SI.data$fever_1 %in% c("Yes","No") & SI.data$fever_2 %in% c("Yes","No"))]))
#secondary case tend to have higher prob to have fever given primary cases developed fever (p=0.004);

chisq.test(table(SI.data$cough_1[which(SI.data$cough_1 %in% c("Yes","No") & SI.data$cough_2 %in% c("Yes","No"))],
                 SI.data$cough_2[which(SI.data$cough_1 %in% c("Yes","No") & SI.data$cough_2 %in% c("Yes","No"))]))
#secondary case tend to have higher prob to have cough as symptom given primary cases developed cough as symptom (p=0.01);

chisq.test(table(SI.data$short.breath_1[which(SI.data$short.breath_1 %in% c("Yes","No") & SI.data$short.breath_2 %in% c("Yes","No"))],
                 SI.data$short.breath_2[which(SI.data$short.breath_1 %in% c("Yes","No") & SI.data$short.breath_2 %in% c("Yes","No"))]))
#secondary case tend to have higher prob to have short of breath as symptom given primary cases developed short of breath as symptom (p=0.01);

chisq.test(table(SI.data$diarrhea_1[which(SI.data$diarrhea_1 %in% c("Yes","No") & SI.data$diarrhea_2 %in% c("Yes","No"))],
                 SI.data$diarrhea_2[which(SI.data$diarrhea_1 %in% c("Yes","No") & SI.data$diarrhea_2 %in% c("Yes","No"))]))
#secondary case tend to have higher prob to have diarrhea as symptom given primary cases developed diarrhea as symptom (p=0.009);

chisq.test(table(SI.data$abnormal.chest.xray_1[which(SI.data$abnormal.chest.xray_1 %in% c("Yes","No") & SI.data$abnormal.chest.xray_2 %in% c("Yes","No"))],
                 SI.data$abnormal.chest.xray_2[which(SI.data$abnormal.chest.xray_1 %in% c("Yes","No") & SI.data$abnormal.chest.xray_2 %in% c("Yes","No"))]))
#secondary case tend to have higher prob to have abnormal xray given primary cases had abnormal xray (p<0.001);
par(mfrow=c(1,1),mar=c(4,4,2,1))
plot(SI.data$age_1,SI.data$age_2,xlab="primary case age",ylab="secondary case age")

# Dummy data
x <- 1:10*10-5
y <- 1:10*10-5
data <- expand.grid(X=x, Y=y)
data$Z <- NA
for (i in x){
  for (j in y){
    data$Z[which(data$X==i & data$Y==j)] <- length(which(SI.data$age_1 <= i+5 & SI.data$age_1 > i-5 & SI.data$age_2 <= j+5 & SI.data$age_2 > j-5))
  }
}
data$Z <- data$Z/sum(data$Z)*100

# Give extreme colors:
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile() +
  geom_text(aes(label = round(Z, 1)),size=8) +
  scale_fill_gradient(low="white", high="red") +
  labs(x="primary case age",y="secondary case age",fill = "Percent of\ntransmissions\n(N=2990)") +
  theme(
    legend.title = element_text(size=16),
    legend.text = element_text(size=14),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14))

#age shows transmission happen between people with similar age;

chisq.test(table(SI.data$gender_1[which(SI.data$gender_1 %in% c("Male","Female") & SI.data$gender_2 %in% c("Male","Female"))],
                 SI.data$gender_2[which(SI.data$gender_1 %in% c("Male","Female") & SI.data$gender_2 %in% c("Male","Female"))]))
#Male primary case tend to pass to Female? (p<0.001);

chisq.test(table(SI.data$race_1[which(SI.data$race_1 %in% c("BLACK","WHITE") & SI.data$race_2 %in% c("BLACK","WHITE"))],
                 SI.data$race_2[which(SI.data$race_1 %in% c("BLACK","WHITE") & SI.data$race_2 %in% c("BLACK","WHITE"))]))
#Pass to same race (p<0.001)

table(SI.data$county_1==SI.data$county_2)
