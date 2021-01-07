library(fmsb)
library(ggplot2)

load("C:\\Users\\holin\\Desktop\\Transmission Dynamics Team Work\\nCoV_GA\\nCoV_GA\\SI.data 2020-07-13 .rda")

#hospitalized
chisq.test(table(SI.data$hospitalized_1[which(SI.data$hospitalized_1 %in% c("Yes","No") & SI.data$hospitalized_2 %in% c("Yes","No"))],
                 SI.data$hospitalized_2[which(SI.data$hospitalized_1 %in% c("Yes","No") & SI.data$hospitalized_2 %in% c("Yes","No"))]))

hos <- table(SI.data$hospitalized_1[which(SI.data$hospitalized_1 %in% c("Yes","No") & SI.data$hospitalized_2 %in% c("Yes","No"))],
             SI.data$hospitalized_2[which(SI.data$hospitalized_1 %in% c("Yes","No") & SI.data$hospitalized_2 %in% c("Yes","No"))])
hospitalized <- oddsratio(213, 281, 512, 2879)
#YES
hos_yes <- paste0(hos[2,2],"/",(hos[2,1] + hos[2,2])," (",round(hos[2,2]*100/(hos[2,1] + hos[2,2]),digit=1),"%)")
#NO
hos_no <- paste0(hos[1,2],"/",(hos[1,1] + hos[1,2])," (",round(hos[1,2]*100/(hos[1,1] + hos[1,2]),digit=1),"%)")

#secondary case tend to have higher prob to be hospitalized given primary cases were hospitalized (<0.001);

#ventilator use
ventilator <- fisher.test(table(SI.data$ventilator_1[which(SI.data$ventilator_1 %in% c("Yes","No") & SI.data$ventilator_2 %in% c("Yes","No"))],
                  SI.data$ventilator_2[which(SI.data$ventilator_1 %in% c("Yes","No") & SI.data$ventilator_2 %in% c("Yes","No"))]))
vent <- table(SI.data$ventilator_1[which(SI.data$ventilator_1 %in% c("Yes","No") & SI.data$ventilator_2 %in% c("Yes","No"))],
              SI.data$ventilator_2[which(SI.data$ventilator_1 %in% c("Yes","No") & SI.data$ventilator_2 %in% c("Yes","No"))])
#YES
vent_yes <- paste0(vent[2,2],"/",(vent[2,1] + vent[2,2])," (",round(vent[2,2]*100/(vent[2,1] + vent[2,2]),digit=1),"%)")
#NO
vent_no <- paste0(vent[1,2],"/",(vent[1,1] + vent[1,2])," (",round(vent[1,2]*100/(vent[1,1] + vent[1,2]),digit=1),"%)")


#death
death <- fisher.test(table(SI.data$death_1[which(SI.data$death_1 %in% c("Yes","No") & SI.data$death_2 %in% c("Yes","No"))],
                  SI.data$death_2[which(SI.data$death_1 %in% c("Yes","No") & SI.data$death_2 %in% c("Yes","No"))]))
die <- table(SI.data$death_1[which(SI.data$death_1 %in% c("Yes","No") & SI.data$death_2 %in% c("Yes","No"))],
                   SI.data$death_2[which(SI.data$death_1 %in% c("Yes","No") & SI.data$death_2 %in% c("Yes","No"))])
#YES
die_yes <- paste0(die[2,2],"/",(die[2,1] + die[2,2])," (",round(die[2,2]*100/(die[2,1] + die[2,2]),digit=1),"%)")
#NO
die_no <- paste0(die[1,2],"/",(die[1,1] + die[1,2])," (",round(die[1,2]*100/(die[1,1] + die[1,2]),digit=1),"%)")


#fever
chisq.test(table(SI.data$fever_1[which(SI.data$fever_1 %in% c("Yes","No") & SI.data$fever_2 %in% c("Yes","No"))],
                 SI.data$fever_2[which(SI.data$fever_1 %in% c("Yes","No") & SI.data$fever_2 %in% c("Yes","No"))]))
fev <- table(SI.data$fever_1[which(SI.data$fever_1 %in% c("Yes","No") & SI.data$fever_2 %in% c("Yes","No"))],
                 SI.data$fever_2[which(SI.data$fever_1 %in% c("Yes","No") & SI.data$fever_2 %in% c("Yes","No"))])
fever <- oddsratio(935, 602, 981, 1020) 
#YES
fev_yes <- paste0(fev[2,2],"/",(fev[2,1] + fev[2,2])," (",round(fev[2,2]*100/(fev[2,1] + fev[2,2]),digit=1),"%)")
#NO
fev_no <- paste0(fev[1,2],"/",(fev[1,1] + fev[1,2])," (",round(fev[1,2]*100/(fev[1,1] + fev[1,2]),digit=1),"%)")
#secondary case tend to have higher prob to have fever given primary cases developed fever (p=0.004);

#cough
chisq.test(table(SI.data$cough_1[which(SI.data$cough_1 %in% c("Yes","No") & SI.data$cough_2 %in% c("Yes","No"))],
                 SI.data$cough_2[which(SI.data$cough_1 %in% c("Yes","No") & SI.data$cough_2 %in% c("Yes","No"))]))
cog <- table(SI.data$cough_1[which(SI.data$cough_1 %in% c("Yes","No") & SI.data$cough_2 %in% c("Yes","No"))],
             SI.data$cough_2[which(SI.data$cough_1 %in% c("Yes","No") & SI.data$cough_2 %in% c("Yes","No"))])
cough <- oddsratio(1589, 578, 935, 556)
#yes
cog_yes <- paste0(cog[2,2],"/",(cog[2,1] + cog[2,2])," (",round(cog[2,2]*100/(cog[2,1] + cog[2,2]),digit=1),"%)")
#NO
cog_no <- paste0(cog[1,2],"/",(cog[1,1] + cog[1,2])," (",round(cog[1,2]*100/(cog[1,1] + cog[1,2]),digit=1),"%)")
#secondary case tend to have higher prob to have cough as symptom given primary cases developed cough as symptom (p=0.01);


#short breath
chisq.test(table(SI.data$short.breath_1[which(SI.data$short.breath_1 %in% c("Yes","No") & SI.data$short.breath_2 %in% c("Yes","No"))],
                 SI.data$short.breath_2[which(SI.data$short.breath_1 %in% c("Yes","No") & SI.data$short.breath_2 %in% c("Yes","No"))]))
shobre <- table(SI.data$short.breath_1[which(SI.data$short.breath_1 %in% c("Yes","No") & SI.data$short.breath_2 %in% c("Yes","No"))],
                SI.data$short.breath_2[which(SI.data$short.breath_1 %in% c("Yes","No") & SI.data$short.breath_2 %in% c("Yes","No"))])
short.breath <- oddsratio(524, 533, 808, 1649) 
#yes
shobre_yes <- paste0(shobre[2,2],"/",(shobre[2,1] + shobre[2,2])," (",round(shobre[2,2]*100/(shobre[2,1] + shobre[2,2]),digit=1),"%)")
#NO
shobre_no <- paste0(shobre[1,2],"/",(shobre[1,1] + shobre[1,2])," (",round(shobre[1,2]*100/(shobre[1,1] + shobre[1,2]),digit=1),"%)")
#secondary case tend to have higher prob to have short of breath as symptom given primary cases developed short of breath as symptom (p=0.01);


#diarrhea
chisq.test(table(SI.data$diarrhea_1[which(SI.data$diarrhea_1 %in% c("Yes","No") & SI.data$diarrhea_2 %in% c("Yes","No"))],
                 SI.data$diarrhea_2[which(SI.data$diarrhea_1 %in% c("Yes","No") & SI.data$diarrhea_2 %in% c("Yes","No"))]))
diar <- table(SI.data$diarrhea_1[which(SI.data$diarrhea_1 %in% c("Yes","No") & SI.data$diarrhea_2 %in% c("Yes","No"))],
              SI.data$diarrhea_2[which(SI.data$diarrhea_1 %in% c("Yes","No") & SI.data$diarrhea_2 %in% c("Yes","No"))])
diarrhea <- oddsratio(360, 480, 636, 1899) 
#yes
diar_yes <- paste0(diar[2,2],"/",(diar[2,1] + diar[2,2])," (",round(diar[2,2]*100/(diar[2,1] + diar[2,2]),digit=1),"%)")
#NO
diar_no <- paste0(diar[1,2],"/",(diar[1,1] + diar[1,2])," (",round(diar[1,2]*100/(diar[1,1] + diar[1,2]),digit=1),"%)")
#secondary case tend to have higher prob to have diarrhea as symptom given primary cases developed diarrhea as symptom (p=0.009);


#abnormal chest xray
chisq.test(table(SI.data$abnormal.chest.xray_1[which(SI.data$abnormal.chest.xray_1 %in% c("Yes","No") & SI.data$abnormal.chest.xray_2 %in% c("Yes","No"))],
                 SI.data$abnormal.chest.xray_2[which(SI.data$abnormal.chest.xray_1 %in% c("Yes","No") & SI.data$abnormal.chest.xray_2 %in% c("Yes","No"))]))
abcx <- table(SI.data$abnormal.chest.xray_1[which(SI.data$abnormal.chest.xray_1 %in% c("Yes","No") & SI.data$abnormal.chest.xray_2 %in% c("Yes","No"))],
              SI.data$abnormal.chest.xray_2[which(SI.data$abnormal.chest.xray_1 %in% c("Yes","No") & SI.data$abnormal.chest.xray_2 %in% c("Yes","No"))])
abnormal.chest.xray <- oddsratio(64,107,235,2127)
#yes
abcx_yes <- paste0(abcx[2,2],"/",(abcx[2,1] + abcx[2,2])," (",round(abcx[2,2]*100/(abcx[2,1] + abcx[2,2]),digit=1),"%)")
#NO
abcx_no <- paste0(abcx[1,2],"/",(abcx[1,1] + abcx[1,2])," (",round(abcx[1,2]*100/(abcx[1,1] + abcx[1,2]),digit=1),"%)")


#gender: male is yes, female is no
chisq.test(table(SI.data$gender_1[which(SI.data$gender_1 %in% c("Male","Female") & SI.data$gender_2 %in% c("Male","Female"))],
                 SI.data$gender_2[which(SI.data$gender_1 %in% c("Male","Female") & SI.data$gender_2 %in% c("Male","Female"))]))
sex <- table(SI.data$gender_1[which(SI.data$gender_1 %in% c("Male","Female") & SI.data$gender_2 %in% c("Male","Female"))],
             SI.data$gender_2[which(SI.data$gender_1 %in% c("Male","Female") & SI.data$gender_2 %in% c("Male","Female"))])
gender <- oddsratio(618,1086,1227,1120)
#yes
sex_yes <- paste0(sex[2,2],"/",(sex[2,1] + sex[2,2])," (",round(sex[2,2]*100/(sex[2,1] + sex[2,2]),digit=1),"%)")
#NO
sex_no <- paste0(sex[1,2],"/",(sex[1,1] + sex[1,2])," (",round(sex[1,2]*100/(sex[1,1] + sex[1,2]),digit=1),"%)")
#Male primary case tend to pass to Female? (p<0.001);


#race: white is yes, black is no
chisq.test(table(SI.data$race_1[which(SI.data$race_1 %in% c("BLACK","WHITE") & SI.data$race_2 %in% c("BLACK","WHITE"))],
                 SI.data$race_2[which(SI.data$race_1 %in% c("BLACK","WHITE") & SI.data$race_2 %in% c("BLACK","WHITE"))]))
bw <- table(SI.data$race_1[which(SI.data$race_1 %in% c("BLACK","WHITE") & SI.data$race_2 %in% c("BLACK","WHITE"))],
            SI.data$race_2[which(SI.data$race_1 %in% c("BLACK","WHITE") & SI.data$race_2 %in% c("BLACK","WHITE"))])
race <- oddsratio(1598,96,80,1083)
#yes
bw_yes <- paste0(bw[2,2],"/",(bw[2,1] + bw[2,2])," (",round(bw[2,2]*100/(bw[2,1] + bw[2,2]),digit=1),"%)")
#NO
bw_no <- paste0(bw[1,2],"/",(bw[1,1] + bw[1,2])," (",round(bw[1,2]*100/(bw[1,1] + bw[1,2]),digit=1),"%)")



SI.data$age_group_1 <- ifelse(SI.data$age_1 <= 35, "young", 
                              ifelse(SI.data$age_1 <= 65, "middle", "old"))

SI.data$age_group_2 <- ifelse(SI.data$age_2 <= 35, "young", 
                              ifelse(SI.data$age_2 <= 65, "middle", "old"))

chisq.test(table(SI.data$age_group_1[which(SI.data$age_group_1 %in% c("young", "middle", "old") & SI.data$age_group_2 %in% c("young", "middle", "old"))],
                 SI.data$age_group_2[which(SI.data$age_group_1 %in% c("young", "middle", "old") & SI.data$age_group_2 %in% c("young", "middle", "old"))]))
chisq.test(table(SI.data$age_group_1[which(SI.data$age_group_1 %in% c("young", "middle") & SI.data$age_group_2 %in% c("young", "middle"))],
                 SI.data$age_group_2[which(SI.data$age_group_1 %in% c("young", "middle") & SI.data$age_group_2 %in% c("young", "middle"))]))
chisq.test(table(SI.data$age_group_1[which(SI.data$age_group_1 %in% c("young",  "old") & SI.data$age_group_2 %in% c("young",  "old"))],
                 SI.data$age_group_2[which(SI.data$age_group_1 %in% c("young",  "old") & SI.data$age_group_2 %in% c("young",  "old"))]))

age_young_vs_middle <- oddsratio(841,373,540,644) #P(Y2|Y1): 644/1017 (63.3%), P(M2|M1): 841/1381 (60.9%)
age_young_vs_old <- oddsratio(177,30,40,644) #P(Y2|Y1): 644/674 (95.5%), P(O2|O1): 177/217 (81.6%)

#age group
SI.data$age_group35_1 <- ifelse(SI.data$age_1 <= 35, "yes", "no")
SI.data$age_group35_2 <- ifelse(SI.data$age_2 <= 35, "yes", "no")
SI.data$age_group45_1 <- ifelse(SI.data$age_1 <= 45, "yes", "no")
SI.data$age_group45_2 <- ifelse(SI.data$age_2 <= 45, "yes", "no")
SI.data$age_group55_1 <- ifelse(SI.data$age_1 <= 55, "yes", "no")
SI.data$age_group55_2 <- ifelse(SI.data$age_2 <= 55, "yes", "no")
SI.data$age_group65_1 <- ifelse(SI.data$age_1 <= 65, "yes", "no")
SI.data$age_group65_2 <- ifelse(SI.data$age_2 <= 65, "yes", "no")


chisq.test(table(SI.data$age_group35_1[which(SI.data$age_group35_1 %in% c("yes", "no") & SI.data$age_group35_2 %in% c("yes", "no"))],
                 SI.data$age_group35_2[which(SI.data$age_group35_1 %in% c("yes", "no") & SI.data$age_group35_2 %in% c("yes", "no"))]))
age35 <- table(SI.data$age_group35_1[which(SI.data$age_group35_1 %in% c("yes", "no") & SI.data$age_group35_2 %in% c("yes", "no"))],
               SI.data$age_group35_2[which(SI.data$age_group35_1 %in% c("yes", "no") & SI.data$age_group35_2 %in% c("yes", "no"))])
age_35_compare <- oddsratio(963, 781, 586, 1733) 
#yes
age35_yes <- paste0(age35[2,2],"/",(age35[2,1] + age35[2,2])," (",round(age35[2,2]*100/(age35[2,1] + age35[2,2]),digit=1),"%)")
#NO
age35_no <- paste0(age35[1,2],"/",(age35[1,1] + age35[1,2])," (",round(age35[1,2]*100/(age35[1,1] + age35[1,2]),digit=1),"%)")



chisq.test(table(SI.data$age_group45_1[which(SI.data$age_group45_1 %in% c("yes", "no") & SI.data$age_group45_2 %in% c("yes", "no"))],
                 SI.data$age_group45_2[which(SI.data$age_group45_1 %in% c("yes", "no") & SI.data$age_group45_2 %in% c("yes", "no"))]))
age45 <- table(SI.data$age_group45_1[which(SI.data$age_group45_1 %in% c("yes", "no") & SI.data$age_group45_2 %in% c("yes", "no"))],
               SI.data$age_group45_2[which(SI.data$age_group45_1 %in% c("yes", "no") & SI.data$age_group45_2 %in% c("yes", "no"))])
age_45_compare <- oddsratio(1680,677,600,1106)
#yes
age45_yes <- paste0(age45[2,2],"/",(age45[2,1] + age45[2,2])," (",round(age45[2,2]*100/(age45[2,1] + age45[2,2]),digit=1),"%)")
#NO
age45_no <- paste0(age45[1,2],"/",(age45[1,1] + age45[1,2])," (",round(age45[1,2]*100/(age45[1,1] + age45[1,2]),digit=1),"%)")



chisq.test(table(SI.data$age_group55_1[which(SI.data$age_group55_1 %in% c("yes", "no") & SI.data$age_group55_2 %in% c("yes", "no"))],
                 SI.data$age_group55_2[which(SI.data$age_group55_1 %in% c("yes", "no") & SI.data$age_group55_2 %in% c("yes", "no"))]))
age55 <- table(SI.data$age_group55_1[which(SI.data$age_group55_1 %in% c("yes", "no") & SI.data$age_group55_2 %in% c("yes", "no"))],
               SI.data$age_group55_2[which(SI.data$age_group55_1 %in% c("yes", "no") & SI.data$age_group55_2 %in% c("yes", "no"))])
age_55_compare <- oddsratio(2592,440,451,580)
#yes
age55_yes <- paste0(age55[2,2],"/",(age55[2,1] + age55[2,2])," (",round(age55[2,2]*100/(age55[2,1] + age55[2,2]),digit=1),"%)")
#NO
age55_no <- paste0(age55[1,2],"/",(age55[1,1] + age55[1,2])," (",round(age55[1,2]*100/(age55[1,1] + age55[1,2]),digit=1),"%)")


chisq.test(table(SI.data$age_group65_1[which(SI.data$age_group65_1 %in% c("yes", "no") & SI.data$age_group65_2 %in% c("yes", "no"))],
                 SI.data$age_group65_2[which(SI.data$age_group65_1 %in% c("yes", "no") & SI.data$age_group65_2 %in% c("yes", "no"))]))
age65 <- table(SI.data$age_group65_1[which(SI.data$age_group65_1 %in% c("yes", "no") & SI.data$age_group65_2 %in% c("yes", "no"))],
               SI.data$age_group65_2[which(SI.data$age_group65_1 %in% c("yes", "no") & SI.data$age_group65_2 %in% c("yes", "no"))])
age_65_compare <- oddsratio(3311,254,299,199) 
#yes
age65_yes <- paste0(age65[2,2],"/",(age65[2,1] + age65[2,2])," (",round(age65[2,2]*100/(age65[2,1] + age65[2,2]),digit=1),"%)")
#NO
age65_no <- paste0(age65[1,2],"/",(age65[1,1] + age65[1,2])," (",round(age65[1,2]*100/(age65[1,1] + age65[1,2]),digit=1),"%)")


symptom <- c("hospitalized", "ventilator use", "death", "fever", "cough", "short breath", "diarrhea", "abnormal chest xray", "gender", "race",
             "age: <=35 vs >35", "age: <=45 vs >45", "age: <=55 vs >55", "age: <=65 vs >65")

conditional_prop <- c(hos_yes,hos_no,vent_yes,vent_no,die_yes,die_no,fev_yes,fev_no,cog_yes,cog_no,
                      shobre_yes,shobre_no,diar_yes,diar_no,abcx_yes,abcx_no,sex_yes,sex_no,bw_yes,bw_no,
                      age35_yes,age35_no,age45_yes,age45_no,age55_yes,age55_no,age65_yes,age65_no
)
symptom_2 <- c("hospitalized", NA,"ventilator use", NA, "death", NA, "fever", NA, "cough", NA, 
               "short breath", NA, "diarrhea", NA, "abnormal chest xray", NA, "gender", NA, "race", NA,
               "age: <=35 vs >35", NA,"age: <=45 vs >45",NA, "age: <=55 vs >55", NA,"age: <=65 vs >65",NA)
case_pair_conditional_prop <- data.frame(symptom_2, conditional_prop)


odds_ratio <- c(hospitalized$estimate, ventilator$estimate, death$estimate, fever$estimate,
                cough$estimate, short.breath$estimate, diarrhea$estimate, abnormal.chest.xray$estimate,
                gender$estimate, race$estimate, age_35_compare$estimate, age_45_compare$estimate,
                age_55_compare$estimate, age_65_compare$estimate)

lower_limit <- c(hospitalized$conf.int[1], ventilator$conf.int[1], death$conf.int[1], fever$conf.int[1],
                 cough$conf.int[1], short.breath$conf.int[1], diarrhea$conf.int[1], abnormal.chest.xray$conf.int[1],
                 gender$conf.int[1], race$conf.int[1], age_35_compare$conf.int[1], age_45_compare$conf.int[1],
                 age_55_compare$conf.int[1], age_65_compare$conf.int[1])

upper_limit <- c(hospitalized$conf.int[2], ventilator$conf.int[2], death$conf.int[2], fever$conf.int[2],
                 cough$conf.int[2], short.breath$conf.int[2], diarrhea$conf.int[2], abnormal.chest.xray$conf.int[2],
                 gender$conf.int[2], race$conf.int[2],  age_35_compare$conf.int[2], age_45_compare$conf.int[2],
                 age_55_compare$conf.int[2], age_65_compare$conf.int[2])

OR_95CI <- paste0(round(odds_ratio, digit=2), " ", "(",
                  round(lower_limit, digit=2), ",", " ",
                  round(upper_limit, digit=2), ")") 

sym_occur_prev_case1 <- c("619/2895 (21.4%)","88/2119 (4.2%)","126/2422 (5.2%)","1443/2593 (55.6%)",
                          "1905/2689 (70.8%)","1051/2585 (40.7%)","744/2458 (30.3%)","262/1868 (14.0%)") 
sym_occur_prev_case2 <- c("423/2895 (14.6%)", "71/2119 (3.4%)","75/2422 (3.1%)","1151/2593 (44.4%)",
                          "1624/2689 (60.4%)","815/2585 (31.5%)","607/2458 (24.7%)","147/1868 (7.9%)")

p_value <- c(hospitalized$p.value, ventilator$p.value, death$p.value, fever$p.value,
             cough$p.value, short.breath$p.value, diarrhea$p.value, abnormal.chest.xray$p.value,
             gender$p.value, race$p.value, age_35_compare$p.value, age_45_compare$p.value,
             age_55_compare$p.value, age_65_compare$p.value)

pair_symptom_correlation <- data.frame(symptom,  odds_ratio, 
                                       lower_limit, upper_limit, OR_95CI, p_value)
symptom_2 <- c(sapply(symptom , function(x) c(x, NA)))
OR_95CI2  <- c(sapply(OR_95CI , function(x) c(x, NA)))
p_value2 <- c(sapply(p_value , function(x) c(x, NA)))
yes_no <- rep(c("Yes","No"),14)

final_table <- data.frame(symptom_2, yes_no, conditional_prop,  OR_95CI2, p_value2)

write.csv(final_table, file="C:\\Users\\holin\\Desktop\\Transmission Dynamics Team Work\\nCoV_GA\\cases pair table2.csv",
          row.names = F)

p <- ggplot(pair_symptom_correlation, aes(x = odds_ratio, y = (symptom))) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = upper_limit, xmin = lower_limit), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  #scale_y_discrete (breaks = unique(symptom), trans="reverse") +
   scale_x_continuous(breaks = seq(-2,8,1) ) +
  #coord_trans(x = "log10") +
  ylab("Symptom") +
  xlab("Odds ratio")+
  labs(title="Association of symptom occurence between first cases and secondary cases")


#####
structure_forest_plot <- data.frame(odds_ratio, lower_limit, upper_limit)
tabletext <- cbind(
  c("Symptom", "hospitalized", "ventilator use", "death", "fever", "cough", "short breath", "diarrhea", "abnormal chest xray"),
  c("First case prev", sym_occur_prev_case1),
  c("Secondary case prev", sym_occur_prev_case2),
  c("Odds ratio (95% CI)", OR_95CI)
)

forestplot(tabletext,
           odds_ratio,
           lower_limit,
           upper_limit,
          
         )











