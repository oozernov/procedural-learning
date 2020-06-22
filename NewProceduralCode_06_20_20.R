# 
# Ola Ozernov-Palchik 
#
# Updated-June 11 2020
#
# MIT ABCD Procedural Learning Study
#

#import packages 
Packages <- c("dplyr", "stats", "nFactors", "psych", "ggplot2", "lme4", 
              "gridExtra", "dplyr","caret","tidyverse", "ggplot2","plyr")

lapply(Packages, library, character.only = TRUE)

#set subject directory 
setwd("~/Dropbox (MIT)/learning_analysis")
#import pocedural.csv file
d <- read.csv('procedural_data120419.csv',stringsAsFactors = FALSE,skipNul = TRUE,
              blank.lines.skip = TRUE)
SL=read.csv("~/Dropbox (MIT)/learning_analysis/SL/analysis_102818/SL_102818.csv")

#exclude ineligible participants
d<-d%>%filter(d$ABCD.ID!='ABCD_1718')
d<-d%>%filter(d$ABCD.ID!='ABCD_1728')
d<-d%>%filter(d$Subgroup=='Dyslexic' | d$Subgroup=='Typical')

#create data frames for task
dfABCD<-d[c(4:36)]
group<-d[c(4,5)]

#sample sizes
local({
  .Table <- xtabs(~Subgroup, data=d)
  cat("\nFrequency table:\n")
  print(.Table)
})

#rename variables
library(tidyverse)
names(d)[names(group) == "ABCD.ID"] <- "id"
names(group)[names(group) == "ABCD.ID"] <- "id"
names(group)[names(group) == "Subgroup"] <- "subgroup"

####Rotary Pursuit####
#format the file 
resABCD <- NULL
resABCD <- data.frame('id'= character(),'subgroup' = character(),'rpType' = character(),'trial' = double(),
                      'On' = double(),'Off' = double(),stringsAsFactors=FALSE)

for (i in 1:nrow(dfABCD)) {
  if (dfABCD[i,10] == ""){ #if null go to next line
    next()
  }
  #print (paste('Row # ',toString(i)))
  #resABCD[nrow(resABCD) + 1,] <- c(dfABCD[i,1],dfABCD[i,2],'rp0',0,NA,dfABCD[i,3])
  for(k in 10:19){
    #print(paste(k,dfABCD[i,k]))
    on <- as.numeric(trimws(strsplit(dfABCD[i,k],';')[[1]][1]))
    off <- as.numeric(trimws(strsplit(dfABCD[i,k],';')[[1]][2]))
    subgroup <- dfABCD[i,2]
    if (nrow(resABCD) == 0) {
      resABCD[1,] <- c(dfABCD[i,1],as.numeric(dfABCD[i,2]),'rp',k-9,on,off)    }
    else {
      resABCD[nrow(resABCD) + 1,] <- c(dfABCD[i,1],as.numeric(dfABCD[i,2]),'rp',k-9,on,off)
    }
  }
}

resABCD$subgroup<-NULL
resABCD<-merge(resABCD,group,"id")

resABCD$On <- as.numeric(resABCD$On) 
resABCD$Off <- as.numeric(resABCD$Off)
resABCD$trial <- as.numeric(resABCD$trial)

#calculate mean and sd for plotting
resABCD$subgroup<-as.factor(resABCD$subgroup)
#resABCD$PropOn<-(resABCD$On/(resABCD$On+resABCD$Off))

res4Plot<-dplyr::summarise(group_by(resABCD,subgroup,trial), n = n(),
          prop_on=mean(On/(On+Off)), sd=sd(On/(On+Off)),se = sd/sqrt(n))

#plot prop on accross trials
ggplot(data = res4Plot, aes(x=trial, y=prop_on, color = subgroup))+
  geom_line() +geom_point()  +
  geom_errorbar(aes(ymin=prop_on-se,ymax=prop_on+se),
                 width=.1,  size=0.5)+
  scale_x_continuous(limits = c(2, 10))+
  #scale_y_continuous(limits = c(0.2, 0.5))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  + 
  #labs(title = " Threshold Accuracy", x = "Trials", y = "Proportion On (secs)") +
  labs(title = " Threshold Accuracy", x = "Trials", y = "Proportion On (secs)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))


#Extract slope 
#On Slopes 
d_glm_fit_list <- vector(mode = "list", length = nrow(resABCD)*2)
index <- 0
abcd_ids=unique(resABCD$id)
b_list=c()
m_list = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  #for (a in c("d", "b")) 
  # index <- index + 1
  print(subj)
  d_subj <- filter(resABCD, id==subj)
  subj_model <- glm(On ~ trial, #complettion time(On/Off) ~trial
                    data = d_subj)
  b_list[subj] <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
  m_list[subj] <- as.numeric(subj_model$coefficients[2]) # coefficient slope
  
}

#Off Slopes 
d_glm_fit_list <- vector(mode = "list", length = nrow(resABCD)*2)
index <- 0

abcd_ids=unique(resABCD$id)
b_list2=c()
m_list2 = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  #for (a in c("d", "b")) 
  # index <- index + 1
  print(subj)
  d_subj <- filter(resABCD, id==subj)
  subj_model <- glm(Off ~ trial, 
                    data = d_subj)
  b_list2[subj] <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
  m_list2[subj] <- as.numeric(subj_model$coefficients[2]) # coefficient slope
  
}

#PropOn Slopes
resABCD$Prop_On <- as.numeric(resABCD$On)/as.numeric(resABCD$Off+resABCD$On)

d_glm_fit_list <- vector(mode = "list", length = nrow(resABCD)*2)
index <- 0

abcd_ids=unique(resABCD$id)
b_list4=c()
m_list4 = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  #for (a in c("d", "b")) 
  # index <- index + 1
  print(subj)
  d_subj <- filter(resABCD, id==subj)
  subj_model <- glm(Prop_On ~ trial, #complettion time(On/Off) ~trial
                    data = d_subj)
  b_list4[subj] <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
  m_list4[subj] <- as.numeric(subj_model$coefficients[2]) # coefficient slope
  
}

#Creat a new Data Frame containing the slopes
rp_data <- data.frame(slopeOn = m_list, slopeOff = m_list2, slopeProp_On = m_list4)
rp_data_new<-cbind(group,rp_data)
names(rp_data_new)[names(rp_data_new) == "id"] <- "PartID"

t.test(rp_data_new$slopeProp_On~rp_data_new$subgroup)

####Mirror Tracing####
#organize data
mtABCD <- NULL
mtABCD <- data.frame('id'= character(),'subgroup' = character(),'Type' = character(),'trial' = double(),
                     'time' = double(),'error' = double(),stringsAsFactors=FALSE)


for (i in 1:nrow(dfABCD)) {
  if (dfABCD[i,23] == ""){ #if null go to next line
    next()
  }
  #print (paste('Row # ',toString(i)))
  #print(dfABCD[i,])
  for(k in 23:31){
    print(paste(k,dfABCD[i,k]))
    time <- as.numeric(trimws(strsplit(dfABCD[i,k],';')[[1]][1]))
    error <- as.numeric(trimws(strsplit(dfABCD[i,k],';')[[1]][2]))
    if (nrow(mtABCD) == 0) {
      mtABCD[1,] <- c(dfABCD[i,1],dfABCD[i,2],'mt',k-22,time,error)
    }
    else {
      mtABCD[nrow(mtABCD) + 1,] <- c(dfABCD[i,1],dfABCD[i,2],'mt',k-22,time,error)
    }
  }
}

mtABCD$time <- as.numeric(mtABCD$time) 
mtABCD$error <- as.numeric(mtABCD$error)
mtABCD$trial <- as.factor(mtABCD$trial)

mtABCD2<-na.omit(mtABCD)

summary(lm(error~subgroup*trial,data=mtABCD))
summary(lm(time~subgroup*trial,data=mtABCD))

#how many in each group
mtABCD2$subgroup<-as.factor(mtABCD2$subgroup)
mtABCD2$trial<-as.factor(mtABCD2$trial)

mt4Plot<-dplyr::summarise(group_by(mtABCD2,trial,subgroup), n = n(),
                          sd=sd(time), time=mean(time), se = sd/sqrt(n))

mt4Plot_error<-dplyr::summarise(group_by(mtABCD2,trial,subgroup), n = n(),
                          sd=sd(error), error=mean(error), se = sd/sqrt(n))


mt4Plot$subgroup<-as.factor(mt4Plot$subgroup)
mt4Plot$trial<-as.numeric(mt4Plot$trial)

#plot time on accross trials
ggplot(data = mt4Plot, aes(x=trial, y=time, color = subgroup))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=time-se,ymax=time+se),
                width=.1,  size=0.5)+
  scale_x_continuous(limits = c(2, 10))+
  scale_y_continuous(limits = c(10, 60))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  + 
  labs(title = " Mirror Tracing", x = "Trials", y = "Time") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),
                                               type = "closed")))

mt4Plot_error$trial<-as.numeric(mt4Plot_error$trial)

#plot error across time
ggplot(data = mt4Plot_error, aes(x=trial, y=error, color = subgroup))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=error-se,ymax=error+se),
                width=.1,  size=0.5)+
  scale_x_continuous(limits = c(2, 10))+
  scale_y_continuous(limits = c(5, 45))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  + 
  labs(title = " Mirror Tracing", x = "Trials", y = "Errors") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),
                                               type = "closed")))


#getting the sloper for Mirror Tracing

#slope based on error
d_list <- vector(mode = "list", length = nrow(mtABCD)*2)
index <- 0

abcd_m_ids=unique(mtABCD$id)
b_list5=c()
m_list5 = c()

for (subj in abcd_m_ids) { #d_export is my list of subject 
  #for (a in c("d", "b")) 
  # index <- index + 1
  print(subj)
  d_m_subj <- filter(mtABCD, id==subj)
  subj_m_model <- glm(error ~ trial, #complettion time(On/Off) ~trial
                      data = d_m_subj)
  b_list5[subj] <- as.numeric(subj_m_model$coefficients[1]) # coefficient intercept
  m_list5[subj] <- as.numeric(subj_m_model$coefficients[2]) # coefficient slope
}

#slope based on time
d_list2 <- vector(mode = "list", length = nrow(mtABCD)*2)
index <- 0

abcd_m_ids=unique(mtABCD$id)
b_list7=c()
m_list7 = c()

for (subj in abcd_m_ids) { #d_export is my list of subject 
  print(subj)
  d_m_subj <- filter(mtABCD, id==subj)
  subj_m_model <- glm(time ~ trial, #complettion time(On/Off) ~trial
                      data = d_m_subj)
  b_list7[subj] <- as.numeric(subj_m_model$coefficients[1]) # coefficient intercept
  m_list7[subj] <- as.numeric(subj_m_model$coefficients[2]) # coefficient slope
  
}

#new data frame containing the mirror slopes
slope_m_data <- data.frame(PartID=abcd_m_ids,slope_me = m_list5, slope_mt = m_list7)


#### Statistical Learning ####
names(SL)[names(SL) == "PartID"] <- "ABCD.ID"
d2<-merge(d,SL)
anova(lm(VSL_ACC~Sex+Age+Subgroup, data=d2))
anova(lm(TSL_ACC~Sex+Age+Subgroup, data=d2))



