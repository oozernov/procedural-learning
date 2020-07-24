# 
# Ola Ozernov-Palchik 
#
# Updated-June 11 2020
#
# MIT ABCD Procedural Learning Study
#

#import packages 
Packages <- c("dplyr", "stats", "psych", "ggplot2", "lme4", 
              "gridExtra", "dplyr","caret","tidyverse", "ggplot2",
              "plyr","lmerTest","ggpubr","nlme"
)

lapply(Packages, library, character.only = TRUE)

####Organizational ####

#set subject directory 
#setwd("~/Dropbox (MIT)/learning_analysis")
setwd("~/Dropbox (MIT)/GitHub/procedural-learning")

#import pocedural.csv file
d <- read.csv('procedural_data072320.csv',stringsAsFactors = FALSE,skipNul = TRUE,
              blank.lines.skip = TRUE)
groups<-read.csv("abcd_group.csv")
names(groups)[names(groups)=="ID"] <- "PartID"
names(d)[names(d)=="abcd_id"] <- "PartID"
d<-merge(d,groups,'PartID')
SL=read.csv("SL_102818.csv")
#Rename variables and organize

#exclude ineligible participants
d<-d%>%filter(d$PartID!='ABCD_1718')
d<-d%>%filter(d$PartID!='ABCD_1728')

d<-d%>%filter(d$Subgroup=="TYP"| d$Subgroup=="DD")

#sample sizes
local({
  .Table <- xtabs(~Subgroup, data=d)
  cat("\nFrequency table:\n")
  print(.Table)
})

#create data frames for task
PL<-d[c(1,188,3:4,54:70,71:80)]


####Rotary Pursuit####

###Format###
rp <- NULL
rp <- data.frame('PartID'= character(),'Subgroup' = character(),'plType' = character(),'trial' = double(),
                      'On' = double(),'Off' = double(),stringsAsFactors=FALSE)

for (i in 1:nrow(PL)) {
  if (PL[i,10] == ""){ #if null go to next line
    next()
  }
  for(k in 6:21){
    on <- as.numeric(trimws(strsplit(PL[i,k],';')[[1]][1]))
    off <- as.numeric(trimws(strsplit(PL[i,k],';')[[1]][2]))
    Subgroup <- PL[i,2]
    if (nrow(rp) == 0) {
      rp[1,] <- c(PL[i,1],as.character(PL[i,2]),'rp',k-5,on,off)    }
    else {
      rp[nrow(rp) + 1,] <- c(PL[i,1],as.character(PL[i,2]),'rp',k-5,on,off)
    }
  }
}


rp2<-rp%>%dplyr::group_by(PartID,Subgroup,trial)%>%
                         mutate(prop_on=On/(On+Off))

rp2$Subgroup<-as.factor(rp2$Subgroup)
rp2$trial<-as.factor(rp2$trial)

###Analyze ###
m1<-lme(prop_on~Subgroup*trial,random=~1|PartID,
        data=rp2)
anova(m1)

lsmeans(m1, list(pairwise ~ Subgroup|trial), adjust = "tukey") #run only for gith


### Plot ###
rp$On <- as.numeric(rp$On) 
rp$Off <- as.numeric(rp$Off)
rp$trial <- as.numeric(rp$trial)
rpPlot<-dplyr::summarise(group_by(rp,Subgroup,trial), n = n(),
                         prop_on=mean(On/(On+Off)), sd=sd(On/(On+Off)),se = sd/sqrt(n))

ggplot(data = rpPlot, aes(x=trial, y=prop_on, color = Subgroup))+
  geom_line() +geom_point()  +
  geom_errorbar(aes(ymin=prop_on-se,ymax=prop_on+se),
                 width=.1,  size=0.5)+
  scale_x_continuous(breaks=seq(2,16,1))+
  #scale_y_continuous(limits = c(0.2, 0.5))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  + 
  labs(x = "Trials", y = "Proportion On (secs)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))


### Slopes ###
#On Slopes 
d_glm_fit_list <- vector(mode = "list", length = nrow(rp)*2)
index <- 0
abcd_ids=unique(rp$PartID)
b_list=c()
m_list = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  #for (a in c("d", "b")) 
  # index <- index + 1
  print(subj)
  d_subj <- filter(rp, PartID==subj)
  subj_model <- glm(On ~ trial, #complettion time(On/Off) ~trial
                    data = d_subj)
  b_list[subj] <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
  m_list[subj] <- as.numeric(subj_model$coefficients[2]) # coefficient slope
  
}

#Off Slopes 
d_glm_fit_list <- vector(mode = "list", length = nrow(rp)*2)
index <- 0

abcd_ids=unique(rp$PartID)
b_list2=c()
m_list2 = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  #for (a in c("d", "b")) 
  # index <- index + 1
  print(subj)
  d_subj <- filter(rp, PartID==subj)
  subj_model <- glm(Off ~ trial, 
                    data = d_subj)
  b_list2[subj] <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
  m_list2[subj] <- as.numeric(subj_model$coefficients[2]) # coefficient slope
  
}

#PropOn Slopes
rp$Prop_On <- as.numeric(rp$On)/as.numeric(rp$Off+rp$On)

d_glm_fit_list <- vector(mode = "list", length = nrow(rp)*2)
index <- 0

abcd_ids=unique(rp$PartID)
b_list4=c()
m_list4 = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  #for (a in c("d", "b")) 
  # index <- index + 1
  print(subj)
  d_subj <- filter(rp, PartID==subj)
  subj_model <- glm(Prop_On ~ trial, #complettion time(On/Off) ~trial
                    data = d_subj)
  b_list4[subj] <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
  m_list4[subj] <- as.numeric(subj_model$coefficients[2]) # coefficient slope
  
}

#Creat a new Data Frame containing the slopes
rp_data <- data.frame(PartID=abcd_ids,slopeOn = m_list, slopeOff = m_list2, slopeProp_On = m_list4)
rp_data$slopeProp_On_t<-as.numeric(abs(rp_data$slopeProp_On))
rp_data$slopeProp_On_t<-log10(rp_data$slopeProp_On+1) #transform slope

d2<-merge(d,rp_data)

m2<-lm(slopeProp_On_t~background_age+background_sex+kbit_ss+Subgroup, data=d2)
anova(m2)

###plot slope ###

plot_rp_slope<-d2%>%dplyr::select("Subgroup","slopeProp_On_t")

ggdensity(plot_rp_slope, x = "slopeProp_On_t",
          add = "mean", rug = TRUE,
          color = "Subgroup", fill = "Subgroup",
          palette = c("#00AFBB", "#E7B800"))


####Mirror Tracing####
#organize data
mt <- NULL
mt <- data.frame('PartID'= character(),'Subgroup' = character(),'Type' = character(),'trial' = double(),
                     'time' = double(),'error' = double(),stringsAsFactors=FALSE)


for (i in 1:nrow(PL)) {
  if (PL[i,22] == ""){ #if null go to next line
    next()
  }
  for(k in 22:31){
    print(paste(k,PL[i,k]))
    time <- as.numeric(trimws(strsplit(PL[i,k],';')[[1]][1]))
    error <- as.numeric(trimws(strsplit(PL[i,k],';')[[1]][2]))
    if (nrow(mt) == 0) {
      mt[1,] <- c(PL[i,1],PL[i,2],'mt',k-21,time,error)
    }
    else {
      mt[nrow(mt) + 1,] <- c(PL[i,1],PL[i,2],'mt',k-21,time,error)
    }
  }
}

mt$time <- as.numeric(mt$time) 
mt$error <- as.numeric(mt$error)
mt$trial <- as.factor(mt$trial)

mt2<-na.omit(mt)

###Analyze ###
m3<-nlme::lme(time~Subgroup*trial,random=~1|PartID,
        data=mt2)
anova(m3)

m4<-nlme::lme(error~Subgroup*trial,random=~1|PartID,
              data=mt2)
anova(m4)


### Plot ###
mt2$time <- as.numeric(mt2$time) 
mt2$error <- as.numeric(mt2$error)
mt2$trial <- as.numeric(mt2$trial)

mtTPlot<-dplyr::summarise(group_by(mt2,Subgroup,trial), n = n(),
                         meanT=mean(time), sd=sd(time),se = sd/sqrt(n))
mtEPlot<-dplyr::summarise(group_by(mt2,Subgroup,trial), n = n(),
                          meanE=mean(error), sd=sd(error),se = sd/sqrt(n))


ggplot(data = mtTPlot, aes(x=trial, y=meanT, color = Subgroup))+
  geom_line() +geom_point()  +
  geom_errorbar(aes(ymin=meanT-se,ymax=meanT+se),
                width=.1,  size=0.5)+
  scale_x_continuous(breaks=seq(0,10,1))+
  #scale_y_continuous(limits = c(0.2, 0.5))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  + 
  labs(x = "Trials", y = "Completion Time (secs)") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))

ggplot(data = mtEPlot, aes(x=trial, y=meanE, color = Subgroup))+
  geom_line() +geom_point()  +
  geom_errorbar(aes(ymin=meanE-se,ymax=meanE+se),
                width=.1,  size=0.5)+
  scale_x_continuous(breaks=seq(0,10,1))+
  #scale_y_continuous(limits = c(0.2, 0.5))+
  theme(
    axis.title = element_text(family = "Trebuchet MS", size = 20),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15))  + 
  labs(x = "Trials", y = "Errors") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
  theme(axis.line = element_line(arrow = arrow(angle = 15, length = unit(.15,"inches"),type = "closed")))



### Slopes ###

#slope based on error
#d_list <- vector(mode = "list", length = nrow(mt)*2)
#index <- 0

abcd_m_ids=unique(mt$PartID)
b_list5=c()
m_list5 = c()

for (subj in abcd_m_ids) { #d_export is my list of subject 
  # index <- index + 1
  print(subj)
  d_m_subj <- filter(mt2, PartID==subj)
  subj_m_model <- lm(error ~ trial, #complettion time(On/Off) ~trial
                      data = d_m_subj)
  b_list5[subj] <- as.numeric(subj_m_model$coefficients[1]) # coefficient intercept
  m_list5[subj] <- as.numeric(subj_m_model$coefficients[2]) # coefficient slope
}

#slope based on time
#d_list2 <- vector(mode = "list", length = nrow(mt)*2)
#index <- 0
b_list7=c()
m_list7 = c()

for (subj in abcd_m_ids) { #d_export is my list of subject 
  print(subj)
  d_m_subj <- filter(mt2, PartID==subj)
  subj_m_model <- glm(time ~ trial, #complettion time~trial
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

anova(lm(TSL_ACC~Sex+Age+Subgroup+, data=d2))


