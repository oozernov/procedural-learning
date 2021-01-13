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
              "plyr","lmerTest","ggpubr","nlme","emmeans"
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
SL=read.csv("sl_analysis/all_sl_wide.csv")
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

rp$On <- as.numeric(rp$On) 
rp$Off <- as.numeric(rp$Off)
rp$trial <- as.numeric(rp$trial)

rp2<-rp%>%dplyr::group_by(PartID,Subgroup,trial)%>%
                         mutate(prop_on=On/(On+Off))


### Outliers ###
# 
# boxplot(rp2$prop_on)$out
# outliers <- boxplot(rp2$prop_on, plot=FALSE)$out
# rp2<-rp2
# rp2<- rp2[-which(rp2$prop_on %in% outliers),] #excluded 8 outliers

###Analyze ###
rp2$Subgroup<-as.factor(rp2$Subgroup)
rp2$trial<-as.factor(rp2$trial)
m1<-lme(prop_on~Subgroup*trial,random=~1|PartID,
        data=rp2,na.action = 'na.omit')
anova(m1)

lsmeans(m1, list(pairwise ~ Subgroup|trial), adjust = "tukey") #run only for gith


### Plot ###
rp2$trial<-as.integer(rp2$trial)

rpPlot<-dplyr::summarise(group_by(rp2,Subgroup,trial), n = n(),
                         mean=mean(prop_on), sd=sd(On/(On+Off)),se = sd/sqrt(n))

ggplot(data = rpPlot, aes(x=trial, y=mean, color = Subgroup))+
  geom_line() +geom_point()  +
  scale_color_manual(values=c('red','blue'))+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
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
abcd_ids=unique(rp2$PartID)
b_list=c()
m_list = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  #for (a in c("d", "b")) 
  # index <- index + 1
  print(subj)
  d_subj <- filter(rp2, PartID==subj)
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
  d_subj <- filter(rp2, PartID==subj)
  subj_model <- glm(Off ~ trial, 
                    data = d_subj)
  b_list2[subj] <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
  m_list2[subj] <- as.numeric(subj_model$coefficients[2]) # coefficient slope
  
}

#PropOn Slopes

d_glm_fit_list <- vector(mode = "list", length = nrow(rp)*2)
index <- 0

abcd_ids=unique(rp$PartID)
b_list4=c()
m_list4 = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  #for (a in c("d", "b")) 
  # index <- index + 1
  print(subj)
  d_subj <- filter(rp2, PartID==subj)
  subj_model <- glm(prop_on ~ trial, #complettion time(On/Off) ~trial
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

plot_rp_slope<-d2%>%dplyr::select('PartID',"Subgroup","slopeProp_On_t")

ggdensity(plot_rp_slope, x = "slopeProp_On_t",
          add = "mean", rug = TRUE,
          color = "Subgroup", fill = "Subgroup",
          #palette = c("#00AFBB", "#E7B800"))
          )
          

rp_slope = plot_rp_slope %>%
  dplyr::group_by(PartID, Subgroup) %>%
  dplyr::summarise(mean = mean(slopeProp_On_t, na.rm = T))

color_easy = c("red", "blue")[rp_slope$Subgroup]
multi_2group.mean_diff.plot(custom_palette=my_color_palette);

multi.group3 <- 
  rp_slope %>%
  dabest(Subgroup,mean, 
         idx = list(c("TYP",'DD')),
         paired = FALSE
  )
plot(multi.group3, palette=c("blue","red"),rawplot.ylabel = "RP Prop On Slope")



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

### Outliers ###

#boxplot(mt$error)$out
#boxplot(mt$time)$out
#dev.off()
#outliers <- boxplot(mt$error, plot=FALSE)$out
#mt3<-mt2
#mt2<- mt2[-which(mt2$error %in% outliers),] #excluded 8 outliers

###Analyze ###

mt2<-na.omit(mt)
m3<-nlme::lme(time~Subgroup*trial,random=~1|PartID,
        data=mt2)
anova(m3) #trial is only one sig

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
  scale_color_manual(values=c('red','blue'))+
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
  scale_color_manual(values=c('red','blue'))+
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

d_glm_fit_list <- vector(mode = "list", length = nrow(rp)*2)
index <- 0
abcd_ids=unique(mt$PartID)
b_list5=c()
m_list5 = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  print(subj)
  d_subj <- filter(mt, PartID==subj)
  subj_model <- glm(error ~ trial, #errors
                    data = d_subj)
  b_list5[subj] <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
  m_list5[subj] <- as.numeric(subj_model$coefficients[2]) # coefficient slope
}


b_list7=c()
m_list7 = c()


d_glm_fit_list <- vector(mode = "list", length = nrow(rp)*2)
index <- 0
abcd_ids=unique(mt$PartID)
b_list7=c()
m_list7 = c()

for (subj in abcd_ids) { #d_export is my list of subject 
  print(subj)
  d_subj <- filter(mt, PartID==subj)
  subj_model <- glm(time ~ trial, #complettion time(On/Off) ~trial
                    data = d_subj)
  b_list7[subj] <- as.numeric(subj_model$coefficients[1]) # coefficient intercept
  m_list7[subj] <- as.numeric(subj_model$coefficients[2]) # coefficient slope
  
}

#new data frame containing the mirror slopes
mt_data <- cbind(PartID=abcd_ids,slope_me = m_list5, slope_mt = m_list7)
mt_data<-as.data.frame(mt_data)
mt_data$slope_me_t<-abs(as.numeric(mt_data$slope_me))
mt_data$slope_me_t<-log10(mt_data$slope_me_t+1) #transform slope

mt_data$slope_mt_t<-abs(as.numeric(mt_data$slope_mt))
mt_data$slope_mt_t<-log10(mt_data$slope_mt_t+1) #transform slope

d2<-merge(d2, mt_data)

#Analyze
m3<-lm(slope_mt_t~background_age+background_sex+kbit_ss+Subgroup, data=d2)
anova(m3)
m4<-lm(slope_me_t~background_age+background_sex+kbit_ss+Subgroup, data=d2)
anova(m4) #sig group difference

###plot slope ###

plot_me_slope<-d2%>%dplyr::select("PartID","Subgroup","slope_me_t")

ggdensity(plot_me_slope, x = "slope_me_t",
          add = "mean", rug = TRUE,
          color = "Subgroup", fill = "Subgroup",
          #palette = c("#00AFBB", "#E7B800"))
)

plot_mt_slope<-d2%>%dplyr::select("Subgroup","slope_mt_t")
ggdensity(plot_mt_slope, x = "slope_mt_t",
          add = "mean", rug = TRUE,
          color = "Subgroup", fill = "Subgroup",
          #palette = c("#00AFBB", "#E7B800"))
)



#plot slopes
mt_slope<-d2%>%dplyr::select("PartID","Subgroup","slopeProp_On_t")

ggdensity(plot_rp_slope, x = "slopeProp_On_t",
          add = "mean", rug = TRUE,
          color = "Subgroup", fill = "Subgroup",
          #palette = c("#00AFBB", "#E7B800"))
)


mt_slope = mt_slope %>%
  dplyr::group_by(PartID, Subgroup) %>%
  dplyr::summarise(mean = mean(slopeProp_On_t, na.rm = T))

multi.group <- 
 mt_slope %>%
  dabest(Subgroup,mean, 
         idx = list(c("TYP","DD")),
         paired = FALSE
  )
plot(multi.group, palette(c("red","blue")),rawplot.ylabel = "MT Time Slope")


me_slope = plot_me_slope %>%
  dplyr::group_by(PartID, Subgroup) %>%
  dplyr::summarise(mean = mean(slope_me_t, na.rm = T))

multi.group2 <- 
  me_slope %>%
  dabest(Subgroup,mean, 
         idx = list(c("TYP","DD")),
         paired = FALSE
  )
plot(multi.group2, color.column = Subgroup,rawplot.ylabel = "MT Errors Slope")


#### Statistical Learning ####
names(SL)[names(SL) == "ABCD.ID"] <- "PartID"
df<-merge(d2,SL,"PartID")

names(df)[names(df) == "slopeProp_On_t"] <- "RP_SLOPE"
names(df)[names(df) == "slope_me_t"] <- "MTE_SLOPE"
names(df)[names(df) == "slope_mt_t"] <- "MTT_SLOPE"
df_t<-df%>%filter(Subgroup=="TYP")
df_d<-df%>%filter(Subgroup=="DD")

df2<-df%>%select('wrmt_basic_ss_2','ppvt_vocab_ss_2','kbit_ss_2','wais_total_ss_2','kbit_ss_2',
                 'RP_SLOPE','MTE_SLOPE','MTT_SLOPE',
                   'VSL_RT_SLOPE','TSL_RT_SLOPE','VSL_ACC','TSL_ACC')
df2_d<-df_d%>%select('wrmt_basic_ss_2','ppvt_vocab_ss_2','kbit_ss_2','wais_total_ss_2','kbit_ss_2',
                 'RP_SLOPE','MTE_SLOPE','MTT_SLOPE',
                 'VSL_RT_SLOPE','TSL_RT_SLOPE','VSL_ACC','TSL_ACC')
df2_t<-df_t%>%select('wrmt_basic_ss_2','ppvt_vocab_ss_2','kbit_ss_2','wais_total_ss_2','kbit_ss_2',
                 'RP_SLOPE','MTE_SLOPE','MTT_SLOPE',
                 'VSL_RT_SLOPE','TSL_RT_SLOPE','VSL_ACC','TSL_ACC')

df2<-na.omit(df2)
res <- cor(df2,method = "pearson") #change here based on normality
melted_cormat <- reshape2::melt(res)
ggplot(data = melted_cormat, aes(Var1, Var2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_fixed()
#DD

df2_d<-na.omit(df2_d)
res_d <- cor(df2_d,method = "pearson") #change here based on normality
melted_cormat_d <- reshape2::melt(res_d)
d<-ggplot(data = melted_cormat_d, aes(Var1, Var2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
        )+
  coord_fixed()+ggtitle("Correlations in Dys")

#TYP


df2_t<-na.omit(df2_t)
res_t <- cor(df2_t,method = "pearson") #change here based on normality
melted_cormat_t <- reshape2::melt(res_t)
t<-ggplot(data = melted_cormat_t, aes(Var1, Var2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )+
  coord_fixed()+ggtitle("Correlations in Typ")

grid.arrange(d,t,ncol=2)
