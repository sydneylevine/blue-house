#########################################
#this script contains all the code that was used to produce analyses and graphs for
#revision 1 submitted to cognition (11/26/23)
#some exploratory analyses and alternate models/graphs were removed from this script for clarity
#see cognition R1.R for full script
######### #######

#clear environment
rm(list=ls())

#all packages
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ggpubr)
library(boot)
library(datasets)
library(ggrepel)
library(ggthemes)
library(lme4)
library(scales)
library(DescTools)
#this gives you non-scientific notation for all numbers
options(scipen=50) #reset with scipen = 0

setwd("~/Dropbox/Research Projects/Postdoc/Contractualism/Blue House Project/Cognition R1")

##################
###  STUDY 1 ####
#################

#pull in data
setwd("/Users/sydney/Dropbox/Research Projects/Postdoc/Contractualism/Blue House Project/Experiments/modeling experiments/property harms and benefits/Experiment--attempt 2/")
data_wide_bargain<-read.csv("acceptability+and+sidepayments_January+28%2C+2018_08.59.csv", head=T, sep=",", na.string=c(""), check.names=T, 
                            stringsAsFactors = F)
data_wide_willing<-read.csv("willing+judgments_January+28%2C+2018_19.20.csv", head=T, sep=",", na.string=c(""), check.names=T, 
                            stringsAsFactors = F)
#strings not read as factors (to avoid factorizing response ID and comments)
#check.names - checks whether col names are syntatically valid

#turn condition into factors
data_wide_bargain$condition<-factor(data_wide_bargain$condition)
data_wide_bargain$condition<-factor(data_wide_bargain$condition,levels=c("hundred","thousand","tenthousand","hunthousand","million","gun"))
levels(data_wide_bargain$condition)


#keep accept and sidepyament judgments together
unite_data_wide_bargain <- data_wide_bargain%>% 
  unite(bluemailbox, bluemailbox, bluemailbox2, sep = ";")%>% 
  unite(blueoutsidedoor, blueoutsidedoor, blueoutsidedoor2, sep = ";")%>% 
  unite(bluehouse, bluehouse, bluehouse2, sep = ";")%>% 
  unite(cuttree, cuttree, cuttree2, sep = ";")%>% 
  unite(breakwindows, breakwindows, breakwindows2, sep = ";")%>% 
  unite(razehouse, razehouse, razehouse2, sep = ";")%>% 
  unite(bleachlawn, bleachlawn, bleachlawn2, sep = ";")%>% 
  unite(blueinsidedoor, blueinsidedoor, blueinsidedoor2, sep = ";")%>% 
  unite(smearpoop, smearpoop, smearpoop2, sep = ";")%>%
  unite(erasemural,erasemural,erasemural2,sep=";")

#putting data in long form
colnames(unite_data_wide_bargain)
colnames(data_wide_willing)
unite_data_bargain<-unite_data_wide_bargain %>% gather(question,answer,6:15)
data_willing<-data_wide_willing %>% gather(question,answer,6:15)

bargain<-unite_data_bargain
willing<-data_willing

#making bargain and willing datasets have the same number of cols
willing<-willing %>%
  mutate(introspect_a="na",
         introspect_b="na",
         other_factors="na",
         condition="willing")
names(willing)
#reordering willing cols to align with bargain
willing<-willing[c(1:5,9:11,6,12,7:8)]

#separating sidepayments from acceptance judgments
bargain<-bargain %>% 
  separate(answer, c("accept", "payment"), sep = "\\;")

#write.csv(bargain,"blue house_individual subjects.csv", row.names=F)

#add another col to willing
willing<-willing %>%
  mutate(accept="na")
colnames(willing)[12]<-"payment"
willing<-willing[c(1:11,13,12)]

#combine willing and bargain datasets
data<-rbind(bargain,willing)
colnames(data)[11]<-"property"
data[c(12:13)]<-sapply(12:13, function(x) as.numeric(data[,x]))

#checking that all data is in data df
#data2<-data[c(5,10:12)]
#data2b<-data2[data2$condition!="willing",]
#data3<-data[c(5,10:11,13)]
#data2.w<-spread(data2,property,accept)
#data2.w<-data2.w[complete.cases(data2.w),]
#data3.w<-spread(data3,property,payment)
#data3.w<-data3.w[complete.cases(data3.w),]

#find mean acceptance for each property/offer pair
means<-data %>%
  filter(condition!="willing")%>%
  group_by(property,condition) %>%
  summarise(mean_accept=mean(accept))

#graph of acceptance data
means$property<-factor(means$property, levels=c("bluemailbox","blueoutsidedoor","blueinsidedoor",
                                                "cuttree","bleachlawn","bluehouse","breakwindows","smearpoop","razehouse","erasemural"))
##GRAPH USED IN PAPER
means %>%
  ggplot( 
    aes(property,mean_accept)) + 
  geom_bar(stat = "identity", aes(fill = condition), position = "dodge") + 
  ggthemes::theme_few()+
  #ggtitle("percent acceptance of offer\nall properties\nall offers") +
  labs(x = "Property", y="Percent Acceptance")+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


####plots of willing data

#summary statistics of willing data
#including 90 and 95% quantiles
means_willing<-data %>%
  filter(condition=="willing")%>%
  group_by(property) %>%
  summarise(mean=mean(payment),
            comp1=quantile(payment, probs = .01),
            comp10=quantile(payment, probs = .10),
            comp20=quantile(payment, probs = .20),
            comp30=quantile(payment, probs = .30),
            comp40=quantile(payment, probs = .40),
            comp50=quantile(payment, probs = .50),
            comp60=quantile(payment, probs = .60),
            comp70=quantile(payment, probs = .70),
            comp80=quantile(payment, probs = .80),
            comp90=quantile(payment, probs = .90),
            comp95=quantile(payment, probs = .95),
            comp99=quantile(payment, probs = .99)
  )

medians_willing<-data %>%
  filter(condition=="willing")%>%
  group_by(property) %>%
  summarise(median=median(payment))

#ordering properties by 95% cutoff value 
data$property<-factor(data$property, levels=c("bluemailbox","blueoutsidedoor","blueinsidedoor","cuttree","bleachlawn","bluehouse","breakwindows","smearpoop","razehouse","erasemural"))


# checking the willing data
check<-data[data$condition=="willing",]
check$log.payment<-log(check$payment)
check$flag<-1
check$flag[is.finite(check$log.payment)]<-0
check$flag2<-1
check$flag2[is.finite(check$payment)]<-0
sum(check$flag)
sum(check$flag2)
check2<-check[check$flag==1,]

#histograms of willing data
#GRAPH USED IN PAPER, print as 5x8
p4<-data %>%
  filter(condition=="willing") %>% 
  ggplot(aes(x = property, y = log(payment+1))) +
  geom_boxplot()+ 
  ggthemes::theme_few()+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4,20.7,23), 
                     labels = c("$100", "$1k","$10k", "$100k","$1M","$10M", "$100M","$1B","$10B"), limits=c(3,23))
p4

p5<-means_willing %>% 
  ggplot(aes(x = property, y = log(comp90))) + 
  geom_point(color="blue", size=2)+ 
  ggthemes::theme_few()+
  #ggrepel::geom_text_repel(aes(label=scales::dollar(comp90)), point.padding = 5, nudge_y = 10)+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4,20.7,23), 
                     labels = c("$100", "$1k","$10k", "$100k","$1M","$10M", "$100M","$1B","$10B"), limits=c(3,23))+
  theme_half_open() + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none")

p5

aligned_plots3<-align_plots(p4,p5, align="hv", axis="tblr")
a3<-ggdraw(aligned_plots3[[1]]) + draw_plot(aligned_plots3[[2]]) 
a3



#### sidepayment graphs ####

#it is hard to see the effect we are looking for because of the different ranges of the sidepayments
#calculate a new variable, which will normalize the sidepayments to the offer
#norm_ans=sidepayment/offer
data$offer[data$condition=="hundred"]<-100
data$offer[data$condition=="thousand"]<-1000
data$offer[data$condition=="tenthousand"]<-10000
data$offer[data$condition=="hunthousand"]<-100000
data$offer[data$condition=="million"]<-1000000
#flag cases where payment is greater than offer
data$flag<-0
data$flag[data$payment>data$offer]<-1
#remove all flagged cases
data<-data[data$flag==0,]
data$norm_ans<-data$payment/data$offer
data$property<-factor(data$property, levels=c("bluemailbox","blueoutsidedoor","blueinsidedoor",
                                              "cuttree","bleachlawn","bluehouse","breakwindows","smearpoop",
                                              "razehouse","erasemural"))


#graph normalized sidepayment distributions

#GRAPH IN PAPER , printed at 5x8
data %>%
  ggplot(aes(x=norm_ans,y=stat(density),color=property)) +
  xlim(0,1)+ 
  geom_density()+
  #geom_freqpoly(binwidth=.1)+
  ggthemes::theme_few()+
  ggtitle("all properties\nnormalized sidepayments") +
  labs(x = "sidepayment/offer")+
  theme(text = element_text(size = 20), legend.text = element_text(size = 18))



###########
# model comparisons
##########

#get comp demands into bargain2
bargain2<-bargain
#means_willing2<-means_willing[c(1:2)] #gets means
means_willing2<-means_willing[c(1,12)] #gets 90% interval "comp90"
names(bargain2)[names(bargain2) == "question"] <- "property"
bargain2<-merge(bargain2,means_willing2)
names(bargain2)[14] <- "compensation"
bargain2$accept<-as.factor(bargain2$accept)
bargain2<-bargain2[c(2,6,7,8,9,1,11,12,13,14)]
bargain2$condition_num[bargain2$condition=="hundred"]<-100
bargain2$condition_num[bargain2$condition=="thousand"]<-1000
bargain2$condition_num[bargain2$condition=="tenthousand"]<-10000
bargain2$condition_num[bargain2$condition=="hunthousand"]<-100000
bargain2$condition_num[bargain2$condition=="million"]<-1000000
bargain2$condition_num[bargain2$condition=="gun"]<-20000000

#get the compensation and acceptability means in one df
means3<-means
names(means3)[3]<-"acceptability"
names(means_willing2)[2]<-"compensation"
means3<-merge(means3,means_willing2)

#get numerical version of condition
means4<-means3
df3<-bargain2[c(7,11)]
means4<-merge(means4,df3)
#take out gun condition
means4<-means4[means4$condition!="gun",]
#calculate log odds of acceptability, logodds = log(prob/1-prob)
means4<-means4%>%
  mutate(lo_accept = log(acceptability/(1-acceptability)))
means4<-distinct(means4)
#get log versions of condition
means4$log_cond<-log(means4$condition_num)
means4$log_comp<-log(means4$compensation)
#mean center predictors
means4$log_cond2<-means4$log_cond - mean(means4$log_cond)
means4$log_comp2<-means4$log_comp - mean(means4$log_comp)


#get log of the predictors and mean center them for the regressions
bargain3<-bargain2[c(2,6,7,8,10,11)]
#removing gun condition
bargain3<-bargain3[bargain3$condition!="gun",]
#take log of the predictors
bargain3$log.condition<-log(bargain3$condition_num)
bargain3$log.compensation<-log(bargain3$compensation)
#mean center the predictors
bargain3$log.condition2<-bargain3$log.condition-mean(bargain3$log.condition)
bargain3$log.compensation2<-bargain3$log.compensation-mean(bargain3$log.compensation)


#linear models
model5<-lm(lo_accept ~ log_cond + log_comp, data=means4)

#MODEL USED IN PAPER
#run with 90th percentile of compensation demands
summary(model5)
#summary(model5) = int:-0.80636, log_cond:0.13563  , log_comp = -0.21213

#### model predictions #####
#get predictions (y values, acceptability) for all x values
############################


###### linear models ######
#y=a+b1X1+b2X2+b3X3 


#USING MODEL 5 IN PAPER
#model 5, log odds of acceptability is dv, without mean centering
means4$predictions <- coef(model5)[1] + (coef(model5)[2]*means4$log_cond) + (coef(model5)[3]*means4$log_comp)
plot(means4$predictions, means4$lo_accept)

#translate predictions back into probability space
means4$predictions.prob<-inv.logit(means4$predictions)
plot(means4$predictions.prob, means4$acceptability)


#nicer graphs of the data and predictions

#take the logistic function of the predicted y values.
#for any combination of x values, what is the predicted prob in log odds space (straight line)
#then apply the logistic function to those values and that gives in prob space
#label the x axis with the actual numbers, but use log space for that axis.

###### graph predictions against actual data #######

#now get predictions for lots of values, not just the x values that are in the data (to make continuous lines)
#create dataframe with values of whole range of log_comp
means7<-data.frame(log_comp = seq(5, 18, by=.1))
#create predictions for each of the log_comp values for each condition
condition.names<-as.vector(unique(means4$condition)) #vector of condition names
conditions<-as.vector(unique(means4$log_cond)) #vector of condition (offer) values, log transformed and mean centered
#loop creates model predictions for each condition
i=0  #index counts which condition we're on
for (val in conditions){
  predictions <- coef(model5)[1] + (coef(model5)[2]*val) + (coef(model5)[3]*means7$log_comp) #gets predictions for one condition
  predictions.prob <- inv.logit(predictions)
  means7[,ncol(means7)+1]<-predictions.prob #adds predictions to the last col.
  i<-i+1 #iterates index
  colnames(means7)[ncol(means7)]<-condition.names[i] #names the col based on the next name in the list
}

#put in long form
means7.l<-gather(means7,condition,prediction,2:6)
means7.l$condition<-factor(means7.l$condition, levels=c("hundred","thousand","tenthousand","hunthousand","million"))

#model data graphed
p3<-means7.l %>% 
  ggplot( 
    aes(x = log_comp, y = prediction,col=condition))+
  geom_point(shape=4, size=.5) + 
  theme_half_open() + 
  ylim(0,.6)+
  xlim(5,18)+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none")
p3


#Human data
#note that we are graphing the acceptability judgments at the non-mean centered log(compensation) values
p2<-means4 %>%
  ggplot( 
    aes(x = log_comp, y = acceptability,col=condition))+  
  geom_point() + 
  ggthemes::theme_few()+
  labs(x = "log_compensation", y="acceptability")+
  ylim(0,.6)+
  #xlim(5,25)+
  theme(legend.text = element_text(size = 30))+theme(text = element_text(size = 16))+
  #use this line when comp is the means
  #scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4,20.7,23), labels = c("$100", "$1k","$10k", "$100k","$1M","$10M", "$100M","$1B","$10B"), limits=c(4.6,25))+ 
  scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1), labels = c("$100", "$1k","$10k", "$100k","$1M","$10M"), limits=c(5,18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2

#put the graphs together

aligned_plots2<-align_plots(p2,p3, align="hv", axis="tblr")
a2<-ggdraw(aligned_plots2[[1]]) + draw_plot(aligned_plots2[[2]]) 
a2  #THIS IS THE CURRENT PLOT IN THE PAPER, PRINT 5X8


##########
## individual differences analysis
#########

indiv.l<-bargain
names(indiv.l)
indiv.l<-indiv.l[c(5,10,11,12)]
indiv.l<-indiv.l[indiv.l$condition!="willing",]
indiv.l<-indiv.l[indiv.l$condition!="gun",]
indiv<-spread(indiv.l,question,accept)
indiv$condition<-droplevels(indiv$condition)

indiv$group<-"others"

indiv$group[indiv$bleachlawn==0 &
              indiv$bluehouse==0 &
              indiv$blueinsidedoor==0 &
              indiv$bluemailbox==0 &
              indiv$blueoutsidedoor==0 & 
              indiv$breakwindows==0 &
              indiv$cuttree==0 &
              indiv$erasemural==0 &
              indiv$razehouse==0 &
              indiv$smearpoop==0]<-"rule.followers"

indiv$group<-as.factor(indiv$group)
summary(indiv$group)
#others rule.followers 
#123            182 

#how many rule.follwers in each offer (condition)?
indiv.table1<-prop.table(table(indiv$group,indiv$condition),margin=2) #margin=2 adds to 100 within a col
indiv.table2<-table(indiv$group,indiv$condition)


barplot(indiv.table1, main="rule-followers in each condition",
        xlab="Condition / Offer",
        legend = rownames(indiv.table1),
        col=c("red","blue","purple"),
        legend.text = TRUE, 
        args.legend = list(x = "topleft"))


#now include "always ok" as one of the mechanisms -- only a few people who ultimately use this strategy
indiv$group2<-"others"

indiv$group2[indiv$bleachlawn==0 &
               indiv$bluehouse==0 &
               indiv$blueinsidedoor==0 &
               indiv$bluemailbox==0 &
               indiv$blueoutsidedoor==0 & 
               indiv$breakwindows==0 &
               indiv$cuttree==0 &
               indiv$erasemural==0 &
               indiv$razehouse==0 &
               indiv$smearpoop==0]<-"rule.followers"

indiv$group2[indiv$bleachlawn==1 &
               indiv$bluehouse==1 &
               indiv$blueinsidedoor==1 &
               indiv$bluemailbox==1 &
               indiv$blueoutsidedoor==1 & 
               indiv$breakwindows==1 &
               indiv$cuttree==1 &
               indiv$erasemural==1 &
               indiv$razehouse==1 &
               indiv$smearpoop==1]<-"always.ok"

indiv$group2<-as.factor(indiv$group2)
summary(indiv$group2)
#always.ok         others   rule.followers 
#5                  118            182 

#how many rule.follwers in each offer (condition)?
indiv.table3<-prop.table(table(indiv$group2,indiv$condition),margin=2) #margin=2 adds to 100 within a col
indiv.table4<-table(indiv$group2,indiv$condition)


barplot(indiv.table1, main="rule-followers in each condition",
        xlab="Condition / Offer",
        legend = rownames(indiv.table1),
        col=c("red","blue","purple"),
        legend.text = TRUE, 
        args.legend = list(x = "topleft"))


#graph acceptability judgments for only those participants who were not rule-followers
indiv1<-indiv[c(1,13)]

bargain.subset<-bargain[c(5,10:12)]
bargain.subset<-merge(bargain.subset,indiv1)
#get only non rule-follwers
bargain.subset<-bargain.subset[bargain.subset$group!="rule.followers",]
bargain.subset$accept<-as.integer(bargain.subset$accept)

#graph non rule-follower data
means.subset<-bargain.subset %>%
  group_by(question,condition) %>%
  summarise(mean_accept=mean(accept, na.rm=T))

means.subset$question<-factor(means.subset$question, levels=c("bluemailbox","blueoutsidedoor","blueinsidedoor",
                                                              "cuttree","bleachlawn","bluehouse","breakwindows","smearpoop","razehouse","erasemural"))

means.subset %>%
  ggplot( 
    aes(question,mean_accept)) + 
  geom_bar(stat = "identity", aes(fill = condition), position = "dodge") + 
  ggthemes::theme_few()+
  #ggtitle("percent acceptance of offer\nall properties\nall offers") +
  labs(x = "Property", y="Percent Acceptance")+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#repeat the same analysis we did for the full dataset
means4a<-means4[c(1,2,7,8)] #relevant values for the model

#calculate log odds of acceptability, logodds = log(prob/1-prob)
means.subset<-means.subset%>%
  mutate(lo_accept = log(mean_accept/(1-mean_accept)))

names(means.subset)[1]<-"property"
means.subset<-merge(means.subset,means4a)

#model5 was used for the full dataset
#run with 90th percentile of compensation demands
model5.indiv<-lm(lo_accept ~ log_cond + log_comp, data=means.subset)

summary(model5.indiv) 
coef(model5.indiv)
#(Intercept)    log_cond    log_comp 
#2.60478652  0.07266016 -0.35860177 

means.subset$predictions <- coef(model5.indiv)[1] + (coef(model5.indiv)[2]*means.subset$log_cond) + (coef(model5.indiv)[3]*means.subset$log_comp)
plot(means.subset$predictions, means.subset$lo_accept)

#translate predictions back into probability space
means.subset$predictions.prob<-inv.logit(means.subset$predictions)
plot(means.subset$predictions.prob, means.subset$mean_accept)


##make nicer graphs of predictions against actual data (repeat of what we did for the full dataset)
###### graph predictions against actual data #######

#now get predictions for lots of values, not just the x values that are in the data (to make continuous lines)
#create dataframe with values of whole range of log_comp
means8<-data.frame(log_comp = seq(5, 18, by=.1))
#create predictions for each of the log_comp values for each condition
condition.names<-as.vector(unique(means.subset$condition)) #vector of condition names
conditions<-as.vector(unique(means.subset$log_cond)) #vector of condition (offer) values, log transformed and mean centered
#loop creates model predictions for each condition
i=0  #index counts which condition we're on
for (val in conditions){
  predictions <- coef(model5.indiv)[1] + (coef(model5.indiv)[2]*val) + (coef(model5.indiv)[3]*means8$log_comp) #gets predictions for one condition
  predictions.prob <- inv.logit(predictions)
  means8[,ncol(means8)+1]<-predictions.prob #adds predictions to the last col.
  i<-i+1 #iterates index
  colnames(means8)[ncol(means8)]<-condition.names[i] #names the col based on the next name in the list
}

#put in long form
means8.l<-gather(means8,condition,prediction,2:6)
means8.l$condition<-factor(means8.l$condition, levels=c("hundred","thousand","tenthousand","hunthousand","million"))

#model data graphed
p3a<-means8.l %>% 
  ggplot( 
    aes(x = log_comp, y = prediction,col=condition))+
  geom_point(shape=4, size=.5) + 
  theme_half_open() + 
  ylim(0,1)+
  xlim(5,18)+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none")
p3a

#Human data
#note that we are graphing the acceptability judgments at the non-mean centered log(compensation) values
p2a<-means.subset %>%
  ggplot( 
    aes(x = log_comp, y = mean_accept,col=condition))+  
  geom_point() + 
  ggthemes::theme_few()+
  labs(x = "log_compensation", y="acceptability")+
  ylim(0,1)+
  #xlim(5,25)+
  theme(legend.text = element_text(size = 30))+theme(text = element_text(size = 16))+
  #use this line when comp is the means
  #scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1, 18.4,20.7,23), labels = c("$100", "$1k","$10k", "$100k","$1M","$10M", "$100M","$1B","$10B"), limits=c(4.6,25))+ 
  scale_x_continuous(breaks = c(4.6, 6.9, 9.2, 11.5, 13.8, 16.1), labels = c("$100", "$1k","$10k", "$100k","$1M","$10M"), limits=c(5,18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2a

#put the graphs together

aligned_plots2a<-align_plots(p2a,p3a, align="hv", axis="tblr")
a2a<-ggdraw(aligned_plots2a[[1]]) + draw_plot(aligned_plots2a[[2]]) 
a2a  #THIS IS THE CURRENT PLOT IN THE PAPER, PRINT 5X8


###########
## sidepayment analysis
##############

# geometric means of non-50-50 side payments
geomean_summary <- data %>% 
  filter(condition != "gun", payment < offer) %>% 
  mutate(near_fifty = (payment >= .5*offer) & (payment <= .55*offer)) %>% 
  filter(!near_fifty) %>% 
  with_groups(
    c(property, offer),
    summarize, payment_geomean = exp(mean(log(payment + 1)))
  ) 

#add geomean of compensation demands to geomean_summary
geomean_comp<- data %>% 
  filter(condition == "willing") %>% 
  with_groups(
    c(property),
    summarize, comp_geomean = exp(mean(log(payment + 1)))
  ) 

geomean_plus_compensation<-merge(geomean_summary,geomean_comp)
names(geomean_plus_compensation)[4]<-"demand"

#compare col tells you if offer is greater than demand
geomean_plus_compensation$compare<-0
geomean_plus_compensation$compare[geomean_plus_compensation$offer>=geomean_plus_compensation$demand]<-1
geomean_plus_compensation$compare<-as.factor(geomean_plus_compensation$compare)


#GRAPH USED IN PAPER, PRINTED AS 3x10
### graphs w colors on separate rows and each property damage separate, printed as 3x10
#demand exceeds offer
geomean_plus_compensation %>% 
  filter(compare==0)%>%
  mutate(
    offer = factor(
      offer, 
      levels = unique(offer), 
      labels = str_c("Max Offer: ", scales::dollar(unique(offer)))
    )
  ) %>% 
  ggplot(aes(demand, payment_geomean)) +
  geom_point(color="black") +
  geom_abline(slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, size=1.5, color="#51AEF0")+
  facet_wrap(~offer, ncol = 4) +
  scale_x_log10(
    "Amount Required by Victim",
    limits = c(1, 5000000),
    breaks = c(1, 100, 10000, 1000000),
    labels = c("","$100","$10K","$1M")
  ) +
  scale_y_log10(
    "Amount Offered as Payment",
    limits = c(1, 5000000), 
    breaks = c(1, 100, 10000, 1000000),
    labels = c("","$100","$10K","$1M")
  )+ggthemes::theme_few()

#offer exceeds demand
geomean_plus_compensation %>% 
  filter(compare==1)%>%
  mutate(
    offer = factor(
      offer, 
      levels = unique(offer), 
      labels = str_c("Max Offer: ", scales::dollar(unique(offer)))
    )
  ) %>% 
  ggplot(aes(demand, payment_geomean)) +
  geom_point(color="black") +
  geom_abline(slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, size=1.5, color="#E98479")+
  facet_wrap(~offer, ncol = 4) +
  scale_x_log10(
    "Amount Required by Victim",
    limits = c(1, 5000000),
    breaks = c(1, 100, 10000, 1000000),
    labels = c("","$100","$10K","$1M")
  ) +
  scale_y_log10(
    "Amount Offered as Payment",
    limits = c(1, 5000000), 
    breaks = c(1, 100, 10000, 1000000),
    labels = c("","$100","$10K","$1M")
  )+ggthemes::theme_few()


#### REGRESSIONS #####

#mean-center the predictors
geomean_plus_compensation$offer2<-factor(geomean_plus_compensation$offer)
geomean_plus_compensation$context2<-factor(geomean_plus_compensation$property)
geomean_plus_compensation$demand_centered<-geomean_plus_compensation$demand - mean(geomean_plus_compensation$demand)
#mean-center the predictors, code compare as -.5, .5
geomean_plus_compensation$compare2[geomean_plus_compensation$compare==0]<- -.5
geomean_plus_compensation$compare2[geomean_plus_compensation$compare==1]<- .5

#this model has no random effects, used in paper
model.geomean9<-lm(payment_geomean ~ demand_centered * compare2, data=geomean_plus_compensation)



#################
### STUDY 2 ######
#################

setwd("~/Dropbox/Research Projects/Postdoc/Contractualism/Blue House Project")


##
#analysis of pink house case
####
pink.w<-read.csv("pinkhouse.csv") #n=209
#removing control questions (exclusions already marked)
pink.w<-pink.w[c(1:7,13:15)] 
#remove excluded subjects, n=163
pink.w<-pink.w[pink.w$exclude==0,]
pink.w<-pink.w[-c(4)]
length(pink.w$subject.code)

pink<-pink.w %>%
  gather(question.code,answer, -c(1:3), na.rm=TRUE)
pink$question.code<-factor(pink$question.code)

#character vectors to label the questions
question.list<-c(rep(c("moral","steve","carol"),2))
counterbalance.list<-c(rep("blue",3),rep("pink",3))

#adds labels to the long form data
pink<-pink %>%
  mutate(question=factor(question.list[question.code]),
         counterbalance=factor(counterbalance.list[question.code])
  )
pink<-pink[,-4]
pink.moral$answer<-factor(pink.moral$answer)

#summarize
pink.sum <- pink %>% 
  filter(question!="moral") %>%
  group_by(question) %>%
  summarize(
    n= length(answer),
    mean = mean(answer, na.rm = T),
    median = median(answer, na.rm=T),
    sd=sd(answer),
    se=(sd(answer))/sqrt(n),
    error = qnorm(0.975)*se, 
    CI.left = mean-error, 
    CI.right = mean+error
  )

#GRAPH IN PAPER, PRINTED AS 4X3
#bar graph 
pink.sum %>%
  ggplot(aes(y=mean, x=question)) + 
  geom_col(fill="#3F8D57", color="black")+
  geom_errorbar(aes(ymin=CI.left, ymax=CI.right), width=.2,position=position_dodge(0.05))+
  #coord_cartesian(ylim = c(0,5000))+
  #scale_y_continuous(trans='log10')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Side-payments")+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

t.test(pink$answer[pink$question=="steve"],pink$answer[pink$question=="carol"],alternative="greater")


#analaysis of moral judgments
pink.moral <- pink[pink$question=="moral",]

#new coding for Carol/Steve answers
pink.moral$steve<-NA
pink.moral$carol<-NA
pink.moral$steve[pink.moral$answer==1]<-1
pink.moral$carol[pink.moral$answer==2]<-1
pink.moral$steve[pink.moral$answer==3]<-1
pink.moral$carol[pink.moral$answer==3]<-1

sum(pink.moral$steve, na.rm=T) #86
sum(pink.moral$carol, na.rm=T) #17

binom.test(x = 86, n= 103, p=.5, alternative = "greater", conf.level = 0.95) #p-value = 1.403e-12

pink.moral.sum<-pink.moral %>%
  group_by(answer) %>%
  summarize(n = length(answer))

pink.moral.sum <- rbind(pink.moral.sum,c(2,0))
pink.moral.sum$proportion <-pink.moral.sum$n/sum(pink.moral.sum$n)


#GRAPH USED IN PAPER, PRINTED AS 4 X 5.5
pink.moral.sum %>%
  ggplot(aes(x=answer, y = proportion)) + 
  geom_bar(stat="identity", position="dodge", fill="#7B80F7", color="black")+
  #coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  xlab("Answer (moral)")+
  ylab("Proportion of subjects")+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


###########
#
# STUDY 3
#
###########




#####
# pilot for reasonable/unreasonable manipulation
#####

reasonable1.w<-read.csv("reasonable1.csv")
length(reasonable1.w$subjectcode) #n=120

#remove excluded subjects (failed control Qs; one was a chatGPT user)
reasonable1.w<-reasonable1.w[is.na(reasonable1.w$exclude),] #n=113
reasonable1.w<-reasonable1.w[2:19]

reasonable1<-reasonable1.w %>%
  gather(question.code,answer,-c(1:3),na.rm=TRUE) 
reasonable1$question.code = factor(reasonable1$question.code)
#character vectors to label the questions
condition.list<-c(rep("baseline",5),rep("reasonable",5),rep("unreasonable",5))
question.list<-c(rep(c("moral","sidepayment","explain","control1","control2"),3)) 
#adds the labels to the long-form reasonable1
reasonable1<-reasonable1 %>%
  mutate(
    condition=factor(condition.list[question.code]),
    question=factor(question.list[question.code])
  )
reasonable1<-reasonable1[-c(4)] #takes out question.code
reasonable1.full<-reasonable1 #reasonable.full has all data (including answers to control qs and explanations)

#get df with just moral judgments and sidepayments
reasonable1<-reasonable1[reasonable1$question=="moral" | reasonable1$question=="sidepayment",]
reasonable1$answer<-as.numeric(reasonable1$answer)

#recode values
reasonable1$answer[reasonable1$question=="moral" & reasonable1$answer==1]<-"split"
reasonable1$answer[reasonable1$question=="moral" & reasonable1$answer==4]<-"donate"
reasonable1$answer[reasonable1$question=="moral" & reasonable1$answer==5]<-"refuse"
reasonable1$answer[reasonable1$question=="moral" & reasonable1$answer==6]<-"keep"


#just moral judgments
reasonable1.m<-reasonable1[reasonable1$question=="moral",]
reasonable1.m$answer<-factor(reasonable1.m$answer)

reasonable.table<-prop.table(table(reasonable1.m$answer,reasonable1.m$condition),margin=2) #margin=2 adds to 100 within a col

chisq.test(table(reasonable1.m$answer,reasonable1.m$condition)) #X-squared = 15.926, df = 6, p-value = 0.01415

#graph by condition
barplot(reasonable.table, main="most morally acceptable?",
        xlab="Condition", col=c("red","blue","purple","pink"),
        legend = rownames(reasonable.table))

#graph by answer choice
reasonable1.df2<-as.data.frame(reasonable.table)
reasonable1.df2 %>%
  ggplot(aes(x=Var1, y = Freq, fill=Var2)) + 
  geom_bar(stat="identity", position="dodge")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  xlab("Condition")+
  ylab("Proportion of subjects")+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))




#sumarize sidepayment data
reasonable1.s<-reasonable1.full[reasonable1.full$question=="sidepayment",]
reasonable1.s$answer<-as.numeric(reasonable1.s$answer)


reasonable1.s.sum <- reasonable1.s %>% 
  group_by(condition) %>%
  summarize(
    mean = mean(answer, na.rm = T),
    median = median(answer, na.rm = T),
    n = length(answer)
  )

#graph
reasonable1.s.sum %>%
  ggplot( 
    aes(condition,mean)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggthemes::theme_few()+
  #ggtitle("percent acceptance of offer\nall properties\nall offers") +
  labs(x = "Condition", y="Sidepayment")+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,1000000)



### redo the analysis with just reasonable/unresaonable conditions (no baseline)

reasonable1.m2<-reasonable1.m[reasonable1.m$condition!="baseline",]
reasonable1.m2$condition<-droplevels(reasonable1.m2$condition)

#answer 2 divides answers into split and not-split
reasonable1.m2$answer2[reasonable1.m2$answer=="split"]<-"split"
reasonable1.m2$answer2[reasonable1.m2$answer!="split"]<-"not.split"
reasonable1.m2$answer2<-factor(reasonable1.m2$answer2)

#answer 3 divides answers into keep and not-keep
reasonable1.m2$answer3[reasonable1.m2$answer=="keep"]<-"keep"
reasonable1.m2$answer3[reasonable1.m2$answer!="keep"]<-"not.keep"
reasonable1.m2$answer3<-factor(reasonable1.m2$answer3)


reasonable1.table2<-prop.table(table(reasonable1.m2$answer,reasonable1.m2$condition),margin=2) #margin=2 adds to 100 within a col
reasonable1.table3<-prop.table(table(reasonable1.m2$answer2,reasonable1.m2$condition),margin=2) #margin=2 adds to 100 within a col

reasonable1.table4<-table(reasonable1.m2$answer2,reasonable1.m2$condition)
reasonable1.table5<-table(reasonable1.m2$answer,reasonable1.m2$condition)
reasonable1.table6<-table(reasonable1.m2$answer3,reasonable1.m2$condition)



chisq.test(table(reasonable1.m2$answer,reasonable1.m2$condition)) #X-squared = 12.247, df = 3, p-value = 0.006584
chisq.test(table(reasonable1.m2$answer2,reasonable1.m2$condition)) #X-squared = 10.592, df = 1, p-value = 0.001136
chisq.test(table(reasonable1.m2$answer3,reasonable1.m2$condition)) #X-squared = 1.2375, df = 1, p-value = 0.266


#graph by condition
barplot(reasonable1.table4, main="most morally acceptable?",
        xlab="Condition", col=c("red","blue","purple","pink"),
        legend = rownames(reasonable1.table4))

barplot(reasonable1.table5, main="most morally acceptable?",
        xlab="Condition", col=c("red","blue","purple","pink"),
        legend = rownames(reasonable1.table5))

barplot(reasonable1.table6, main="most morally acceptable?",
        xlab="Condition", col=c("red","blue","purple","pink"),
        legend = rownames(reasonable1.table6))



### power analysis for chi-square test for two condition experiment.

#power.chisq.test(n = NULL, w = NULL, df = NULL, sig.level = 0.05, power = NULL)

#n	total number of observations.
#w effect size.
#df	 degree of freedom (depends on the chosen test.
# sig.level	 Significance level (Type I error probability).
# power	Power of test (1 minus Type II error probability).

#Exactly one of the parameters w, n, power or sig.level must be passed as NULL, 
#and this parameter is determined from the others. 
#Note that the last one has non-NULL default, so NULL must be explicitly passed, if you want to compute it.
#For the chi-square test, the effect size index w is calculated by dividing the chi-square value 
#by the number of scores and taking the square root, 
#and it is considered small if w = 0.10, medium if w = 0.30, and large if w = 0.50. 



#split vs no-split
#X-squared = 10.592, df = 1 (from pilot data)

w = sqrt(10.6/74) #0.3784749
power.chisq.test(n = NULL, w = .38, df = 1, sig.level = 0.05, power = .95) #n = 89.99106

# four answer choices
#X-squared = 12.247, df = 3

w2 = sqrt(12.2/74) #0.4060355

power.chisq.test(n = NULL, w = .41, df = 3, sig.level = 0.05, power = .95) #n = 102.141



######
# pre-registered reasonable vs unreasonable study
# mturk participants, sept 5, 2023; n=150
#######
setwd("~/Dropbox/Research Projects/Postdoc/Contractualism/Blue House Project/Cognition R1")
reasonable2.w<-read.csv("reasonable2.csv")
length(reasonable2.w$subjectcode) #n=150

#remove excluded subjects (failed control Qs)
reasonable2.w<-reasonable2.w[is.na(reasonable2.w$exclude),] #n=145
reasonable2.w<-reasonable2.w[2:12]  #removes demographics for now

reasonable2<-reasonable2.w %>%
  gather(question.code,answer,-c(1:3),na.rm=TRUE) 
reasonable2$question.code = factor(reasonable2$question.code)
#character vectors to label the questions
condition.list<-c(rep("reasonable",4),rep("unreasonable",4))
question.list<-c(rep(c("moral","sidepayment","control1","control2"),2)) 
#adds the labels to the long-form reasonable2
reasonable2<-reasonable2 %>%
  mutate(
    condition=factor(condition.list[question.code]),
    question=factor(question.list[question.code])
  )
reasonable2<-reasonable2[-c(4)] #takes out question.code
reasonable2.full<-reasonable2 #reasonable.full has all data (including answers to control qs)

#get df with just moral judgments and sidepayments
reasonable2<-reasonable2[reasonable2$question=="moral" | reasonable2$question=="sidepayment",]
reasonable2$answer<-as.numeric(reasonable2$answer)

#recode values
reasonable2$answer[reasonable2$question=="moral" & reasonable2$answer==1]<-"split"
reasonable2$answer[reasonable2$question=="moral" & reasonable2$answer==4]<-"donate"
reasonable2$answer[reasonable2$question=="moral" & reasonable2$answer==5]<-"refuse"
reasonable2$answer[reasonable2$question=="moral" & reasonable2$answer==6]<-"keep"


#just moral judgments
reasonable2.m<-reasonable2[reasonable2$question=="moral",]
reasonable2.m$answer<-factor(reasonable2.m$answer)

reasonable2.table<-prop.table(table(reasonable2.m$answer,reasonable2.m$condition),margin=2) #margin=2 adds to 100 within a col

chisq.test(table(reasonable2.m$answer,reasonable2.m$condition)) #X-squared = 7.9981, df = 3, p-value = 0.04605


#graph by condition
barplot(reasonable2.table, main="most morally acceptable?",
        xlab="Condition", col=c("red","blue","purple","pink"),
        legend = rownames(reasonable2.table))

#graph by answer choice
reasonable2.df2<-as.data.frame(reasonable2.table)
reasonable2.df2 %>%
  ggplot(aes(x=Var1, y = Freq, fill=Var2)) + 
  geom_bar(stat="identity", position="dodge")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  xlab("Condition")+
  ylab("Proportion of subjects")+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



#answer 2 divides answers into split and not-split
reasonable2.m$answer2[reasonable2.m$answer=="split"]<-"split"
reasonable2.m$answer2[reasonable2.m$answer!="split"]<-"not.split"
reasonable2.m$answer2<-factor(reasonable2.m$answer2)

#answer 3 divides answers into keep and not-keep
reasonable2.m$answer3[reasonable2.m$answer=="keep"]<-"keep"
reasonable2.m$answer3[reasonable2.m$answer!="keep"]<-"not.keep"
reasonable2.m$answer3<-factor(reasonable2.m$answer3)


reasonable2.table2<-prop.table(table(reasonable2.m$answer,reasonable2.m$condition),margin=2) #margin=2 adds to 100 within a col
reasonable2.table3<-prop.table(table(reasonable2.m$answer2,reasonable2.m$condition),margin=2) #margin=2 adds to 100 within a col

reasonable2.table4<-table(reasonable2.m$answer2,reasonable2.m$condition)
reasonable2.table5<-table(reasonable2.m$answer,reasonable2.m$condition)
reasonable2.table6<-table(reasonable2.m$answer3,reasonable2.m$condition)

chisq.test(table(reasonable2.m$answer2,reasonable2.m$condition)) #X-squared = 3.6938, df = 1, p-value = 0.05461
chisq.test(table(reasonable2.m$answer3,reasonable2.m$condition)) #X-squared = 0.57181, df = 1, p-value = 0.4495

#graph by condition
barplot(reasonable2.table3, main="most morally acceptable?",
        xlab="Condition", col=c("red","blue","purple","pink"),
        legend = rownames(reasonable2.table))



#sumarize sidepayment data 
reasonable2.s<-reasonable2.full[reasonable2.full$question=="sidepayment",]
reasonable2.s$answer<-as.numeric(reasonable2.s$answer)


reasonable2.s.sum <- reasonable2.s %>% 
  group_by(condition) %>%
  summarize(
    mean = mean(answer, na.rm = T),
    median = median(answer, na.rm = T),
    n = length(answer)
  )

#graph
reasonable2.s.sum %>%
  ggplot( 
    aes(condition,mean)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggthemes::theme_few()+
  #ggtitle("percent acceptance of offer\nall properties\nall offers") +
  labs(x = "Condition", y="Sidepayment")+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(0,1000000)


## power analysis to re-run this experiment.
#power.chisq.test(n = NULL, w = NULL, df = NULL, sig.level = 0.05, power = NULL)

#n	total number of observations.
#w effect size.
#df	 degree of freedom (depends on the chosen test.
# sig.level	 Significance level (Type I error probability).
# power	Power of test (1 minus Type II error probability).

#Exactly one of the parameters w, n, power or sig.level must be passed as NULL, 
#and this parameter is determined from the others. 
#Note that the last one has non-NULL default, so NULL must be explicitly passed, if you want to compute it.
#For the chi-square test, the effect size index w is calculated by dividing the chi-square value 
#by the number of scores and taking the square root, 
#and it is considered small if w = 0.10, medium if w = 0.30, and large if w = 0.50. 



#split vs no-split
#X-squared = 3.6938, df = 1 (from first pre-reg study)

w = sqrt(3.7/145) #0.16
power.chisq.test(n = NULL, w = .16, df = 1, sig.level = 0.05, power = .98) #n = 629



##### demographic data
setwd("~/Dropbox/Research Projects/Postdoc/Contractualism/Blue House Project/Cognition R1")
reasonable2.d<-read.csv("reasonable2.csv")
length(reasonable2.d$subjectcode) #n=150

#remove excluded subjects (failed control Qs)
reasonable2.d<-reasonable2.d[is.na(reasonable2.d$exclude),] #n=145

mean(reasonable2.d$age) #41.49655
summary(as.factor(reasonable2.d$gender)) 
#female: 81
#male: 60
# non-binary: 3
# trans woman: 1
summary(as.factor(reasonable2.d$english.primary))
# english as primary: 98%
sum(reasonable2.d$race1,na.rm=T) #4
sum(reasonable2.d$race2,na.rm=T) #7
sum(reasonable2.d$race3,na.rm=T) #13
sum(reasonable2.d$race4,na.rm=T) #1
sum(reasonable2.d$race5,na.rm=T) #11
sum(reasonable2.d$race6,na.rm=T) #2
sum(reasonable2.d$race7,na.rm=T) #111
sum(reasonable2.d$race8,na.rm=T) #3
sum(reasonable2.d$race9,na.rm=T) #3
#total = 155 (not exclusive categories)

mean(reasonable2.d$politics) # 13.24828 - 10 = 3.2 on 1 to 5 scale (1 = extremely con)



######
# pre-registered (again!) reasonable vs unreasonable study
# mturk participants, sept 19, 2023; n=629
#######
reasonable3.w<-read.csv("reasonable3.csv")
length(reasonable3.w$subjectcode) #n=627

#remove excluded subjects (failed control Qs)
reasonable3.w<-reasonable3.w[is.na(reasonable3.w$exclude),] #n=582 (45 excluded)
reasonable3.w<-reasonable3.w[2:12]  #removes demographics for now

reasonable3<-reasonable3.w %>%
  gather(question.code,answer,-c(1:3),na.rm=TRUE) 
reasonable3$question.code = factor(reasonable3$question.code)
#character vectors to label the questions
condition.list<-c(rep("reasonable",4),rep("unreasonable",4))
question.list<-c(rep(c("moral","sidepayment","control1","control2"),2)) 
#adds the labels to the long-form reasonable3
reasonable3<-reasonable3 %>%
  mutate(
    condition=factor(condition.list[question.code]),
    question=factor(question.list[question.code])
  )
reasonable3<-reasonable3[-c(4)] #takes out question.code
reasonable3.full<-reasonable3 #reasonable.full has all data (including answers to control qs)

#get df with just moral judgments and sidepayments
reasonable3<-reasonable3[reasonable3$question=="moral" | reasonable3$question=="sidepayment",]
reasonable3$answer<-as.numeric(reasonable3$answer)

#recode values
reasonable3$answer[reasonable3$question=="moral" & reasonable3$answer==1]<-"split"
reasonable3$answer[reasonable3$question=="moral" & reasonable3$answer==4]<-"donate"
reasonable3$answer[reasonable3$question=="moral" & reasonable3$answer==5]<-"refuse"
reasonable3$answer[reasonable3$question=="moral" & reasonable3$answer==6]<-"keep"


#just moral judgments
reasonable3.m<-reasonable3[reasonable3$question=="moral",]
reasonable3.m$answer<-factor(reasonable3.m$answer)

reasonable3.table<-prop.table(table(reasonable3.m$answer,reasonable3.m$condition),margin=2) #margin=2 adds to 100 within a col

chisq.test(table(reasonable3.m$answer,reasonable3.m$condition)) #X-squared = 66.185, df = 3, p-value = 2.799e-14


#graph by condition
barplot(reasonable3.table, main="most morally acceptable?",
        xlab="Condition", col=c("red","blue","purple","pink"),
        legend = rownames(reasonable2.table))

#graph by answer choice
reasonable3.df2<-as.data.frame(reasonable2.table)
reasonable3.df2 %>%
  ggplot(aes(x=Var1, y = Freq, fill=Var2)) + 
  geom_bar(stat="identity", position="dodge")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  xlab("Condition")+
  ylab("Proportion of subjects")+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



#answer 2 divides answers into split and not-split
reasonable3.m$answer2[reasonable3.m$answer=="split"]<-"split"
reasonable3.m$answer2[reasonable3.m$answer!="split"]<-"not.split"
reasonable3.m$answer2<-factor(reasonable3.m$answer2)

#answer 3 divides answers into keep and not-keep
reasonable3.m$answer3[reasonable3.m$answer=="keep"]<-"keep"
reasonable3.m$answer3[reasonable3.m$answer!="keep"]<-"not.keep"
reasonable3.m$answer3<-factor(reasonable3.m$answer3)


reasonable3.table2<-prop.table(table(reasonable3.m$answer,reasonable3.m$condition),margin=2) #margin=2 adds to 100 within a col
reasonable3.table3<-prop.table(table(reasonable3.m$answer2,reasonable3.m$condition),margin=2) #margin=2 adds to 100 within a col

reasonable3.table4<-table(reasonable3.m$answer2,reasonable3.m$condition)
reasonable3.table5<-table(reasonable3.m$answer,reasonable3.m$condition)
reasonable3.table6<-table(reasonable3.m$answer3,reasonable3.m$condition)

chisq.test(table(reasonable3.m$answer2,reasonable3.m$condition)) #X-squared = 60.749, df = 1, p-value = 6.484e-15
chisq.test(table(reasonable3.m$answer3,reasonable3.m$condition)) #X-squared = 0.17711, df = 1, p-value = 0.6739


#graph by condition
barplot(reasonable3.table3, main="most morally acceptable?",
        xlab="Condition", col=c("red","blue","purple","pink"),
        legend = rownames(reasonable3.table3))



##### demographic data
setwd("~/Dropbox/Research Projects/Postdoc/Contractualism/Blue House Project/Cognition R1")
reasonable3.d<-read.csv("reasonable3.csv")
length(reasonable3.d$subjectcode) #n=627

#remove excluded subjects (failed control Qs)
reasonable3.d<-reasonable3.d[is.na(reasonable3.d$exclude),] #n=582

mean(reasonable3.d$age) #41.4055
summary(as.factor(reasonable3.d$gender)) 
#female/woman: 352
#male/man/xy: 224
# non-binary/agender/genderqueer: 4
# trans: 1
summary(as.factor(reasonable3.d$english.primary))
# english as primary: 99%
sum(reasonable3.d$race1,na.rm=T) #14
sum(reasonable3.d$race2,na.rm=T) #43
sum(reasonable3.d$race3,na.rm=T) #53
sum(reasonable3.d$race4,na.rm=T) #2
sum(reasonable3.d$race5,na.rm=T) #59
sum(reasonable3.d$race6,na.rm=T) #2
sum(reasonable3.d$race7,na.rm=T) #466
sum(reasonable3.d$race8,na.rm=T) #5
sum(reasonable3.d$race9,na.rm=T) #5
#(not exclusive categories)

mean(reasonable3.d$politics, na.rm=T) # 13.25345 - 10 = 3.3 on 1 to 5 scale (1 = extremely con)




########### now combine the two datasets and run the same anlaysis again ########

reasonable.combined<-rbind(reasonable2.m,reasonable3.m)
length(reasonable.combined$subjectcode) #n=727



reasonablec.table2<-prop.table(table(reasonable.combined$answer,reasonable.combined$condition),margin=2) #margin=2 adds to 100 within a col
reasonablec.table3<-prop.table(table(reasonable.combined$answer2,reasonable.combined$condition),margin=2) #margin=2 adds to 100 within a col

reasonablec.table4<-table(reasonable.combined$answer2,reasonable.combined$condition)
reasonablec.table5<-table(reasonable.combined$answer,reasonable.combined$condition)
reasonablec.table6<-table(reasonable.combined$answer3,reasonable.combined$condition)

chisq.test(table(reasonable.combined$answer,reasonable.combined$condition)) #X-squared = 63.925, df = 3, p-value = 8.516e-14
chisq.test(table(reasonable.combined$answer2,reasonable.combined$condition)) #X-squared = 62.505, df = 1, p-value = 2.658e-15
chisq.test(table(reasonable.combined$answer3,reasonable.combined$condition)) #X-squared = 0.89993, df = 1, p-value = 0.3428


### making graph for paper of the combined data

reasonable.df<-as.data.frame(reasonablec.table2)
names(reasonable.df)<-c("answer","condition","proportion")


#### GRAPH USED IN PAPER, printed 3x4
ggplot(data=reasonable.df, aes(x=condition, y=proportion, fill=answer)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c('#E98479','#A6A83C','#51AEF0','#DA79ED'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

