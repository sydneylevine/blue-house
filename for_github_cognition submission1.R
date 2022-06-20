#this script contains all the code that was used to produce analyses and graphs for
#submission to cognition (6/20/22)

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


#this gives you non-scientific notation for all numbers
options(scipen=50) #reset with scipen = 0


#################
#####  STUDY 1 ####
###################


####
#study 1 
#office/management cases
#free response study
#showing that sometimes, ppl break rules that will benefit the company
#####

data.w<-read.csv("office3.csv")
length(data.w$subjectcode) #n=50

length(data.w$subjectcode[data.w$company>0]) #13
length(data.w$subjectcode[data.w$you>0]) #33, 66%
length(data.w$subjectcode[data.w$other>0]) #17, 34%
length(data.w$subjectcode[data.w$agree>0]) #17

length(data.w$subjectcode[data.w$company>0 & data.w$you>0]) #9 you and company benefit

length(data.w$subjectcode[data.w$company==0 & data.w$you>0]) #2 company doesnt lose, you benefit

length(data.w$subjectcode[data.w$company>=0 & data.w$agree>=0]) #10

cor(data.w$company, data.w$agree, method="pearson") #0.2368859

#GRAPHS USED IN PAPER, print 3x3
data.w %>%
  ggplot(aes(x=company))+
  geom_histogram(binwidth = 1,colour='black',size=.5)+
  ggthemes::theme_few()

data.w %>%
  ggplot(aes(x=you))+
  geom_histogram(binwidth = 1,colour='black',size=.5)+
  ggthemes::theme_few()

data.w %>%
  ggplot(aes(x=other))+
  geom_histogram(binwidth = 1,colour='black',size=.5)+
  ggthemes::theme_few()

data.w %>%
  ggplot(aes(x=agree))+
  geom_histogram(binwidth = 1,colour='black',size=.5)+
  ggthemes::theme_few()

data.w %>%
  ggplot(aes(x=company))+
  geom_bar() +
  scale_x_binned(n.breaks=7)


sum(data.w$agree>0) #17 -> 34% think their manager would have said yes

cor(data.w$agree,data.w$company) #0.2368859

####
#study 1 (pilot)
#replication of above (but without the word "agree" in the manager question) -- prob dont include in paper
#office/management cases
#free response study
#showing that sometimes, ppl break rules that will benefit the company
#####

data.w<-read.csv("office1.csv")
length(data.w$subjectcode) #n=51

length(data.w$subjectcode[data.w$company>0]) #12
length(data.w$subjectcode[data.w$you>0]) #38
length(data.w$subjectcode[data.w$other>0]) #20
length(data.w$subjectcode[data.w$agree>0]) #13

length(data.w$subjectcode[data.w$company>0 & data.w$you>0]) #10 you and company benefit

length(data.w$subjectcode[data.w$company==0 & data.w$you>0]) #5 company doesnt lose, you benefit

length(data.w$subjectcode[data.w$company>=0 & data.w$agree>=0]) #5

cor(data.w$company, data.w$agree, method="pearson") #0.3078198

hist(data.w$company)
hist(data.w$you)
hist(data.w$other)
hist(data.w$agree)

sum(data.w$manager) #19
length(data.w$manager) #51 
#37% think their manager would have said yes

cor(data.w$agree,data.w$company)

ggscatter(data.w, x = "agree", y = "company", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "agree", ylab = "company")

ggscatter(data.w, x = "agree", y = "you", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "agree", ylab = "you")

ggscatter(data.w, x = "agree", y = "other", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "agree", ylab = "other")



#create data frame of benefit asssessments
measures <- c("Company", "Worker", "Someone Else", "Worker & Company" )
values <- c(12/51, 38/51, 20/51, 10/51)

df <- data.frame(measures, values)

df %>%
  ggplot(aes(y=values, x=measures)) + 
  geom_col()+
  coord_cartesian(ylim = c(0,1))+
  ggtitle("Who benefitted when you broke the rule?")

#to do: play around with "manager" variable


#################
####  STUDY 2 ####
#office/management cases
#vignette study
##################

data2.w<-read.csv("office2.csv")
length(data2.w$subjectcode) #n=50

data2<-data2.w %>%
  gather(question.code,answer,-c(1:3,64:70),na.rm=TRUE) 
data2$question.code = factor(data2$question.code)
#character vectors to label the questions
context.list<-c(rep("printing",5),rep("dentist",5),rep("supermarket",5),rep("it",5),rep("school",5),
                rep("bank",5),rep("pharmacy",5),rep("clinic",5),rep("truck",5),rep("office",5),
                rep("icecream",5),rep("construction",5))
question.list<-c(rep(c("ok","agree","worker","company","other"),12)) 
#adds the labels to the long-form data2
data2<-data2 %>%
  mutate(
    context=factor(context.list[question.code]),
    question=factor(question.list[question.code])
  )
data2<-data2[-c(11)] #takes out question.code

#data was coded weirdly in qualtrics
data2$answer[data2$question=="ok" & data2$answer==11]<-2
data2$answer[data2$question=="ok" & data2$answer==12]<-1
data2$answer[data2$question=="ok" & data2$answer==13]<-0
data2$answer[data2$question=="ok" & data2$answer==14]<--1
data2$answer[data2$question=="ok" & data2$answer==15]<--2

data2$answer[data2$question=="agree" & data2$answer==11]<-2
data2$answer[data2$question=="agree" & data2$answer==12]<-1
data2$answer[data2$question=="agree" & data2$answer==13]<-0
data2$answer[data2$question=="agree" & data2$answer==14]<--1
data2$answer[data2$question=="agree" & data2$answer==15]<--2

data2$answer[data2$question=="worker" & data2$answer==11]<-5
data2$answer[data2$question=="worker" & data2$answer==12]<-4
data2$answer[data2$question=="worker" & data2$answer==13]<-3
data2$answer[data2$question=="worker" & data2$answer==14]<-2
data2$answer[data2$question=="worker" & data2$answer==15]<-1

data2$answer[data2$question=="company" & data2$answer==11]<-5
data2$answer[data2$question=="company" & data2$answer==12]<-4
data2$answer[data2$question=="company" & data2$answer==13]<-3
data2$answer[data2$question=="company" & data2$answer==14]<-2
data2$answer[data2$question=="company" & data2$answer==15]<-1

data2$answer[data2$question=="other" & data2$answer==1]<-5
data2$answer[data2$question=="other" & data2$answer==2]<-4
data2$answer[data2$question=="other" & data2$answer==3]<-3
data2$answer[data2$question=="other" & data2$answer==4]<-2
data2$answer[data2$question=="other" & data2$answer==5]<-1


#summarize data
data2.sum <- data2 %>% 
  group_by(context,question) %>%
  summarize(
    mean = mean(answer, na.rm = T)
  )

office.means<-spread(data2.sum,question,mean)

#THIS IS THE GRAPH FOR THE PAPER, printed 4.5x4.5
office.means %>%
  ggplot(aes(x=agree, y = ok))+
  geom_point(size=2)+
  theme_few()+
  geom_smooth(method='lm', formula= y~x)+
  theme(axis.text=element_text(size=14))+
  ggrepel::geom_label_repel(aes(label=context),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')

#these correlations not currently in the paper
ggscatter(office.means, x = "agree", y = "ok", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Agree", ylab = "OK", label = "context")

ggscatter(means, x = "company", y = "agree", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Company", ylab = "Agree", label = "context")

ggscatter(means, x = "company", y = "ok", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Company", ylab = "OK", label = "context")

ggscatter(means, x = "worker", y = "ok", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Worker", ylab = "OK", label = "context")

ggscatter(means, x = "other", y = "ok", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Other", ylab = "OK", label = "context")

###
#regressions
##

#get means of other, company, self predictors in df with ok judgments
data2.m<-data2[data2$question=="ok",]
data2.m<-merge(data2.m,means)

#logistic mixed effects model with subject/context as random variable
library(lme4)
model1 <- lmer(answer~ agree + (1|subjectcode) + (1|context), data=data2.m) 
model2 <- lmer(answer~ (1|subjectcode) + (1|context), data=data2.m) 
model3 <- lmer(answer~ agree + company + (1|subjectcode) + (1|context), data=data2.m) 
model4 <- lmer(answer~ agree + other + (1|subjectcode) + (1|context), data=data2.m) 
model5 <- lmer(answer~ agree + worker + (1|subjectcode) + (1|context), data=data2.m) 
model6 <- lmer(answer~ agree + company + worker + other + (1|subjectcode) + (1|context), data=data2.m) 

anova(model1,model2) #ChiSq = 13.593  ; p= 0.000227 *** (sig improvement when agree is added to the model)
anova(model1,model3) #ChiSq = 0.4386  ; p= 0.5078 (no sig improvement when company is added to the model)
anova(model1,model4) #ch-sq = 2.5335  p= 0.1114
anova(model1,model5) #chi-sq = 0.2267  p= 0.634
anova(model1,model6)  #chi-sq = 5.4387  p=0.1424





######### #######
###  STUDY 3 ####
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

#add another col to willing
willing<-willing %>%
  mutate(accept="na")
colnames(willing)[12]<-"payment"
willing<-willing[c(1:11,13,12)]

#combine willing and bargain datasets
data<-rbind(bargain,willing)
colnames(data)[11]<-"property"
data[c(12:13)]<-sapply(12:13, function(x) as.numeric(data[,x]))

#find mean acceptance for each property/offer pair
means<-data %>%
  filter(condition!="willing")%>%
  group_by(property,condition) %>%
  summarise(mean_accept=mean(accept))

#graph of acceptance data
means$property<-factor(means$property, levels=c("bluemailbox","blueoutsidedoor","blueinsidedoor",
                                                "cuttree","bleachlawn","bluehouse","breakwindows","smearpoop","razehouse","erasemural"))
means %>%
  ggplot( 
    aes(property,mean_accept)) + 
  geom_bar(stat = "identity", aes(fill = condition), position = "dodge") + 
  ggthemes::theme_few()+
  #ggtitle("percent acceptance of offer\nall properties\nall offers") +
  labs(x = "Property", y="Percent Acceptance")+
  theme(axis.text=element_text(size=12))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plots of willing data
#ordering properties by 95% cutoff value (derived below)
data$property<-factor(data$property, levels=c("bluemailbox","blueoutsidedoor","blueinsidedoor","cuttree","bleachlawn","bluehouse","breakwindows","smearpoop","razehouse","erasemural"))

#histograms of willing data
#GRAPH USED IN PAPER, print as 5x8
p4<-data %>%
  filter(condition=="willing") %>% 
  ggplot(aes(x = property, y = log(payment))) +
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


#linear models, doesnt take into account repeated measures
model3<-lm(acceptability ~ log_cond2 + log_comp2, data=means4)
model4<-lm(lo_accept ~ log_cond2 + log_comp2, data=means4)
model5<-lm(lo_accept ~ log_cond + log_comp, data=means4)

#MODEL USED IN PAPER
#run with 90th percentile of compensation demands
#summary(model5) = int:-0.80636, log_cond:0.13563  , log_comp = -0.21213

#these all use means of compensation demands
#summary(model3): int =  0.17193352, log_cond2 = 0.02533725, log_comp2 = -0.01142946 
#summary(model4): int = -1.85332536, log_cond2 = 0.17471296  , log_comp2 = -0.08247786 
#summary(model5):  int = -2.47560502, log_cond =  0.17471296  , log_comp = -0.08247786  

#### model predictions #####
#get predictions (y values, acceptability) for all x values
############################


###### linear models ######
#y=a+b1X1+b2X2+b3X3 

#model 3, use coef(model3) to grab coefficients from the model
means4$predictions <- coef(model3)[1] + (coef(model3)[2]*means4$log_cond2) + (coef(model3)[3]*means4$log_comp2)
plot(means4$predictions, means4$acceptability)

#model 4, log odds of acceptability is dv
means4$predictions <- coef(model4)[1] + (coef(model4)[2]*means4$log_cond2) + (coef(model4)[3]*means4$log_comp2)
plot(means4$predictions, means4$lo_accept)

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
  geom_smooth(method = "lm", se = FALSE, size=1.5, color="blue")+
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
  geom_smooth(method = "lm", se = FALSE, size=1.5, color="red")+
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

#plotting all graphs on top of each other

#demand exceeds offer
p6<-geomean_plus_compensation %>% 
  filter(compare==0)%>%
  filter(offer==100)%>%
  ggplot(aes(demand, payment_geomean)) +
  geom_point(color="black") +
  #geom_abline(slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, size=1.5, color="blue")+
  theme_half_open() + 
  scale_x_log10(limits = c(1, 5000000)) +
  scale_y_log10(limits = c(1, 5000000))+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none")

p7<-geomean_plus_compensation %>% 
  filter(compare==0)%>%
  filter(offer==1000)%>%
  ggplot(aes(demand, payment_geomean)) +
  geom_point(color="black") +
  #geom_abline(slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, size=1.5, color="blue")+
  theme_half_open() + 
  scale_x_log10(limits = c(1, 5000000)) +
  scale_y_log10(limits = c(1, 5000000))+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none")

p8<-geomean_plus_compensation %>% 
  filter(compare==0)%>%
  filter(offer==10000)%>%
  ggplot(aes(demand, payment_geomean)) +
  geom_point(color="black") +
  #geom_abline(slope = 1, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, size=1.5, color="blue")+
  theme_half_open() + 
  scale_x_log10(limits = c(1, 5000000)) +
  scale_y_log10(limits = c(1, 5000000))+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none")

p9<-geomean_plus_compensation %>% 
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
  geom_smooth(method = "lm", se = FALSE, size=1.5, color="red")+
  #facet_wrap(~offer, ncol = 4) +
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


#put the graphs together

aligned_plots3<-align_plots(p9, p6,p7,p8, align="hv", axis="tblr")
a3<-ggdraw(aligned_plots3[[1]]) + draw_plot(aligned_plots3[[2]]) + draw_plot(aligned_plots3[[3]]) + draw_plot(aligned_plots3[[4]]) 
a3 

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
### STUDY 4 ######
#################


##
#analysis of pink house case
####
pink.w<-read.csv("pinkhouse.csv") #n=209
#removing control questions (exclusions already marked)
pink.w<-pink.w[c(1:7,13:15)] 
#remove excluded subjects, n=163
pink.w<-pink.w[pink.w$exclude==0,]
pink.w<-pink.w[-c(4)]

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

#bar graph 
pink.sum %>%
  ggplot(aes(y=mean, x=question)) + 
  geom_col()+
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


pink.moral.sum %>%
  ggplot(aes(x=answer, y = proportion)) + 
  geom_bar(stat="identity", position="dodge")+
  #coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  xlab("Answer (moral)")+
  ylab("Proportion of subjects")+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

